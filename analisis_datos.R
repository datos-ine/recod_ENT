### Mortalidad por enfermedades no transmisibles y causas externas en
### Argentina (2010–2023): redistribución de defunciones por causas garbage
### Análisis de datos
### Autora: Tamara Ricardo
### Revisor: Juan I. Irassar

# Cargar paquetes --------------------------------------------------------
# remotes::install_github("https://github.com/datos-ine/joinpointR")

pacman::p_load(
  patchwork,
  ggridges,
  scico,
  # Tablas
  flextable,
  gtsummary,
  # Tasas estandarizadas
  PHEindicatormethods,
  # Regresión joinpoint
  # segmented,
  joinpointR,
  # Manejo de datos
  scales,
  rio,
  janitor,
  tidyverse
  # ,
  # update = TRUE
)


# Cargar/preparar datos --------------------------------------------------
## Población estándar Argentina (2022) -----
pob_est_2022 <- import("clean/arg_pob_est_2022.rds")

## Proyecciones poblacionales Argentina (2010-2023) -----
proy_2010_2023 <- import("clean/arg_proy_mensual_2010_2023.rds") |>
  # Seleccionar primer mes del año
  filter(mes == 1)


## Defunciones por grupo de causas -----
recod_defun <- import("clean/arg_defun_mes_2010-2023_recod.rds")


# Análisis exploratorio --------------------------------------------------
## Frecuencia defunciones por sexo
recod_defun |>
  tabyl(sexo) |>
  adorn_pct_formatting()

## Frecuencia defunciones por grupo etario
recod_defun |>
  tabyl(grupo_edad) |>
  adorn_pct_formatting()

## Códigos garbage más frecuentes
recod_defun |>
  filter(grupo_n2 %in% c("GC1", "GC2")) |>
  count(cie10_cod) |>
  mutate(pct = percent(n / sum(n), accuracy = .1)) |>
  arrange(-n) |>
  head(n = 15)

## Frecuencia defunciones por grupo causa nivel 1
recod_defun |>
  tabyl(grupo_edad, grupo_n1) |>
  adorn_totals(where = "row") |>
  adorn_percentages() |>
  adorn_pct_formatting()

# Diferencias por edad
chisq.test(recod_defun$grupo_edad, recod_defun$grupo_n1)


## Tabla 1 ---------------------------------------------------------------
theme_gtsummary_language(
  language = "es",
  decimal.mark = ",",
  big.mark = "."
)

### Frecuencia defunciones por grupo etario y grupo de causas
tab1 <- recod_defun |>
  # Seleccionar columnas
  select(grupo_edad, grupo_causa = grupo_n2) |>
  # Reagrupar causas GC
  mutate(
    grupo_causa = case_when(
      grupo_causa %in% c("GC1", "GC2") ~ "GC1-GC2",
      grupo_causa %in% c("GC3", "GC4") ~ "GC3-GC4",
      .default = grupo_causa
    ) |>

      # Ordenar
      fct_relevel(
        "Homicidio",
        "Suicidio",
        "Tránsito",
        "Otras CE",
        "GC1-GC2",
        "GC3-GC4",
        after = Inf
      )
  ) |>

  # Tabla 2x2
  tbl_summary(
    by = grupo_edad,
    include = grupo_causa,
    digits = list(grupo_causa ~ c(0, 1)),
    label = list(grupo_causa = "")
  ) |>
  add_p() |>

  # Opciones tabla
  bold_labels() |>
  modify_header(
    label = "**Grupo causa**",
    p.value = "**P**",
    all_stat_cols() ~ "{level} ({style_percent(p, digits = 1)}%)"
  ) |>
  modify_spanning_header(all_stat_cols() ~ "**Grupo etario**") |>
  remove_footnote_header(columns = all_stat_cols()) |>
  modify_indent(columns = label, indent = 0L)


# Evolución temporal tasas GC (total país) -------------------------------
## Tasas estandarizadas por año ------------------------------------------
datos_jp <- recod_defun |>
  # Seleccionar muertes por GC
  filter(str_detect(grupo_n1, "GC")) |>
  droplevels() |>

  # Agrupar datos por año
  count(anio, grupo_edad, grupo_n2) |>

  # Unir con proyecciones poblacionales
  left_join(
    proy_2010_2023 |>
      # Agrupar por año
      count(anio, grupo_edad, wt = proy_pob, name = "pob")
  ) |>

  # Añadir población estándar 2022
  left_join(pob_est_2022) |>

  # Calcular tasa estandarizada
  group_by(anio, grupo_n2) |>
  calculate_dsr(
    x = n,
    n = pob,
    stdpop = pob_est_2022,
    type = "standard"
  )


## Regresión joinpoint ---------------------------------------------------
mod <- model_jp(
  data = datos_jp,
  value = "value",
  time = "anio",
  group = "grupo_n2",
  k = 3
)

## Tabla
summary_jp(mod, ft = TRUE, var1 = "Nivel")

## APCs
get_apc(mod = mod$GC1)
get_apc(mod = mod$GC2)
get_apc(mod = mod$GC4)

## AAPCs
get_aapc(mod = mod$GC1, show_ci = TRUE)
get_aapc(mod = mod$GC2, show_ci = TRUE)
get_aapc(mod = mod$GC3, show_ci = TRUE)
get_aapc(mod = mod$GC4, show_ci = TRUE)


## Figura 2 --------------------------------------------------------------
### Ridgeplots ----
g1 <- datos_jp |>
  ggplot(aes(x = value, y = grupo_n2, fill = grupo_n2)) +
  # Geometrías
  geom_density_ridges(
    jittered_points = TRUE,
    position = "raincloud",
    color = NA,
    alpha = 0.75,
    point_color = "grey40",
    point_alpha = 0.5,
    scale = 5
  ) +

  # Escalas
  scale_x_continuous(limits = c(0, 150)) +

  # Layout
  labs(
    x = "Tasa est. (100.000 hab.)",
    y = NULL
  )


### Joinpoints -----
g2 <- mod |>
  map_df(
    ~ {
      tibble(
        anio = .x$model$anio,
        obs = .x$model$`log(value)`,
        fit = predict(.x)
      )
    },
    .id = "grupo_n2"
  ) |>

  # Añadir joinpoints
  mutate(
    jp = case_when(
      grupo_n2 == "GC1" ~ mod$GC1$psi[, "Est."],
      grupo_n2 == "GC2" & anio < 2018 ~ mod$GC2$psi[1, "Est."],
      grupo_n2 == "GC2" & anio >= 2018 ~ mod$GC2$psi[2, "Est."],
      grupo_n2 == "GC4" ~ mod$GC4$psi[, "Est."],
    ) |>
      round()
  ) |>

  # Gráfico
  ggplot(aes(x = anio, y = obs, color = grupo_n2)) +

  # Geometrías
  geom_point(size = 2.5, alpha = .75) +
  geom_line(aes(y = fit), lwd = 1) +
  geom_vline(aes(xintercept = jp), color = "darkgrey", lwd = 2, alpha = .5) +

  # Escalas
  scale_x_continuous(n.breaks = 10) +
  labs(x = NULL, y = "Tasa est. (100.000 hab.)")


### Unir gráficos -----
fig2 <- g1 /
  g2 &
  # Layout
  scale_fill_scico_d(
    palette = "managua",
    aesthetics = c("colour", "fill"),
    name = NULL
  ) &
  theme_minimal() &
  theme(legend.position = "bottom") &
  plot_annotation(tag_levels = "A")


### Guardar gráfico ----
ggsave(
  fig2,
  filename = "Figura2.png",
  width = 16,
  height = 18,
  units = "cm",
  dpi = 300
)


# Evolución tasas GC (regiones) ------------------------------------------
## Tasas estandarizadas por año y región ---------------------------------
datos_jp_reg <- recod_defun |>
  # Seleccionar muertes por GC
  filter(str_detect(grupo_n1, "GC")) |>
  droplevels() |>

  # Agrupar datos por año
  count(anio, region_deis, grupo_edad, grupo_n2) |>

  # Unir con proyecciones poblacionales
  left_join(
    proy_2010_2023 |>
      # Agrupar por año
      count(anio, region_deis, grupo_edad, wt = proy_pob, name = "pob")
  ) |>

  # Añadir población estándar 2022
  left_join(pob_est_2022) |>

  # Crear variable de agrupamiento
  unite(c(region_deis, grupo_n2), col = "region_grupo") |>

  # Calcular tasa estandarizada
  group_by(anio, region_grupo) |>
  calculate_dsr(
    x = n,
    n = pob,
    stdpop = pob_est_2022,
    type = "standard"
  )


## Regresión joinpoint ---------------------------------------------------
mod_reg <- model_jp(
  datos_jp_reg,
  value = "value",
  time = "anio",
  group = "region_grupo",
  k = 3
)


## Tabla 2 ---------------------------------------------------------------
tab2 <- summary_jp(mod_reg, ft = TRUE, var1 = "Región", var2 = "Nivel")


## Figura S2 -------------------------------------------------------------
figs2 <- datos_jp_reg |>
  ggplot(aes(x = value, y = grupo_n2, fill = grupo_n2)) +
  facet_wrap(~region_deis, ncol = 2) +
  geom_density_ridges(
    jittered_points = TRUE,
    position = "raincloud",
    color = NA,
    alpha = 0.75,
    point_color = "grey40",
    point_alpha = 0.5
  ) +
  scale_fill_scico_d(palette = "managua", name = NULL) +
  scale_x_continuous(limits = c(0, 150)) +
  labs(
    x = "Tasa est. de mortalidad (100.000 hab.)",
    y = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "none")


### Guardar gráfico -----
ggsave(
  figs2,
  filename = "FiguraS2.png",
  width = 16,
  height = 18,
  units = "cm",
  dpi = 300
)
