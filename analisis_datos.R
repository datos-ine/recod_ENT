### Mortalidad por enfermedades no transmisibles y causas externas en
### Argentina (2010–2023): redistribución de defunciones por causas garbage
### Análisis de datos
### Autora: Tamara Ricardo
### Revisor: Juan I. Irassar
# Última modificación: 23-04-2026 15:03

# Cargar paquetes --------------------------------------------------------
pacman::p_load(
  # Mapas
  sf,
  tmap,
  geoAr,
  spdep,
  # Gráficos
  patchwork,
  ggridges,
  scico,
  # Tablas
  flextable,
  # Tasas estandarizadas
  PHEindicatormethods,
  # Regresión joinpoint
  segmented,
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


# Crear shapefile jurisdicciones -----------------------------------------
shp_arg <- get_geo(geo = "ARGENTINA") |>
  # Añadir ids de provincia
  add_geo_codes() |>

  # Crear columna para jurisdicción
  mutate(
    jurisdiccion = case_when(
      codprov_censo == "02" ~ "CABA",
      codprov_censo %in% c("38", "66") ~ "NOA1",
      codprov_censo %in% c("10", "86") ~ "NOA2",
      codprov_censo %in% c("46", "70", "74") ~ "Cuyo",
      codprov_censo %in% c("42", "58", "62") ~ "Patagonia Norte",
      codprov_censo %in% c("26", "78", "94") ~ "Patagonia Sur",
      .default = name_iso
    )
  ) |>

  ## Agrupar polígonos por jurisdicción
  summarise(
    geometry = st_union(geometry),
    .by = jurisdiccion
  )

## Vecinos espaciales
nb <- poly2nb(shp_arg, queen = TRUE)


# Análisis exploratorio --------------------------------------------------
## Frecuencia defunciones por sexo
recod_defun |>
  tabyl(sexo) |>
  adorn_pct_formatting()

## Frecuencia defunciones por grupo etario
recod_defun |>
  tabyl(grupo_edad) |>
  adorn_pct_formatting()

## Frecuencia defunciones por grupo causa nivel 1
recod_defun |>
  tabyl(grupo_edad, grupo_n1) |>
  adorn_totals(where = "row") |>
  adorn_percentages() |>
  adorn_pct_formatting()


## Códigos garbage más frecuentes
recod_defun |>
  filter(grupo_n2 %in% c("GC1", "GC2")) |>
  count(cie10_cod) |>
  mutate(pct = percent(n / sum(n), accuracy = .1)) |>
  arrange(-n) |>
  head(n = 15)


## Tabla 1 ---------------------------------------------------------------
### Frecuencia defunciones por grupo etario y grupo de causas
tab1 <- recod_defun |>
  # Seleccionar columnas
  select(grupo_edad, grupo_n2) |>
  # Reagrupar causas GC
  mutate(
    grupo_n2 = case_when(
      grupo_n2 %in% c("GC1", "GC2") ~ "GC1-2",
      grupo_n2 %in% c("GC3", "GC4") ~ "GC3-4",
      grupo_n2 == "Diabetes" ~ "DM",
      grupo_n2 == "Neoplasias" ~ "NPL",
      grupo_n2 == "Homicidio" ~ "HOM",
      grupo_n2 == "Suicidio" ~ "SUI",
      grupo_n2 == "Tránsito" ~ "TRA",
      .default = grupo_n2
    ) |>

      # Ordenar
      fct_relevel(
        "NPL",
        "Otras ENT",
        "HOM",
        "SUI",
        "TRA",
        "Otras CE",
        after = 4
      )
  ) |>

  # Crear tabla
  tabyl(grupo_edad, grupo_n2) |>
  adorn_totals(where = "row") |>
  adorn_percentages() |>
  mutate(across(.cols = where(is.numeric), .fns = ~ round(.x * 100, 1))) |>

  # Formato tabla
  flextable() |>
  colformat_num(
    decimal.mark = ",",
    big.mark = ".",
  ) |>
  align(j = -1, align = "center", part = "all") |>
  autofit() |>

  # Encabezado
  add_header_row(
    values = c("Grupo etario", "CMNN", "ENT", "CE", "GC"),
    colwidths = c(1, 1, 5, 4, 2)
  ) |>
  merge_at(j = 1, part = "header") |>
  merge_at(j = 2, part = "header") |>

  # Pie de tabla
  add_footer_row(
    values = c(
      "DM: diabetes mellitus; ECV: enfermedades cardiovasculares; ERC: enfermedades respiratorias crónicas; NPL: neoplasias; HOM: homicidio; SUI: suicidio; TRA: accidentes de tránsito; GC: causas garbage."
    ),
    colwidths = 13
  )


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
mod <- datos_jp |>
  group_by(grupo_n2) |>
  group_split() |>
  set_names(levels(datos_jp$grupo_n2)) |>
  map(
    ~ {
      lm(log(value) ~ anio, data = .x) |>
        selgmented(
          seg.Z = ~anio,
          Kmax = 3,
          type = "bic",
          th = 2,
          plot.ic = TRUE
        )
    }
  )


### GC1 -----
# joinpoints
summary(mod$GC1)

# APC
slope(mod$GC1, APC = TRUE, digits = 2)

# AAPC
aapc(mod$GC1, wrong.se = FALSE) |>
  percent()


### GC2 -----
# joinpoints
summary(mod$GC2)

# APC
slope(mod$GC2, APC = TRUE, digits = 2)

# AAPC
aapc(mod$GC2) |>
  percent()


### GC3 -----
epikit::fmt_ci(
  e = (exp(mod$GC3$coefficients[2]) - 1) * 100,
  l = (exp(confint(mod$GC3)[2, 1]) - 1) * 100,
  u = (exp(confint(mod$GC3)[2, 2]) - 1) * 100
)


### GC4 -----
# joinpoints
summary(mod$GC4)

# APC
slope(mod$GC4, APC = TRUE, digits = 2)

# AAPC
aapc(mod$GC4) |>
  percent()


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
    point_alpha = 0.5
  ) +

  # Escalas
  scale_fill_scico_d(palette = "managua", name = NULL) +
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
    )
  ) |>

  # Gráfico
  ggplot(aes(x = anio, y = obs, color = grupo_n2)) +
  facet_wrap(~grupo_n2) +

  # Geometrías
  geom_point(size = 2.5, alpha = .75) +
  geom_line(aes(y = fit), lwd = 1.25) +
  geom_vline(aes(xintercept = jp), color = "darkgrey", lwd = 1) +

  # Escalas
  scale_color_scico_d(palette = "managua") +
  scale_x_continuous(n.breaks = 7) +
  labs(x = "Año", y = "Tasa est. (100.000 hab.)")


### Unir gráficos -----
fig2 <- g1 /
  g2 &
  # Layout
  theme_minimal() &
  theme(legend.position = "none") &
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

  # Calcular tasa estandarizada
  group_by(anio, region_deis, grupo_n2) |>
  calculate_dsr(
    x = n,
    n = pob,
    stdpop = pob_est_2022,
    type = "standard"
  )


## Figura 3 --------------------------------------------------------------
fig3 <- datos_jp_reg |>
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
  fig3,
  filename = "Figura3.png",
  width = 16,
  height = 16,
  units = "cm",
  dpi = 300
)


## Regresión joinpoint ---------------------------------------------------
mod_reg <- datos_jp_reg |>
  group_by(region_deis, grupo_n2) |>
  group_split() |>
  set_names(
    datos_jp_reg |>
      distinct(region_deis, grupo_n2) |>
      unite("id", region_deis, grupo_n2, remove = FALSE) |>
      pull(id)
  ) |>
  map(
    ~ {
      lm(log(value) ~ anio, data = .x) |>
        selgmented(
          seg.Z = ~anio,
          Kmax = 3,
          type = "bic",
          th = 2,
          plot.ic = T
        )
    }
  )


## Tabla 2 ---------------------------------------------------------------
## Generar tabla
tab2 <- mod_reg |>
  map_dfr(
    ~ {
      if ("segmented" %in% class(.x)) {
        cortes <- sort(c(
          min(.x$model$anio),
          .x$psi[, "Est."],
          max(.x$model$anio)
        ))

        # APC
        sl <- slope(.x, APC = TRUE, digits = 2)$anio |>
          as_tibble() |>
          clean_names()

        # AAPC
        aapc_obj <- aapc(.x, parm = "anio", wrong.se = FALSE)

        sl |>
          mutate(
            # Número de joinpoints
            jp = if_else(
              row_number() == 1,
              nrow(.x$psi),
              NA
            ),

            # Período
            periodo = paste(
              round(head(cortes, -1)),
              round(tail(cortes, -1)),
              sep = "-"
            ),

            # APC
            APC = number(est, accuracy = .1, decimal.mark = ",", suffix = "%"),

            # 95% IC APC
            ic_apc = paste0(
              "(",
              number(ci_95_percent_l, accuracy = 0.1, decimal.mark = ","),
              "; ",
              number(
                ci_95_percent_u,
                accuracy = 0.1,
                decimal.mark = ",",
                suffix = "%"
              ),
              ")"
            ),

            # AAPC solo en primer segmento
            AAPC = if_else(
              row_number() == 1,
              percent(aapc_obj[1], accuracy = 0.1, decimal.mark = ","),
              NA
            ),

            # 95%IC del AAPC
            ic_aapc = if_else(
              row_number() == 1,
              paste0(
                "(",
                percent(
                  aapc_obj[3],
                  accuracy = 0.1,
                  decimal.mark = ",",
                  suffix = ""
                ),
                "; ",
                percent(aapc_obj[4], accuracy = 0.1, decimal.mark = ","),
                ")"
              ),
              NA
            )
          )
      } else {
        tibble()
      }
    },
    .id = "id"
  ) |>

  # Separar región y nivel
  separate_wider_delim(id, names = c("region", "nivel"), delim = "_") |>

  # Descartar columnas
  select(-est, -starts_with("ci"))


## Mostrar tabla
tab2 |>
  flextable() |>
  set_header_labels(
    values = list(
      region = "Región",
      nivel = "Nivel",
      jp = "JP",
      periodo = "Período",
      ic_apc = "95%IC",
      ic_aapc = "95%IC"
    )
  ) |>
  merge_v() |>
  autofit() |>
  
  theme_booktabs()
