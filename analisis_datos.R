### Mortalidad por enfermedades no transmisibles y causas externas en
### Argentina (2010–2023): redistribución de defunciones por causas garbage
### Análisis exploratorio de datos
### Autora: Tamara Ricardo
### Revisor: Juan I. Irassar
# Última modificación: 14-04-2026 09:33

# Cargar paquetes --------------------------------------------------------
pacman::p_load(
  # Mapas
  sf,
  tmap,
  geoAr,
  # Gráficos
  patchwork,
  treemapify,
  scico,
  # Tablas
  gtsummary,
  flextable,
  # Estadísticos
  PHEindicatormethods,
  ljr,
  spdep,
  # Manejo de datos
  scales,
  rio,
  janitor,
  tidyverse
  # update = TRUE
)


# Configuración tablas ---------------------------------------------------
set_gtsummary_theme(list(
  "style_number-arg:decimal.mark" = ",",
  "style_number-arg:big.mark" = "."
))


# Cargar/preparar datos --------------------------------------------------
## Defunciones anuales por grupo de causas
recod_defun <- import("clean/arg_defun_mes_2010-2022_recod.rds") |>
  # Crear variable para subgrupo de causas
  mutate(
    subgrupo_causa = case_when(
      str_detect(grupo_causa, "Dia|ECV|ERC|Neo|ENT") ~ "ENT",
      str_detect(grupo_causa, "Hom|Sui|Trá|CE") ~ "CE",
      str_detect(grupo_causa, "GC") ~ "GC",
      grupo_causa == "CMNN" ~ "CMNN"
    ) |>

      # Ordenar niveles
      fct_relevel("CE", "CMNN", after = 1)
  )


## Proyecciones poblacionales anuales (2010-2022)
proy_pob_anio <- import("clean/arg_pob_mensual_2010_2023.rds") |>
  # Filtrar datos primer mes del año
  filter(mes == 1) |>
  select(-mes)


## Población estándar Argentina (2022)
pob_est_2022 <- import("clean/arg_pob_est_2022.rds") |>
  # Filtrar totales
  distinct(grupo_edad, pob_est_2022)


# Crear shapefile jurisdicciones -----------------------------------------
shp_arg <- get_geo(geo = "ARGENTINA") |>
  ## Añadir ids de provincia
  add_geo_codes() |>

  ## Crear columna para jurisdicción
  mutate(
    jurisdiccion = case_when(
      codprov_censo == "02" ~ "CABA",
      codprov_censo %in% c("38", "66") ~ "NOA1",
      codprov_censo %in% c("10", "86") ~ "NOA2",
      codprov_censo %in% c("46", "70", "74") ~ "Cuyo",
      codprov_censo %in% c("42", "58", "62") ~ "Patagonia Norte",
      codprov_censo %in% c("26", "78", "94") ~ "Patagonia Sur",

      ### Valor por defecto
      .default = name_iso
    )
  ) |>

  ## Agrupar polígonos por jurisdicción
  summarise(geometry = st_union(geometry), .by = jurisdiccion)


# Análisis exploratorio --------------------------------------------------
## Frecuencia defunciones por sexo
recod_defun |>
  tabyl(sexo) |>
  adorn_pct_formatting()


## Frecuencia defunciones por grupo etario
recod_defun |>
  tabyl(grupo_edad) |>
  adorn_pct_formatting()


# Tabla 1 ----------------------------------------------------------------
## Frecuencia defunciones por grupo causas y edad
recod_defun |>
  # Cambiar niveles grupo_causa
  mutate(
    grupo_causa = if_else(
      str_detect(grupo_causa, "GC"),
      grupo_causa,
      subgrupo_causa
    ) |>
      # Ordenar niveles
      fct_drop() |>
      fct_relevel("ENT", "CE")
  ) |>

  # Tabla
  tbl_cross(
    row = grupo_edad,
    col = grupo_causa,
    percent = "row",
    # statistic = "{p}%",
    digits = c(0, 1),
    margin = "row",
    label = list(
      grupo_edad = "Grupo etario",
      grupo_causa = "Grupo de causas"
    )
  ) |>

  add_p() |>
  bold_labels()


# Evolución temporal tasas GC --------------------------------------------
## Tasas de mortalidad estandarizadas
datos_jp <- recod_defun |>
  # Filtrar muertes por GC
  filter(str_detect(grupo_causa, "GC")) |>
  droplevels() |>

  # Agrupar muertes por año
  count(anio, grupo_edad, grupo_causa) |>

  # Añadir proyecciones poblacionales
  left_join(
    proy_pob_anio |>
      count(anio, grupo_edad, wt = proy_pob_mes, name = "proy_pob")
  ) |>

  # Añadir población estándar
  left_join(pob_est_2022) |>

  # Calcular tasa estandarizada
  group_by(anio, grupo_causa) |>
  calculate_dsr(
    x = n,
    n = proy_pob,
    stdpop = pob_est_2022,
    type = "standard"
  ) |>

  # Pasar a formato wide
  select(anio, grupo_causa, tasa = value, n = total_count, pob = total_pop) |>
  pivot_wider(
    names_from = grupo_causa,
    values_from = c(n, tasa),
    names_glue = "{grupo_causa}_{.value}"
  )


## Regresión joinpoint -----
# GC1
m1 <- ljrb(K = 3, y = datos_jp$GC1_n, n = datos_jp$pob, tm = datos_jp$anio)

# GC2
m2 <- ljrb(K = 3, y = datos_jp$GC2_n, n = datos_jp$pob, tm = datos_jp$anio)

# GC3
m3 <- ljrb(K = 3, y = datos_jp$GC3_n, n = datos_jp$pob, tm = datos_jp$anio)

# GC4
m4 <- ljrb(K = 3, y = datos_jp$GC4_n, n = datos_jp$pob, tm = datos_jp$anio)


## Función auxiliar para estimar APC y AAPC -----
get_apc_segments <- function(model, years) {
  # Coeficientes del modelo
  coefs <- model$Coef

  # Intervalos
  breaks <- c(min(years), sort(model$Joinpoints), max(years))

  # Tabla
  tibble(
    inicio = head(breaks, -1),
    fin = tail(breaks, -1),
    duracion = fin - inicio,
    beta = cumsum(
      c(coefs["t"], coefs[str_detect(names(coefs), "max")])
    ),
    APC = exp(beta) - 1,
    AAPC = if_else(
      inicio == min(years),
      sum(APC * duracion) / sum(duracion),
      NA
    )
  )
}


## Tabla S2: Resultados joinpoint -----
tab <- bind_rows(
  # GC1
  get_apc_segments(model = m1, years = datos_jp$anio),
  # GC2
  get_apc_segments(model = m2, years = datos_jp$anio),
  # GC3
  get_apc_segments(model = m3, years = datos_jp$anio),
  # GC4
  get_apc_segments(model = m4, years = datos_jp$anio),
  .id = c("nivel")
) |>

  # Etiquetas niveles
  mutate(nivel = fct_relabel(nivel, ~ c("GC1", "GC2", "GC3", "GC4"))) |>

  # Formato columnas años
  mutate(across(
    .cols = c(inicio:duracion),
    .fns = ~ number(.x, accuracy = 1, big.mark = "")
  )) |>

  # Formato beta
  mutate(beta = number(beta, accuracy = .001, decimal.mark = ",")) |>

  # Formato columnas frecuencias
  mutate(across(
    .cols = c(APC, AAPC),
    .fns = ~ percent(.x, accuracy = .1, decimal.mark = ",")
  )) |>

  # Formato de tabla
  flextable() |>
  merge_v(j = 1)

tab

# Figura 2 ---------------------------------------------------------------
# Joinpoints GC1
g1 <- datos_jp |>
  ggplot(aes(x = anio, y = GC1_tasa)) +

  # Geometrías
  geom_point(
    size = 3.5,
    color = "#FFCE66BF"
  ) +

  geom_line(color = "#FFCE66BF") +

  geom_vline(
    xintercept = m1$Joinpoints,
    color = "darkgrey",
    linewidth = 1
  ) +
  labs(title = "GC1", y = "Tasa estandarizada (100.000 hab.)")


# Joinpoints GC2
g2 <- datos_jp |>
  ggplot(aes(x = anio, y = GC2_tasa)) +

  # Geometrías
  geom_point(
    size = 3.5,
    color = "#92463ABF"
  ) +
  geom_line(color = "#92463ABF") +
  geom_vline(
    xintercept = m2$Joinpoints,
    color = "darkgrey",
    linewidth = 1
  ) +

  labs(title = "GC2", y = NULL)


# Joinpoints GC3
g3 <- datos_jp |>
  ggplot(aes(x = anio, y = GC3_tasa)) +

  # Geometrías
  geom_point(
    size = 3.5,
    color = "#4D5492BF"
  ) +
  geom_line(color = "#4D5492BF") +
  geom_vline(
    xintercept = m3$Joinpoints,
    color = "darkgrey",
    linewidth = 1
  ) +

  labs(title = "GC3", y = "Tasa estandarizada (100.000 hab.)")


# Joinpoints GC4
g4 <- datos_jp |>
  ggplot(aes(x = anio, y = GC4_tasa)) +

  # Geometrías
  geom_point(
    size = 3.5,
    color = "#80E6FFBF"
  ) +
  geom_line(color = "#80E6FFBF") +
  geom_vline(
    xintercept = m4$Joinpoints,
    color = "darkgrey",
    linewidth = 1
  ) +

  labs(title = "GC4", y = NULL)


## Unir gráficos -----
g1 +
  g2 +
  g3 +
  g4 &
  scale_x_continuous(name = NULL, limits = c(2010, NA), n.breaks = 7) &
  scale_y_log10(n.breaks = 7) &
  theme_minimal() &
  theme(
    plot.title = element_text(face = "bold", size = 11),
    axis.title.y = element_text(size = 9)
  )


## Guardar gráfico ----
ggsave(
  filename = "Figura2.png",
  width = 16,
  height = 16,
  units = "cm",
  dpi = 300
)

# Distribución espacial muertes GC ---------------------------------------
