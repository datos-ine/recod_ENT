### Mortalidad por enfermedades no transmisibles y causas externas en
### Argentina (2010–2023): redistribución de defunciones por causas garbage
### Análisis exploratorio de datos
### Autora: Tamara Ricardo
### Revisor: Juan I. Irassar

# Cargar paquetes --------------------------------------------------------
pacman::p_load(
  # Mapas
  sf,
  tmap,
  geoAr,
  # Gráficos
  patchwork,
  ggrepel,
  treemapify,
  scico,
  # Tablas
  gtsummary,
  # Estadísticos
  PHEindicatormethods,
  glmmTMB,
  spdep,
  easystats,
  # Manejo de datos
  # BAMMtools,
  scales,
  rio,
  janitor,
  tidyverse
)


# Configuración tablas ---------------------------------------------------
set_gtsummary_theme(list(
  "style_number-arg:decimal.mark" = ",",
  "style_number-arg:big.mark" = "."
))


# Cargar datos -----------------------------------------------------------
recod_defun <- import("clean/arg_defun_mes_2010-2022_recod.rds")


## Proyecciones poblacionales anuales (2010-2022)
proy_pob_anio <- import("clean/arg_pob_mensual_2010_2023.rds") |>
  filter(mes == 1) |>
  select(anio, jurisdiccion:proy_pob, pob_est_2022)


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
  group_by(jurisdiccion) |>
  summarise(geometry = st_union(geometry))


# Análisis exploratorio --------------------------------------------------
## Frecuencia defunciones por sexo
recod_defun |>
  count(sexo, wt = n) |>
  mutate(pct = percent(n / sum(n), accuracy = 0.1))


## Frecuencia defunciones por grupo etario
recod_defun |>
  count(grupo_edad, wt = n) |>
  mutate(pct = percent(n / sum(n), accuracy = 0.1))


## Frecuencia defunciones por grupo de causas
recod_defun |>
  count(grupo_causa, wt = n) |>
  mutate(pct = percent(n / sum(n), accuracy = 0.1))


## Tabla 1: Frecuencia defunciones por grupo causa y edad -----
recod_defun |>
  # Seleccionar variables
  select(grupo_edad, grupo_causa, n) |>

  # Recategorizar causas
  mutate(
    grupo_causa = if_else(
      str_detect(grupo_causa, "GC"),
      grupo_causa,
      "Causas definidas"
    )
  ) |>

  # Individualizar filas
  uncount(weights = n) |>

  # Tabla
  tbl_cross(
    row = grupo_edad,
    col = grupo_causa,
    percent = "row",
    digits = c(0, 1),
    margin = "row",
    label = list(grupo_edad = "Grupo etario", grupo_causa = "Grupo de causas")
  ) |>

  add_p() |>
  bold_labels()


## Figura 2: Frecuencias por grupo causa -----
### Frecuencias paso 1 -----
fig2.1 <- recod_defun |>
  count(grupo_causa, wt = n) |>
  mutate(pct = n / sum(n)) |>

  # Crear subgrupos
  mutate(
    grupo_causa = if_else(
      str_detect(grupo_causa, "Diab"),
      "Diabetes",
      grupo_causa
    ),

    subgrupo1 = case_when(
      str_detect(grupo_causa, "Dia|ECV|ERC|Neo|ENT") ~ "ENT",
      str_detect(grupo_causa, "Hom|Sui|Trá|CE") ~ "CE",
      str_detect(grupo_causa, "GC") ~ "GC",
      grupo_causa == "CMNN" ~ "CMNN"
    ),

    subgrupo2 = if_else(
      grupo_causa %in% c("Otras ENT", "Otras CE"),
      grupo_causa,
      subgrupo1
    ) |>
      fct_relevel("ENT", "Otras ENT", "CE", "Otras CE")
  )


### Frecuencias paso 2 -----
fig2.2 <- recod_defun |>
  count(grupo_causa = paso2.2, wt = n) |>
  mutate(pct = n / sum(n)) |>

  # Crear subgrupos
  mutate(
    grupo_causa = if_else(
      str_detect(grupo_causa, "Diab"),
      "Diabetes",
      grupo_causa
    ),

    subgrupo1 = case_when(
      str_detect(grupo_causa, "Dia|ECV|ERC|Neo|ENT") ~ "ENT",
      str_detect(grupo_causa, "Hom|Sui|Trá|CE") ~ "CE",
      str_detect(grupo_causa, "GC") ~ "GC",
      grupo_causa == "CMNN" ~ "CMNN"
    ),

    subgrupo2 = if_else(
      grupo_causa %in% c("Otras ENT", "Otras CE"),
      grupo_causa,
      subgrupo1
    ) |>
      fct_relevel("ENT", "Otras ENT", "CE", "Otras CE")
  )


### Unir gráficos -----
(fig2.1 |>
  ggplot(aes(area = pct, subgroup = subgrupo1)) +
  theme(legend.position = "none")) /

  (fig2.2 |>
    ggplot(aes(area = pct, subgroup = subgrupo1)) +
    theme(legend.position = "bottom")) &

  # Geometrías
  geom_treemap(aes(fill = subgrupo2)) &
  geom_treemap_subgroup_border() &
  geom_treemap_text(
    # aes(label = grupo_causa),
    aes(
      label = paste0(
        grupo_causa,
        " (",
        percent(pct, accuracy = .1, decimal.mark = ","),
        ")"
      )
    ),
    place = "center",
    color = "white",
    fontface = "bold",
    size = 9,
    reflow = TRUE
  ) &

  scale_fill_scico_d(
    palette = "navia",
    begin = .15,
    end = .85,
    name = ""
  ) &

  plot_annotation(tag_levels = "A")


### Guardar gráfico -----
ggsave(
  filename = "Figura2.png",
  width = 16,
  height = 19,
  units = "cm",
  dpi = 300
)


## Cambio de frecuencias paso 1 vs paso 2 -----
fig2.1 |>
  select(grupo_causa, pct1 = pct) |>
  left_join(
    fig2.2 |>
      select(grupo_causa, pct2 = pct)
  ) |>

  mutate(cambio_fr = percent(pct2 - pct1, accuracy = .1))


# Evolución temporal muertes por GC --------------------------------------
## Dataset para las tasas -----
tasa_anio <- recod_defun |>
  # Filtrar muertes por GC
  filter(str_detect(grupo_causa, "GC")) |>

  droplevels() |>

  # Agrupar muertes
  count(anio, jurisdiccion, grupo_edad, sexo, grupo_causa, wt = n) |>

  # Estandarizar año
  mutate(anio_st = anio - 2010) |>

  # Añadir población
  left_join(proy_pob_anio)


## GLMM con interacción -----
m1 <- glmmTMB(
  n ~ grupo_causa * anio_st + (1 | grupo_edad),
  offset = log(proy_pob),
  family = nbinom2,
  data = tasa_anio
)

# Modelo sin interacción
m1.1 <- update(m1, ~ . - grupo_causa:anio)

# Comparar modelos
compare_performance(m1, m1.1, rank = TRUE, metrics = c("AIC", "BIC"))

# Análisis residuales
check_overdispersion(m1)

check_zeroinflation(m1)

# Parámetros
model_parameters(m1, exponentiate = TRUE)


## Figura 3: Evolución temporal tasas GC -----
tasa_anio |>
  # Calcular tasa
  group_by(anio, grupo_causa) |>
  calculate_dsr(
    x = n,
    n = proy_pob,
    stdpop = pob_est_2022
  ) |>

  # Crear etiqueta tasas al inicio y fin del estudio
  mutate(
    value_ci = if_else(
      anio %in% c(2010, 2023),
      paste0(
        number(value, accuracy = 0.1, decimal.mark = ","),
        " (IC ",
        number(lowercl, accuracy = 0.1, decimal.mark = ","),
        "-",
        number(uppercl, accuracy = 0.1, decimal.mark = ","),
        ")"
      ),
      NA
    )
  ) |>

  # Año a formato fecha
  mutate(anio = make_date(anio)) |>

  ### Gráfico
  ggplot(aes(
    x = anio,
    y = value,
    color = grupo_causa,
    fill = grupo_causa,
    group = grupo_causa
  )) +

  # Geometrías
  geom_ribbon(
    aes(ymin = lowercl, ymax = uppercl),
    # alpha = 0.35,
    color = NA
  ) +
  geom_line() +
  # geom_point() +
  geom_text_repel(
    aes(label = value_ci),
    vjust = c(rep(-1, 52), rep(2, 4)),
    min.segment.length = 0,
    size = 2.5,
    color = "grey40"
  ) +

  # Escalas
  scale_color_scico_d(
    palette = "navia",
    begin = .15,
    end = .85,
    name = ""
  ) +

  scale_fill_scico_d(
    palette = "navia",
    begin = .15,
    end = .85,
    alpha = .3,
    name = ""
  ) +

  scale_x_date(
    date_breaks = "2 years",
    date_labels = "%Y"
  ) +

  scale_y_continuous(
    limits = c(0, 125),
    n.breaks = 7
  ) +

  # Layout
  labs(
    x = NULL,
    y = "Tasa est.",
    color = NULL
  ) +
  # coord_fixed(ratio = 100) +
  theme_minimal()


### Guardar gráfico ----
ggsave(
  filename = "Figura3.png",
  width = 16,
  height = 14,
  units = "cm",
  dpi = 300
)


# Distribución espacial --------------------------------------------------
## Tasa estandarizada por jurisdicción ----
tasa_esp <- shp_arg |>
  left_join(
    tasa_anio |>
      group_by(jurisdiccion, grupo_causa) |>
      calculate_dsr(
        x = n,
        n = proy_pob,
        stdpop = pob_est_2022
      )
  )


## Test de Moran global (todos los GC) ----
moran.test(
  x = tasa_esp$value,
  listw = poly2nb(tasa_esp) |> nb2listw(zero.policy = TRUE),
  zero.policy = TRUE
)


### Test de Moran global (GC1) ----
tasa_esp |>
  filter(grupo_causa == "GC1") |>
  (\(df) {
    moran.test(
      x = df$value,
      listw = poly2nb(df) |> nb2listw(zero.policy = TRUE),
      zero.policy = TRUE
    )
  })()


### Test de Moran global (GC2) ----
tasa_esp |>
  filter(grupo_causa == "GC2") |>
  (\(df) {
    moran.test(
      x = df$value,
      listw = poly2nb(df) |> nb2listw(zero.policy = TRUE),
      zero.policy = TRUE
    )
  })()


### Test de Moran global (GC3) ----
tasa_esp |>
  filter(grupo_causa == "GC3") |>
  (\(df) {
    moran.test(
      x = df$value,
      listw = poly2nb(df) |> nb2listw(zero.policy = TRUE),
      zero.policy = TRUE
    )
  })()


### Test de Moran global (GC4) ----
tasa_esp |>
  filter(grupo_causa == "GC4") |>
  (\(df) {
    moran.test(
      x = df$value,
      listw = poly2nb(df) |> nb2listw(zero.policy = TRUE),
      zero.policy = TRUE
    )
  })()


## Índice de Moran local (Ii) -----
tasa_esp <- tasa_esp |>
  bind_cols(localmoran(
    x = tasa_esp$value,
    listw = poly2nb(tasa_esp, queen = TRUE) |> nb2listw()
  ))

## Figura 4 -----
### Tasas estandarizadas ------
fig4.1 <- tasa_esp |>
  # Mapa
  tm_shape() +
  tm_polygons(
    fill = "value",
    fill.scale = tm_scale_continuous(values = "-scico.navia"),
    fill_alpha = .75,
    fill.legend = tm_legend(
      position = tm_pos_out(),
      frame = FALSE,
      bg = FALSE,
      height = 10,
      margins = c(1, 1, 1, -2),
      title = "Tasa est."
    )
  ) +

  # Coordenadas
  tm_grid(lines = FALSE, n.y = 4, n.x = 5, ) +

  # Flecha del Norte
  tm_compass(position = tm_pos_out(pos.v = "bottom")) +

  # Layout
  tm_facets_wrap(by = "grupo_causa", nrow = 1)


### Índice de Moran local (Ii) -----
fig4.2 <- tasa_esp |>
  # Mapa
  tm_shape() +
  tm_polygons(
    fill = "Ii",
    fill.scale = tm_scale_continuous(values = "-scico.navia"),
    fill_alpha = .75,
    fill.legend = tm_legend(
      position = tm_pos_out(),
      frame = FALSE,
      bg = FALSE,
      height = 10,
      margins = c(1, 1, 1, -2),
      title = "Ii"
    )
  ) +

  # Coordenadas
  tm_grid(lines = FALSE, n.y = 4, n.x = 5, ) +

  # Flecha del Norte
  tm_compass(position = tm_pos_out(pos.v = "bottom")) +

  # Layout
  tm_facets_wrap(by = "grupo_causa", nrow = 1)

### Unir gráficos ----
tmap_arrange(fig4.1, fig4.2) |>

  tmap_save(
    filename = "Figura4.png",
    width = 16,
    height = 16,
    units = "cm",
    dpi = 300
  )
