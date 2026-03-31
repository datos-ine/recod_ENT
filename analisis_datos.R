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
  DiagrammeR,
  scico,
  # Tablas
  gtsummary,
  # Estadísticos
  PHEindicatormethods,
  glmmTMB,
  spdep,
  easystats,
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


# Cargar datos -----------------------------------------------------------
## Defunciones anuales por grupo de causas
recod_defun <- import("clean/arg_defun_mes_2010-2022_recod.rds") |>
  # Crear subgrupo causas
  mutate(
    subgrupo = case_when(
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
  filter(mes == 1)


## Población estándar 2022
pob_est_2022 <- import("clean/arg_pob_est_2022.rds")


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
  summarise(geometry = st_union(geometry), .groups = "drop")


# Análisis exploratorio --------------------------------------------------
## Frecuencia defunciones por sexo
recod_defun |>
  count(sexo) |>
  mutate(pct = percent(n / sum(n), accuracy = 0.1))


## Frecuencia defunciones por grupo etario
recod_defun |>
  count(grupo_edad) |>
  mutate(pct = percent(n / sum(n), accuracy = 0.1))


# Tabla 1 ----------------------------------------------------------------
## Frecuencia defunciones por grupo causas y edad
recod_defun |>
  # Cambiar niveles grupo_causa
  mutate(
    grupo_causa = if_else(
      str_detect(grupo_causa, "GC"),
      grupo_causa,
      subgrupo
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
    label = list(grupo_edad = "Grupo etario", grupo_causa = "Grupo de causas")
  ) |>

  add_p() |>
  bold_labels()


# Evolución temporal muertes por GC --------------------------------------
## Dataset para modelar tasas -----
datos <- recod_defun |>
  # Filtrar muertes por GC
  filter(subgrupo == "GC") |>
  droplevels() |>

  # Agrupar muertes
  count(anio, jurisdiccion, grupo_edad, sexo, grupo_causa) |>

  # Estandarizar año
  mutate(anio_st = scale(anio, center = TRUE, scale = FALSE)) |>

  # Añadir población
  left_join(proy_pob_anio)


## GLMM binomial negativo -----
# Modelo con interacción año:grupo_causa
m1 <- glmmTMB(
  n ~ grupo_causa * anio_st + (1 | grupo_edad),
  offset = log(proy_pob_mes),
  family = nbinom2,
  data = datos
)

# Modelo sin interacción
m1.1 <- update(m1, ~ . - grupo_causa:anio_st)

# Comparar modelos
compare_performance(
  m1,
  m1.1,
  rank = TRUE,
  metrics = c("AIC", "BIC")
)

# Análisis residuales
check_overdispersion(m1)

check_zeroinflation(m1)

# Parámetros
model_parameters(m1, exponentiate = TRUE)


# Figura 2 ---------------------------------------------------------------
## Tasas estandarizadas por año -----
tasa_anio_st <- recod_defun |>
  # Filtrar muertes por GC
  filter(str_detect(grupo_causa, "GC")) |>
  droplevels() |>

  # Agrupar muertes
  count(anio, grupo_causa, grupo_edad) |>

  # Añadir proyecciones poblacionales
  left_join(
    proy_pob_anio |>
      # Agrupar datos
      group_by(anio, grupo_edad) |>
      summarise(proy_pob = sum(proy_pob_mes), .groups = "drop")
  ) |>

  # Añadir población estándar 2022
  left_join(
    pob_est_2022 |>
      distinct(grupo_edad, pob_est_2022)
  ) |>

  # Calcular tasa estandarizada
  group_by(anio, grupo_causa) |>
  calculate_dsr(
    x = n,
    n = proy_pob,
    stdpop = pob_est_2022,
    ageband = grupo_edad
  ) |>

  # Crear etiqueta para tasas al inicio y fin del estudio
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
  mutate(anio = make_date(anio))


## Gráfico -----
tasa_anio_st |>
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
    color = NA,
    alpha = 0.5
  ) +

  geom_line() +

  geom_text_repel(
    aes(label = value_ci),
    vjust = c(rep(-1, 52), 4.5, rep(2, 3)),
    min.segment.length = 0,
    size = 2.5,
    color = "grey30"
  ) +

  # Escalas ejes
  scale_x_date(
    date_breaks = "2 years",
    date_labels = "%Y",
    name = NULL
  ) +

  scale_y_continuous(
    limits = c(0, NA),
    n.breaks = 10,
    name = "Tasa estandarizada (100.000 hab.)"
  ) +

  # Escalas color
  scale_discrete_manual(
    aesthetics = c("color", "fill"),
    values = scico(
      n = 4,
      palette = "managua",
      begin = .1
    ),
    name = NULL
  ) +

  # Layout
  theme_minimal()


## Guardar gráfico ----
ggsave(
  filename = "Figura2.png",
  width = 16,
  height = 14,
  units = "cm",
  dpi = 300
)


# Distribución espacial --------------------------------------------------
## Tasa estandarizada por jurisdicción -----
tasa_esp_st <- recod_defun |>
  # Filtrar muertes por GC
  filter(str_detect(grupo_causa, "GC")) |>
  droplevels() |>

  # Agrupar muertes
  count(jurisdiccion, grupo_causa, grupo_edad) |>

  # Añadir proyecciones poblacionales 2023
  left_join(
    proy_pob_anio |>
      # Seleccionar datos 2023
      filter(anio == 2023) |>

      # Agrupar datos
      group_by(jurisdiccion, grupo_edad) |>
      summarise(proy_pob = sum(proy_pob_mes), .groups = "drop")
  ) |>

  # Añadir población estándar 2022
  left_join(
    pob_est_2022 |>
      distinct(grupo_edad, pob_est_2022)
  ) |>

  # Calcular tasa estandarizada
  group_by(jurisdiccion, grupo_causa) |>
  calculate_dsr(
    x = n,
    n = proy_pob,
    stdpop = pob_est_2022,
    ageband = grupo_edad
  )


## Índice de Moran global -----
### Objeto espacial
tasa_esp_st <- left_join(
  shp_arg,
  tasa_esp_st
)

### GC1 -----
tasa_esp_st |>
  filter(grupo_causa == "GC1") |>
  (\(df) {
    moran.test(
      x = df$value,
      listw = poly2nb(df) |> nb2listw(zero.policy = TRUE),
      zero.policy = TRUE
    )
  })()


### GC2 -----
tasa_esp_st |>
  filter(grupo_causa == "GC2") |>
  (\(df) {
    moran.test(
      x = df$value,
      listw = poly2nb(df) |> nb2listw(zero.policy = TRUE),
      zero.policy = TRUE
    )
  })()


### GC3 -----
tasa_esp_st |>
  filter(grupo_causa == "GC3") |>
  (\(df) {
    moran.test(
      x = df$value,
      listw = poly2nb(df) |> nb2listw(zero.policy = TRUE),
      zero.policy = TRUE
    )
  })()


### GC4 -----
tasa_esp_st |>
  filter(grupo_causa == "GC4") |>
  (\(df) {
    moran.test(
      x = df$value,
      listw = poly2nb(df) |> nb2listw(zero.policy = TRUE),
      zero.policy = TRUE
    )
  })()


## Índice de Moran local (Ii) -----
tasa_esp_st <- tasa_esp_st |>
  left_join(
    tasa_esp_st |>
      group_by(grupo_causa) |>
      group_modify(\(df, key) {
        lw <- poly2nb(df, queen = TRUE) |> nb2listw(zero.policy = TRUE)
        cbind(df, localmoran(df$value, lw, zero.policy = TRUE))
      }) |>
      ungroup()
  )


# Figura 3 ---------------------------------------------------------------
## Mapa tasas estandarizadas ------
fig3.1 <- tasa_esp_st |>
  # Mapa
  tm_shape() +
  tm_polygons(
    fill = "value",
    fill.scale = tm_scale_continuous(values = "-scico.managua"),
    fill_alpha = .75,
    fill.legend = tm_legend(
      position = tm_pos_out(),
      frame = FALSE,
      bg = FALSE,
      height = 10,
      margins = c(1, 1, 2, -1),
      title = "Tasa"
    )
  ) +

  # Coordenadas
  tm_grid(lines = FALSE, n.y = 4, n.x = 5, ) +

  # Flecha del Norte
  tm_compass(position = tm_pos_out(pos.v = "bottom")) +

  # Layout
  tm_facets_wrap(by = "grupo_causa", nrow = 1) +
  tm_title("Tasa estandarizada de mortalidad (100.000 hab.)", size = 1)


## Mapa índice de Moran local -----
fig3.2 <- tasa_esp_st |>
  # Mapa
  tm_shape() +
  tm_polygons(
    fill = "Ii",
    fill.scale = tm_scale_continuous(
      values = "-scico.managua"
    ),
    fill_alpha = .75,
    fill.legend = tm_legend(
      position = tm_pos_out(),
      frame = FALSE,
      bg = FALSE,
      height = 10,
      margins = c(1, 1, 1, -1),
      title = "Ii"
    )
  ) +

  # Coordenadas
  tm_grid(lines = FALSE, n.y = 4, n.x = 5, ) +

  # Flecha del Norte
  tm_compass(position = tm_pos_out(pos.v = "bottom")) +

  # Layout
  tm_facets_wrap(by = "grupo_causa", nrow = 1) +
  tm_title("índice de Moran local (Ii)", size = 1)


### Guardar mapa -----
fig3 <- tmap_arrange(fig3.1, fig3.2)

tmap_save(
  fig3,
  filename = "Figura3.png",
  width = 16,
  height = 16,
  units = "cm",
  dpi = 300
)


# Figura 4 ---------------------------------------------------------------
## Frecuencias paso 1 -----
tabyl(recod_defun, grupo_causa) |>
  arrange(-n) |>
  adorn_pct_formatting()


## Frecuencias por paso -----
datos <- recod_defun |>
  count(subgrupo, grupo_causa, name = "n_1") |>
  mutate(pct_1 = prop.table(n_1)) |>

  # Añadir frecuencias paso 2
  left_join(
    recod_defun |>
      count(grupo_causa = paso2, name = "n_2") |>
      mutate(pct_2 = prop.table(n_2))
  ) |>

  # Añadir frecuencias paso 3
  left_join(
    recod_defun |>
      count(grupo_causa = paso3, name = "n_3") |>
      mutate(pct_3 = prop.table(n_3))
  ) |>

  # Añadir frecuencias paso 4
  left_join(
    recod_defun |>
      count(grupo_causa = paso4, name = "n_4") |>
      mutate(pct_4 = prop.table(n_4))
  ) |>

  # Base long
  pivot_longer(cols = n_1:pct_4) |>

  # Separar columnas
  separate_wider_delim(cols = name, names = c("name", "paso"), delim = "_") |>

  # Volver a formato wide
  pivot_wider(names_from = name, values_from = value) |>

  # Crear etiquetas para %
  mutate(pct_lab = percent(pct, accuracy = .1, decimal.mark = ",")) |>

  # Ordenar niveles
  mutate(
    grupo_causa = fct_relevel(grupo_causa, "Otras ENT", after = 4) |>
      fct_relevel("Otras CE", after = 8)
  )


# # Crear etiquetas para frecuencias
# mutate(
#   pct_lab = paste0(
#     grupo_causa,
#     " (",
#     percent(pct, accuracy = .1, decimal.mark = ","),
#     ")"
#   )
# )

## Gráfico -----
datos |>
  ggplot(aes(
    area = pct,
    subgroup = subgrupo,
    fill = grupo_causa,
    label = pct_lab
  )) +

  # Geometrías
  geom_treemap() +

  geom_treemap_subgroup_border() +

  geom_treemap_text(
    place = "center",
    color = "white",
    fontface = "bold",
    size = 9,
    min.size = 2,
    reflow = TRUE
  ) +

  # Escala de colores
  # scale_fill_discrete_c4a_div(palette = "managua", reverse = TRUE) +
  scale_fill_scico_d(
    palette = "managua",
    # direction = -1,
    begin = .1,
    name = ""
  ) +

  # Paneles
  facet_wrap(
    ~paso,
    ncol = 2,
    labeller = as_labeller(c(
      "1" = "Paso 1",
      "2" = "Paso 2",
      "3" = "Paso 3",
      "4" = "Paso 4"
    ))
  ) +

  # Layout
  theme(
    legend.position = "bottom",
    strip.background = element_blank(),
    strip.text = element_text(size = 9, face = "bold"),
    plot.margin = margin(1, 1, 1, 1, "mm"),
    axis.title = element_text(margin = margin(1)),
    panel.spacing = unit(1, "mm")
  ) +
  guides(fill = guide_legend(nrow = 2))


## Guardar gráfico -----
ggsave(
  filename = "Figura4.png",
  width = 18,
  height = 21,
  units = "cm",
  dpi = 300
)


# Figura 5 ---------------------------------------------------------------
## Datos para el gráfico -----
dat_paso <- recod_defun |>
  count(anio, grupo_causa, name = "n_1") |>
  mutate(pct_1 = prop.table(n_1), .by = anio) |>

  # Añadir frecuencias paso 4
  left_join(
    recod_defun |>
      count(anio, grupo_causa = paso4, name = "n_2") |>
      mutate(pct_2 = prop.table(n_2), .by = anio)
  ) |>

  # Diferencia de frecuencias
  mutate(dif_pct = pct_2 - pct_1) |>

  # Quitar NAs
  drop_na() |>

  # Crear subgrupo de causas
  mutate(
    subgrupo = case_when(
      str_detect(grupo_causa, "Dia|ECV|ERC|Neo") ~ "ENT",
      str_detect(grupo_causa, "Hom|Sui|Trá") ~ "CE",
      str_detect(grupo_causa, "GC") ~ "GC",
      .default = "Otras CD"
    )
  )


## Gráfico -----
dat_paso |>
  filter(!subgrupo %in% c("GC", "Otras CD")) |>
  ggplot(aes(x = make_date(anio), y = dif_pct, fill = grupo_causa)) +

  # Geometrías
  geom_col(position = "dodge", color = "grey40") +

  # Escalas ejes
  scale_y_continuous(
    name = "Incremento frecuencia",
    labels = label_percent()
  ) +

  scale_x_date(
    name = "Año",
    date_breaks = "1 year",
    date_labels = "%Y"
  ) +

  # Escalas color
  scale_fill_scico_d(
    name = "",
    palette = "navia",
    begin = .15,
    end = .85
  ) +

  # Facets
  facet_wrap(
    ~subgrupo,
    scales = "free"
  ) +

  # Layout
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "bottom")


## Guardar gráfico ----
ggsave(
  filename = "Figura5.png",
  width = 16,
  height = 18,
  units = "cm",
  dpi = 300
)
