### Mortalidad por enfermedades no transmisibles y causas externas en
### Argentina (2010–2023): redistribución de defunciones por causas garbage
### Análisis exploratorio de datos
### Autora: Tamara Ricardo
### Revisor: Juan I. Irassar
# Última modificación: 30-03-2026 13:56

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
recod_defun <- import("clean/arg_defun_mes_2010-2022_recod.rds")


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


# Tabla 1 ----------------------------------------------------------------
## Frecuencia defunciones por grupo causas y edad
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


# Evolución temporal muertes por GC --------------------------------------
## Dataset para modelar tasas -----
tasa_anio <- recod_defun |>
  # Filtrar muertes por GC
  filter(str_detect(grupo_causa, "GC")) |>
  droplevels() |>

  # Agrupar muertes
  count(anio, jurisdiccion, grupo_edad, sexo, grupo_causa, wt = n) |>

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
  data = tasa_anio
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
  count(anio, grupo_causa, grupo_edad, wt = n) |>

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


### Gráfico
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
    alpha = 0.4
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
      palette = "navia",
      begin = .15,
      end = .85
    ),
    name = NULL
  ) +

  # Layout
  theme_minimal()


### Guardar gráfico ----
ggsave(
  filename = "Figura2.png",
  width = 16,
  height = 14,
  units = "cm",
  dpi = 300
)


# Distribución espacial --------------------------------------------------
## Tasa estandarizada por jurisdicción -----
tasa_esp_st <- shp_arg |>
  left_join(
    recod_defun |>
      # Filtrar muertes por GC
      filter(str_detect(grupo_causa, "GC")) |>
      droplevels() |>

      # Agrupar muertes
      count(jurisdiccion, grupo_causa, grupo_edad, wt = n)
  ) |>

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
  bind_cols(localmoran(
    x = tasa_esp_st$value,
    listw = poly2nb(tasa_esp_st, queen = TRUE) |> nb2listw()
  ))


## Test de Lee -----
### GC1 -----
tasa_esp_st |>
  filter(grupo_causa == "GC1") |>
  (\(x) {
    lee.test(
      x = x$total_count,
      y = x$total_pop,
      listw = poly2nb(x) |> nb2listw(zero.policy = TRUE),
      zero.policy = TRUE
    )
  })()

### GC2 -----
tasa_esp_st |>
  filter(grupo_causa == "GC2") |>
  (\(x) {
    lee.test(
      x = x$total_count,
      y = x$total_pop,
      listw = poly2nb(x) |> nb2listw(zero.policy = TRUE),
      zero.policy = TRUE
    )
  })()

### GC3 -----
tasa_esp_st |>
  filter(grupo_causa == "GC3") |>
  (\(x) {
    lee.test(
      x = x$total_count,
      y = x$total_pop,
      listw = poly2nb(x) |> nb2listw(zero.policy = TRUE),
      zero.policy = TRUE
    )
  })()


### GC4 -----
tasa_esp_st |>
  filter(grupo_causa == "GC4") |>
  (\(x) {
    lee.test(
      x = x$total_count,
      y = x$total_pop,
      listw = poly2nb(x) |> nb2listw(zero.policy = TRUE),
      zero.policy = TRUE
    )
  })()


## Figura 3 -----
### Tasas estandarizadas ------
fig3.1 <- tasa_esp_st |>
  # Mapa
  tm_shape() +
  tm_polygons(
    fill = "value",
    fill.scale = tm_scale_continuous(
      values = "-scico.navia"
    ),
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


### Índice de Moran local (Ii) -----
fig3.2 <- tasa_esp_st |>
  # Mapa
  tm_shape() +
  tm_polygons(
    fill = "Ii",
    fill.scale = tm_scale_continuous(
      values = "-scico.navia"
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


### Unir gráficos -----
tmap_arrange(fig3.1, fig3.2) |>

  tmap_save(
    filename = "Figura3.png",
    width = 16,
    height = 16,
    units = "cm",
    dpi = 300
  )


# Figura 4 ---------------------------------------------------------------
## Frecuencias por paso 1 y paso 2 -----
dat_paso <- recod_defun |>
  count(grupo_causa, wt = n, name = "n_1") |>
  mutate(pct_1 = prop.table(n_1)) |>

  left_join(
    recod_defun |>
      count(grupo_causa = paso2.2, wt = n, name = "n_2") |>
      mutate(pct_2 = prop.table(n_2))
  ) |>

  # Base long
  pivot_longer(
    cols = c(n_1, n_2, pct_1, pct_2)
  ) |>

  # Separar categorías
  separate(name, into = c("name", "paso"), sep = "_") |>

  # Volver a formato wide
  pivot_wider(names_from = name, values_from = value) |>

  # Modificar niveles variables y crear subgrupos
  mutate(
    grupo_causa = str_remove(grupo_causa, " mellitus"),

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
      factor(
        levels = c("ENT", "Otras ENT", "CE", "Otras CE", "GC", "CMNN")
      ),
    .before = grupo_causa
  ) |>

  # Crear etiquetas para frecuencias
  mutate(
    pct_lab = paste0(
      grupo_causa,
      " (",
      percent(pct, accuracy = .1, decimal.mark = ","),
      ")"
    )
  )


## Crear gráfico
dat_paso |>
  ggplot(aes(
    area = pct,
    subgroup = subgrupo1,
    fill = subgrupo2,
    label = pct_lab
  )) +

  # Paneles
  facet_wrap(
    ~paso,
    ncol = 1,
    labeller = as_labeller(c(
      "1" = "Paso 1: Categorización causas de muerte",
      "2" = "Paso 2: Redistribución GC3, GC4 y neumonías inespecíficas"
    ))
  ) +

  # Geometrías
  geom_treemap() +
  geom_treemap_subgroup_border() +
  geom_treemap_text(
    place = "center",
    color = "white",
    fontface = "bold",
    size = 9,
    reflow = TRUE
  ) +

  # Escala de colores
  scale_fill_scico_d(
    palette = "navia",
    begin = .15,
    end = .85,
    name = ""
  ) +

  # Layout
  theme(
    legend.position = "bottom",
    strip.background = element_blank(),
    strip.text = element_text(size = 10, face = "bold")
  ) +
  guides(fill = guide_legend(nrow = 1))


## Guardar gráfico -----
ggsave(
  filename = "Figura4.png",
  width = 16,
  height = 19,
  units = "cm",
  dpi = 300
)

#### SEGUIR DESDE ACÁ#####
