### Mortalidad por enfermedades no transmisibles y causas externas en
### Argentina (2010–2023): redistribución de defunciones por causas garbage
### Análisis exploratorio de datos
### Autora: Tamara Ricardo
### Revisor: Juan I. Irassar
# Última modificación: 27-03-2026 13:56

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
  tidyverse,
  update = TRUE
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
  select(-contains("mes"), -region_deis)


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


# Figura 2 ---------------------------------------------------------------
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
  filename = "Figura2.png",
  width = 16,
  height = 19,
  units = "cm",
  dpi = 300
)


# Evolución temporal muertes por GC --------------------------------------
## Dataset para las tasas -----
tasa_anio <- recod_defun |>
  # Filtrar muertes por GC
  filter(str_detect(grupo_causa, "GC")) |>
  droplevels() |>

  # Agrupar muertes
  count(anio, jurisdiccion, grupo_edad, sexo, grupo_causa, wt = n) |>

  # Estandarizar año
  mutate(anio_st = anio - 2010, .after = anio) |>

  # Añadir población
  left_join(proy_pob_anio)


## GLMM binomial negativo -----
# Modelo con interacción año:grupo_causa
m1 <- glmmTMB(
  n ~ grupo_causa * anio_st + (1 | grupo_edad),
  offset = log(proy_pob),
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


# Figura 3 ---------------------------------------------------------------
## Tasas estandarizadas por año -----
tasa_anio_st <- tasa_anio |>
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
    alpha = 0.3
  ) +

  geom_line() +

  geom_text_repel(
    aes(label = value_ci),
    vjust = c(rep(-1, 52), rep(2, 4)),
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
    limits = c(0, 125),
    n.breaks = 7,
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
  filename = "Figura3.png",
  width = 16,
  height = 14,
  units = "cm",
  dpi = 300
)


#### SEGUIR DESDE ACÁ #####
# Distribución espacial --------------------------------------------------
## Tasa estandarizada por jurisdicción -----
tasa_esp <- shp_arg |>
  left_join(
    tasa_anio |>
      bind_rows(
        tasa_anio |>
          count(
            anio,
            jurisdiccion,
            grupo_edad,
            sexo,
            anio_st,
            proy_pob,
            pob_est_2022,
            wt = n
          ) |>
          mutate(grupo_causa = "Total")
      ) |>

      # Estandarizar tasa
      group_by(jurisdiccion, grupo_causa) |>
      calculate_dsr(
        x = n,
        n = proy_pob,
        stdpop = pob_est_2022
      )
  )


## Test de Moran global -----
### GC totales -----
tasa_esp |>
  filter(grupo_causa == "Total") |>
  (\(df) {
    moran.test(
      x = df$value,
      listw = poly2nb(df) |> nb2listw(zero.policy = TRUE),
      zero.policy = TRUE
    )
  })()


### GC1 -----
tasa_esp |>
  filter(grupo_causa == "GC1") |>
  (\(df) {
    moran.test(
      x = df$value,
      listw = poly2nb(df) |> nb2listw(zero.policy = TRUE),
      zero.policy = TRUE
    )
  })()


### GC2 -----
tasa_esp |>
  filter(grupo_causa == "GC2") |>
  (\(df) {
    moran.test(
      x = df$value,
      listw = poly2nb(df) |> nb2listw(zero.policy = TRUE),
      zero.policy = TRUE
    )
  })()


### GC3 -----
tasa_esp |>
  filter(grupo_causa == "GC3") |>
  (\(df) {
    moran.test(
      x = df$value,
      listw = poly2nb(df) |> nb2listw(zero.policy = TRUE),
      zero.policy = TRUE
    )
  })()


### GC4 -----
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


## Test de Lee -----
### GC totales -----
tasa_esp |>
  filter(grupo_causa == "Total") |>
  (\(x) {
    lee.test(
      x = x$total_count,
      y = x$total_pop,
      listw = poly2nb(x) |> nb2listw(zero.policy = TRUE),
      zero.policy = TRUE
    )
  })()


### GC1 -----
tasa_esp |>
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
tasa_esp |>
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
tasa_esp |>
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
tasa_esp |>
  filter(grupo_causa == "GC4") |>
  (\(x) {
    lee.test(
      x = x$total_count,
      y = x$total_pop,
      listw = poly2nb(x) |> nb2listw(zero.policy = TRUE),
      zero.policy = TRUE
    )
  })()


## Figura 4 -----
### Población x jurisdicción -----
fig4.1 <- tasa_esp |>
  # Mapa
  tm_shape() +
  tm_polygons(
    fill = "total_pop",
    fill.scale = tm_scale_continuous(
      values = "-scico.navia",
      limits = c(min(tasa_esp$total_pop), max(tasa_esp$total_pop) + 10)
    ),
    fill_alpha = .75,
    fill.legend = tm_legend(
      position = tm_pos_out(),
      frame = FALSE,
      bg = FALSE,
      height = 10,
      margins = c(1, 1, 2, -1),
      title = "N"
    )
  ) +

  # Coordenadas
  tm_grid(lines = FALSE, n.y = 4, n.x = 5, ) +

  # Flecha del Norte
  tm_compass(position = tm_pos_out(pos.v = "bottom")) +

  # Layout
  tm_title("Población por jurisdicción", size = 1)


### Tasas estandarizadas ------
fig4.1 <- tasa_esp |>
  # Mapa
  tm_shape() +
  tm_polygons(
    fill = "value",
    fill.scale = tm_scale_continuous(
      values = "-scico.navia",
      limits = c(0, max(tasa_esp$value) + 10)
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
fig4.2 <- tasa_esp |>
  # Mapa
  tm_shape() +
  tm_polygons(
    fill = "Ii",
    fill.scale = tm_scale_continuous(
      values = "-scico.navia",
      limits = c(min(tasa_esp$Ii), max(tasa_esp$Ii) + 1)
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
tmap_arrange(fig4.1, fig4.2) |>

  tmap_save(
    filename = "Figura4.png",
    width = 16,
    height = 16,
    units = "cm",
    dpi = 300
  )


## Figura S2 -----
### Población -----
figs2.1 <- tasa_esp |>
  # Mapa
  tm_shape() +
  tm_polygons(
    fill = "total_pop",
    fill.scale = tm_scale_continuous(
      values = "-scico.navia",
      limits = c(min(tasa_esp$total_pop), max(tasa_esp$total_pop) + 10)
    ),
    fill_alpha = .75,
    fill.legend = tm_legend(
      position = tm_pos_out(),
      frame = FALSE,
      bg = FALSE,
      height = 10,
      margins = c(1, 1, 1, -1),
      title = "N"
    )
  ) +

  # Coordenadas
  tm_grid(lines = FALSE, n.y = 4, n.x = 5, ) +

  # Flecha del Norte
  tm_compass(position = tm_pos_out(pos.v = "bottom")) +

  # Layout
  tm_facets_wrap(by = "grupo_causa", nrow = 1) +
  tm_title("Población por jurisdicción", size = 1)


### Muertes por GC -----
figs2.2 <- tasa_esp |>
  # Mapa
  tm_shape() +
  tm_polygons(
    fill = "total_count",
    fill.scale = tm_scale_continuous(
      values = "-scico.navia",
      limits = c(min(tasa_esp$total_count), max(tasa_esp$total_count) + 10)
    ),
    fill_alpha = .75,
    fill.legend = tm_legend(
      position = tm_pos_out(),
      frame = FALSE,
      bg = FALSE,
      height = 10,
      margins = c(1, 1, 1, -1),
      title = "N"
    )
  ) +

  # Coordenadas
  tm_grid(lines = FALSE, n.y = 4, n.x = 5, ) +

  # Flecha del Norte
  tm_compass(position = tm_pos_out(pos.v = "bottom")) +

  # Layout
  tm_facets_wrap(by = "grupo_causa", nrow = 1) +
  tm_title("Número de muertes por GC", size = 1)


### Unir gráficos -----
tmap_arrange(figs2.1, figs2.2, fig4.1, fig4.2) |>

  tmap_save(
    filename = "FiguraS2.png",
    width = 16,
    height = 20,
    units = "cm",
    dpi = 300
  )
