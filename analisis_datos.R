### Mortalidad por enfermedades no transmisibles y causas externas en
### Argentina (2010–2023): redistribución de defunciones por causas garbage
### Análisis exploratorio de datos
### Autora: Tamara Ricardo
### Revisor: Juan I. Irassar
# Última modificación: 20-03-2026 11:48

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
  glmmTMB,
  spdep,
  # Manejo de datos
  BAMMtools,
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

  ## Agrupar polígonos
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


# Figura 2: Treeplots por paso -------------------------------------------
## Frecuencias paso 1 -----
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


## Frecuencias paso 2 -----
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


## Unir gráficos
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
    aes(label = paste0(grupo_causa, " (", percent(pct, .1), ")")),
    place = "center",
    color = "white",
    fontface = "bold",
    size = 9,
    reflow = TRUE
  ) &

  # scale_fill_viridis_d(
  #   option = "inferno",
  #   begin = .1,
  #   end = .6,
  #   alpha = .5,
  #   name = ""
  # ) &

  scale_fill_scico_d(
    palette = "navia",
    begin = .15,
    end = .85,
    name = ""
  ) &
  plot_annotation(tag_levels = "A") &
  plot_layout()


## Guardar gráfico
ggsave(
  filename = "Figura2.png",
  width = 16,
  height = 19,
  units = "cm",
  dpi = 300
)

## Cambio de frecuencias ----
fig2.1 |>
  select(grupo_causa, pct1 = pct) |>
  left_join(
    fig2.2 |>
      select(grupo_causa, pct2 = pct)
  ) |>

  mutate(cambio_fr = percent(pct2 - pct1, accuracy = .1))


# Tasas de mortalidad por GC ---------------------------------------------
## Dataset para las tasas -----
dat <- recod_defun |>
  # Filtrar muertes por GC
  filter(str_detect(grupo_causa, "GC")) |>

  # Agrupar muertes
  count(anio, jurisdiccion, grupo_edad, sexo, grupo_causa) |>

  # Añadir proyecciones poblacionales y población estándar
  left_join(proy_pob_anio)


## Tasa mortalidad estandarizada por año -----
tasa_anio <- dat |>
  group_by(anio, grupo_causa) |>
  calculate_dsr(
    x = n,
    n = proy_pob,
    stdpop = pob_est_2022
  )


## Tasa mortalidad estandarizada por jurisdiccion -----
tasa_jur <- shp_arg |>
  left_join(
    dat |>
      group_by(jurisdiccion, grupo_causa) |>
      calculate_dsr(
        x = n,
        n = proy_pob,
        stdpop = pob_est_2022
      )
  )


## Figura 3 -----
### Tasas de mortalidad por año -----
fig3.1 <- tasa_anio |>
  # Año a formato fecha
  mutate(anio = make_date(anio)) |>

  ### Gráfico
  ggplot(aes(
    x = anio,
    y = value,
    color = grupo_causa,
    group = grupo_causa
  )) +

  # Geomas
  geom_line() +
  geom_point() +

  # Escalas
  scale_color_scico_d(
    palette = "navia",
    begin = .15,
    end = .85,
    name = ""
  ) +
  # scale_color_viridis_d(option = "inferno", end = 0.85) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +

  # Layout
  labs(
    x = NULL,
    y = "Tasa est.",
    color = NULL
  ) +
  coord_fixed(ratio = 50) +
  theme_minimal()


### Tasas de mortalidad por jurisdicción -----
fig3.2 <- tasa_jur |>
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
  tm_facets_wrap(by = "grupo_causa", ncol = 2)


### Unir gráficos -----
fig3.1 /
  wrap_elements(full = tmap_grob(fig3.2, asp = 0), clip = FALSE) +
  plot_annotation(tag_levels = "A")


## Guardar gráfico
ggsave(
  filename = "Figura3.png",
  width = 16,
  height = 19,
  units = "cm",
  dpi = 300
)


## Comparar tasas anuales -----
m1 <- glmmTMB(
  n ~ grupo_edad + grupo_causa * anio,
  offset = log(proy_pob),
  family = nbinom2,
  data = dat |> mutate(anio = anio - 2010)
)

model_parameters(m1, exponentiate = TRUE)


## Índice de Moran global -----
moran.test(
  x = tasa_jur$value,
  listw = poly2nb(tasa_jur, queen = TRUE) |> nb2listw()
)

## Índice de Moran local -----
localmoran(
  x = tasa_jur$value,
  listw = poly2nb(tasa_jur, queen = TRUE) |> nb2listw()
)

## Figura S3: índice de Moran local (Ii) -----
tasa_jur |>
  bind_cols(localmoran(
    x = tasa_jur$value,
    listw = poly2nb(tasa_jur, queen = TRUE) |> nb2listw()
  )) |>
  tm_shape() +
  tm_polygons(
    fill = "Ii",
    fill.scale = tm_scale_continuous(
      values = c("#2c7bb6", "#ffffbf", "#d7191c")
    ),
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
  tm_compass() +

  tm_facets_wrap(by = "grupo_causa", ncol = 2)


## Guardar figura
tmap_save(
  filename = "FiguraS3.png",
  width = 16,
  height = 16,
  units = "cm",
  dpi = 300
)
