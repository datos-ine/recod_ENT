### Mortalidad por enfermedades no transmisibles y causas externas en
### Argentina (2010–2023): redistribución de defunciones por causas garbage
### Análisis de datos
### Autora: Tamara Ricardo
### Revisor: Juan I. Irassar

# Cargar paquetes --------------------------------------------------------
pacman::p_load(
  # Tasas estandarizadas
  PHEindicatormethods,
  ljr,
  segmented,
  spdep,
  # Mapas
  sf,
  tmap,
  geoAr,
  # Gráficos
  patchwork,
  ggridges,
  treemapify,
  scico,
  # Tablas
  gtsummary,
  # Manejo de datos
  scales,
  rio,
  janitor,
  tidyverse
  # ,
  # update = TRUE
)


# # Función auxiliar para estimar APC y AAPC -------------------------------
# get_apc_segments <- function(model, years) {
#   # Coeficientes del modelo
#   coefs <- model$Coef

#   # Intervalos
#   breaks <- c(min(years), sort(model$Joinpoints), max(years))

#   # Tabla
#   tibble(
#     inicio = head(breaks, -1),
#     fin = tail(breaks, -1),
#     duracion = fin - inicio,
#     beta = cumsum(
#       c(coefs["t"], coefs[str_detect(names(coefs), "max")])
#     ),
#     APC = exp(beta) - 1,
#     AAPC = if_else(
#       inicio == min(years),
#       sum(APC * duracion) / sum(duracion),
#       NA
#     )
#   )
# }

# Configuración tablas gtsummary -----------------------------------------
set_gtsummary_theme(list(
  "style_number-arg:decimal.mark" = ",",
  "style_number-arg:big.mark" = "."
))


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


## Tabla 1 ---------------------------------------------------------------
### Frecuencia defunciones por grupo etario y grupo de causas
recod_defun |>
  # Modificar niveles grupo_causa
  mutate(
    grupo_n1 = if_else(
      str_detect(grupo_n2, "GC"),
      grupo_n2,
      grupo_n1
    ) |>
      # Ordenar niveles
      fct_drop() |>
      fct_relevel("ENT", "CE")
  ) |>

  # Crear tabla
  tbl_cross(
    row = grupo_edad,
    col = grupo_n1,
    percent = "row",
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
## Tasas estandarizadas por año ----
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
  ) |>

  # Ordenar datos
  arrange(grupo_n2, anio) |>

  # Pasar a formato wide
  select(
    anio,
    grupo_causa = grupo_n2,
    tasa = value,
    n = total_count,
    total_pop
  ) |>

  pivot_wider(
    names_from = grupo_causa,
    values_from = c(n, tasa),
    names_glue = "{grupo_causa}_{.value}"
  )


## Regresión joinpoint GC1 -----
m1 <- lm(log(GC1_tasa) ~ anio, data = datos_jp)

# Probar 0 vs 1 joinpoint
pscore.test(m1)

# Probar 1 vs 2 joinpoints
m1.1 <- segmented(m1, seg.Z = ~anio, npsi = 1)

pscore.test(m1.1, more.break = TRUE)

# AAPC
aapc(m1.1)


## Regresión joinpoint GC2 -----
m2 <- lm(log(GC2_tasa) ~ anio, data = datos_jp)

# Probar 0 vs 1 joinpoint
pscore.test(m2)

# Probar 1 vs 2 joinpoints
m2.1 <- segmented(m2, seg.Z = ~anio, npsi = 1)

pscore.test(m2.1, more.break = TRUE)

# AAPC
aapc(m2.1)


## Regresión joinpoint GC3 -----
m3 <- lm(log(GC3_tasa) ~ anio, data = datos_jp)

# Probar 0 vs 1 joinpoint
pscore.test(m3)

# Probar 1 vs 2 joinpoints
m3.1 <- segmented(m3, seg.Z = ~anio, npsi = 1)

pscore.test(m3.1, more.break = TRUE)

# AAPC
aapc(m3.1)


## Regresión joinpoint GC4 -----
m4 <- lm(log(GC4_tasa) ~ anio, data = datos_jp)

# Probar 0 vs 1 joinpoint
pscore.test(m4)

# Probar 1 vs 2 joinpoints
m4.1 <- segmented(m4, seg.Z = ~anio, npsi = 1)

pscore.test(m4.1, more.break = TRUE)

# AAPC
aapc(m4.1)

## Figura 2 --------------------------------------------------------------
### Paleta colorblind-friendly
pal <- scico(n = 4, palette = "managua", begin = .1, end = .9)

### Crear una grilla fina de años
data <- tibble(
  anio = seq(min(datos_jp$anio), max(datos_jp$anio), by = 0.1)
) |>
  # Pendientes GC1
  mutate(fit_m1 = predict(m1.1, newdata = pick(anio))) |>

  # Pendientes GC2
  mutate(fit_m2 = predict(m2.1, newdata = pick(anio))) |>

  # Pendientes GC3
  mutate(fit_m3 = predict(m3.1, newdata = pick(anio))) |>

  # Pendientes GC4
  mutate(fit_m4 = predict(m4.1, newdata = pick(anio)))


### Joinpoint GC1 -----
datos_jp |>
  ggplot(aes(x = anio, y = GC1_tasa)) +
  geom_point(
    size = 3.5,
    color = pal[1]
  ) +

  geom_line(data = data, aes(y = exp(fit_m1))) +
  geom_vline(
    xintercept = m1.1$psi[, "Est."],
    color = "darkgrey",
    linewidth = 1
  ) +
  scale_x_continuous(name = NULL, limits = c(2010, NA), n.breaks = 7) +
  labs(title = "GC1", y = "Tasa estandarizada (100.000 hab.)") +
  theme_minimal()


### Joinpoint GC2 -----
datos_jp |>
  ggplot(aes(x = anio, y = GC2_tasa)) +
  geom_point(
    size = 3.5,
    color = pal[2]
  ) +

  geom_line(data = data, aes(y = exp(fit_m2))) +
  geom_vline(
    xintercept = m2.1$psi[, "Est."],
    color = "darkgrey",
    linewidth = 1
  ) +
  scale_x_continuous(name = NULL, limits = c(2010, NA), n.breaks = 7) +
  labs(title = "GC2", y = "Tasa estandarizada (100.000 hab.)") +
  theme_minimal()


### Joinpoint GC3 -----
datos_jp |>
  ggplot(aes(x = anio, y = GC3_tasa)) +
  geom_point(
    size = 3.5,
    color = pal[3]
  ) +

  geom_line(data = data, aes(y = exp(fit_m3))) +
  geom_vline(
    xintercept = m3.1$psi[, "Est."],
    color = "darkgrey",
    linewidth = 1
  ) +
  scale_x_continuous(name = NULL, limits = c(2010, NA), n.breaks = 7) +
  labs(title = "GC3", y = "Tasa estandarizada (100.000 hab.)") +
  theme_minimal()


### Joinpoint GC4 -----
datos_jp |>
  ggplot(aes(x = anio, y = GC4_tasa)) +
  geom_point(
    size = 3.5,
    color = pal[4]
  ) +

  geom_line(data = data, aes(y = exp(fit_m4))) +
  geom_vline(
    xintercept = m4.1$psi[, "Est."],
    color = "darkgrey",
    linewidth = 1
  ) +
  scale_x_continuous(name = NULL, limits = c(2010, NA), n.breaks = 7) +
  labs(title = "GC4", y = "Tasa estandarizada (100.000 hab.)") +
  theme_minimal()
