### Mortalidad por enfermedades no transmisibles y causas externas en
### Argentina (2010–2023): redistribución de defunciones por causas garbage
### Limpieza de los dataset:
## - Proyecciones poblacionales por sexo y grupo etario quinquenal, 2010-2023 (INDEC)
## - Población estándar por sexo y grupo etario, Argentina, Censo 2022 (INDEC)
### Autora: Tamara Ricardo
# Última modificación: 15-04-2026 10:36

# Cargar paquetes --------------------------------------------------------
pacman::p_load(
  zoo,
  rio,
  janitor,
  tidyverse,
  readxl
)


# Cargar datos -----------------------------------------------------------
## Población estándar Censo 2022 -----
pob_est_2022_raw <- import("raw/_tmp_3661501.xlsX", range = "B14:C36")

## Proyecciones poblacionales por provincia (2010-2023) -----
proy_2010_2023_raw <- "raw/c2_proyecciones_prov_2010_2040.xls"


# Limpiar datos ----------------------------------------------------------
## Población estándar 2022 -----
pob_est_2022 <- pob_est_2022_raw |>
  # Estamdarizar nombres de columnas
  clean_names() |>
  rename(grupo_edad_5 = 1) |>

  # Crear grupo etario ampliado
  mutate(
    grupo_edad = case_when(
      between(grupo_edad_5, "00 a 04", "10 a 14") |
        grupo_edad_5 == "15 a 19" ~ "<20 años",
      between(grupo_edad_5, "20 a 24", "35 a 39") ~ "20-39 años",
      between(grupo_edad_5, "40 a 44", "45 a 49") ~ "40-49 años",
      between(grupo_edad_5, "50 a 54", "55 a 59") ~ "50-59 años",
      between(grupo_edad_5, "60 a 64", "65 a 69") ~ "60-69 años",
      between(grupo_edad_5, "70 a 74", "75 a 79") ~ "70-79 años",
      .default = "≥80 años"
    ) |>

      # Ordenar niveles
      fct_relevel("<20 años", after = 0) |>
      fct_relevel("≥80 años", after = Inf)
  ) |>

  # Reagrupar datos
  count(grupo_edad, wt = casos, name = "pob_est_2022")


## Proyecciones poblacionales 2010-2023 -----
proy_2010_2023 <- map(
  .x = c("A3:X28", "A31:X56", "A59:L84"),
  .f = ~ excel_sheets(proy_2010_2023_raw)[3:26] |>
    set_names() |>
    # Leer hojas
    map(\(x) read_excel(proy_2010_2023_raw, sheet = x, range = .x)) |>

    # Unir hojas de provincias
    list_rbind(names_to = "prov") |>

    # Quitar columnas vacías
    remove_empty("cols")
) |>

  # Unir datasets
  list_cbind() |>

  # Estandarizar nombres de columnas
  clean_names() |>

  # Descartar columnas innecesarias
  select(-prov_21, -edad_22, -prov_41, -edad_42) |>

  # Renombrar columnas
  rename_with(
    .cols = x2010:x51,
    .fn = ~ paste(
      rep(c("Total", "Masculino", "Femenino"), length(.x) / 3),
      rep(2010:2024, each = 3),
      sep = "_"
    )
  ) |>

  # Descartar filas con NAs y totales
  filter_out(edad_2 %in% c("Total", NA)) |>

  # Crear jurisdicción DEIS
  mutate(
    jurisdiccion = case_when(
      str_detect(prov_1, "JUJUY|SALTA") ~ "NOA1",
      str_detect(prov_1, "CATAMARCA|ESTERO") ~ "NOA2",
      str_detect(prov_1, "SAN |RIOJA") ~ "Cuyo",
      str_detect(prov_1, "PAMPA|NEUQUÉN|NEGRO") ~ "Patagonia Norte",
      str_detect(prov_1, "CHUBUT|SANTA CRUZ|FUEGO") ~ "Patagonia Sur",
      prov_1 == "02-CABA" ~ "CABA",
      .default = str_sub(prov_1, 4) |>
        str_to_title()
    )
  ) |>

  # Crear grupo etario ampliado
  mutate(
    grupo_edad = case_when(
      edad_2 %in% c("0-4", "5-9", "10-14", "15-19") ~ "<20 años",
      between(edad_2, "20-24", "35-39") ~ "20-39 años",
      between(edad_2, "40-44", "45-49") ~ "40-49 años",
      between(edad_2, "50-54", "55-59") ~ "50-59 años",
      between(edad_2, "60-64", "65-69") ~ "60-69 años",
      between(edad_2, "70-74", "75-79") ~ "70-79 años",
      .default = "≥80 años"
    ) |>

      # Ordenar niveles
      fct_relevel("<20 años", after = 0) |>
      fct_relevel("≥80 años", after = Inf)
  ) |>

  # Pasar a formato long
  pivot_longer(cols = Total_2010:Femenino_2024) |>

  # Separar sexo y año
  separate_wider_delim(cols = name, delim = "_", names = c("sexo", "anio")) |>

  # Agrupar datos
  count(
    jurisdiccion,
    anio = parse_number(anio),
    sexo,
    grupo_edad,
    wt = parse_number(value),
    name = "proy"
  )


# Estimar población mensual ----------------------------------------------
proy_mes_2010_2023 <- proy_2010_2023 |>
  # Expandir dataset
  expand(
    jurisdiccion,
    sexo,
    grupo_edad,
    fecha = seq(
      from = ymd("2010-01-01"),
      to = ymd("2023-12-01"),
      by = "1 month"
    )
  ) |>

  # Crear columna para mes y año
  mutate(
    anio = year(fecha),
    mes = month(fecha)
  ) |>

  # Unir con base población anual
  left_join(
    proy_2010_2023 |>
      mutate(fecha = make_date(anio))
  ) |>

  # Interpolación lineal
  mutate(
    proy_pob = na.approx(proy, x = fecha, na.rm = FALSE),
    .by = c(jurisdiccion, sexo, grupo_edad)
  ) |>

  # Eliminar filas 2024
  filter_out(anio == 2024) |>

  # Ordenar columnas
  select(anio, mes, jurisdiccion, sexo, grupo_edad, proy_pob)


# Exportar datos limpios -------------------------------------------------
## Proyecciones poblacionales mensuales
export(proy_mes_2010_2023, file = "clean/arg_proy_mensual_2010_2023.rds")

## Población estándar 2022
export(pob_est_2022, file = "clean/arg_pob_est_2022.rds")


# Limpiar environment ----------------------------------------------------
rm(list = ls())

pacman::p_unload("all")
