### Mortalidad por enfermedades no transmisibles y causas externas en
### Argentina (2010–2023): redistribución de defunciones por causas garbage
### Limpieza de los dataset:
## - Proyecciones poblacionales por sexo y grupo etario quinquenal, 2010-2023 (INDEC)
## - Población estándar por sexo y grupo etario, Argentina, Censo 2022 (INDEC)
### Autora: Tamara Ricardo
# Última modificación: 14-04-2026 12:52

# Cargar paquetes --------------------------------------------------------
pacman::p_load(
  geoAr,
  zoo,
  rio,
  janitor,
  tidyverse,
  readxl
)


# Cargar datos -----------------------------------------------------------
## Proyecciones poblacionales por provincia (2010-2023) -----
proy_2010_2023_raw <- "raw/c2_proyecciones_prov_2010_2040.xls"


## Población estándar Censo 2022 -----
pob_est_2022_raw <- import("raw/_tmp_60299141.xlsX", range = "B15:E38")


# Crear etiquetas provincia y región -------------------------------------
cod_prov <- show_arg_codes() |>
  # Filtrar totales país
  filter(between(codprov_censo, "02", "94")) |>

  # Cambiar etiqueta CABA
  mutate(prov_nombre = if_else(codprov_censo == "02", id, name_iso)) |>

  # Agrupar provincias poco pobladas en jurisdicciones
  mutate(
    jurisdiccion = case_when(
      str_detect(prov_nombre, "Jujuy|Salta") ~ "NOA1",
      str_detect(prov_nombre, "Cat|Est") ~ "NOA2",
      str_detect(prov_nombre, "San |Rioja") ~ "Cuyo",
      str_detect(prov_nombre, "Pampa|Neu|Río ") ~ "Patagonia Norte",
      str_detect(prov_nombre, "Chubut|Cruz|Fue") ~ "Patagonia Sur",
      .default = prov_nombre
    )
  )


# Limpiar datos proyecciones poblacionales -------------------------------
proy_2010_2023 <- map(
  c("A3:X28", "A31:X56", "A59:H84"),
  ~ excel_sheets(proy_2010_2023_raw)[3:26] |>
    set_names() |>
    map(\(x) read_excel(proy_2010_2023_raw, sheet = x, range = .x)) |>
    list_rbind(names_to = "prov")
) |>
  list_cbind() |>

  # Eliminar columnas vacías
  remove_empty("cols") |>

  # Estandarizar nombres de columnas
  clean_names() |>

  # Seleccionar columnas relevantes
  select(
    codprov_censo = prov_1,
    grupo_edad_5 = edad_2,
    x2010:x25,
    x2016:x50,
    x2022:x59
  ) |>

  # Renombrar columnas
  rename_with(
    .cols = c(x2010:x59),
    .fn = ~ paste0(
      rep(c("Total_", "Masculino_", "Femenino_"), length(.x) / 3),
      rep(2010:2023, each = 3)
    )
  ) |>

  # Filtrar filas con NAs y totales
  filter(!grupo_edad_5 %in% c(NA, "Total")) |>

  # Pasar a formato long
  pivot_longer(cols = c(Total_2010:Femenino_2023)) |>
  # Separar año y sexo
  separate_wider_delim(name, delim = "_", names = c("sexo", "anio")) |>

  # Crear grupo etario ampliado
  mutate(
    grupo_edad = case_when(
      between(grupo_edad_5, "20-24", "35-39") ~ "20-39 años",
      between(grupo_edad_5, "40-44", "45-49") ~ "40-49 años",
      between(grupo_edad_5, "50-54", "55-59") ~ "50-59 años",
      between(grupo_edad_5, "60-64", "65-69") ~ "60-69 años",
      between(grupo_edad_5, "70-74", "75-79") ~ "70-79 años",
      between(grupo_edad_5, "80-74", "95-99") |
        grupo_edad_5 == "100 y más" ~ "≥80 años",
      .default = "<20 años"
    ) |>
      # Ordenar niveles
      fct_relevel("<20 años", after = 0) |>
      fct_relevel("≥80 años", after = Inf)
  ) |>

  # Convertir año y población a numérico
  mutate(across(.cols = c(anio, value), .fns = ~ parse_number(.x))) |>

  # ID de provincia a numérico
  mutate(codprov_censo = str_remove_all(codprov_censo, "[^0-9]")) |>

  # Añadir jurisdicción
  left_join(cod_prov) |>

  # Reagrupar datos
  count(
    anio,
    jurisdiccion,
    grupo_edad,
    sexo,
    wt = value,
    name = "proy_pob"
  )


## Población estándar 2022 ----
pob_est_2022 <- pob_est_2022_raw |>
  # Estandarizar nombres de columnas
  rename(
    grupo_edad_5 = 1,
    Femenino = 2,
    Masculino = 3,
    pob_est_2022 = 4
  ) |>

  # Filtrar filas vacías
  drop_na() |>

  # Crear grupo etario ampliado
  mutate(
    grupo_edad = case_when(
      between(grupo_edad_5, "20 a 24", "35 a 39") ~ "20-39 años",
      between(grupo_edad_5, "40 a 44", "45 a 49") ~ "40-49 años",
      between(grupo_edad_5, "50 a 54", "55 a 59") ~ "50-59 años",
      between(grupo_edad_5, "60 a 64", "65 a 69") ~ "60-69 años",
      between(grupo_edad_5, "70 a 74", "75 a 79") ~ "70-79 años",
      between(grupo_edad_5, "80 a 74", "95 a 99") |
        between(grupo_edad_5, "100 a 104", "105 y más") ~ "≥80 años",
      .default = "<20 años"
    ) |>

      # Ordenar niveles
      fct_relevel("<20 años", after = 0) |>
      fct_relevel("≥80 años", after = Inf)
  ) |>

  # Población a numérico
  mutate(across(.cols = c(Femenino:pob_est_2022), .fns = ~ parse_number(.x))) |>

  # Recalcular poblaciones
  group_by(grupo_edad) |>
  summarise(across(.cols = c(Femenino:pob_est_2022), .fns = ~ sum(.x))) |>

  # Pasar a formato long
  pivot_longer(
    cols = Femenino:Masculino,
    names_to = "sexo",
    values_to = "pob_est_2022_sexo"
  )


# Estimar población mensual ----------------------------------------------
pob_mes_2010_2023 <- proy_2010_2023 |>
  # Expandir dataset
  expand(
    jurisdiccion,
    sexo,
    grupo_edad,
    fecha = seq.Date(
      from = ymd("2010-01-01"),
      to = ymd("2023-12-01"),
      by = "month"
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

  # Agrupar datos
  group_by(jurisdiccion, sexo, grupo_edad) |>

  # Ordenar por fecha
  arrange(fecha) |>

  # Interpolación lineal
  mutate(
    proy_pob_mes = na.approx(proy_pob, x = fecha, na.rm = FALSE)
  ) |>
  ungroup() |>

  # Reordenar columnas
  select(
    anio,
    mes,
    jurisdiccion,
    sexo,
    grupo_edad,
    proy_pob_mes
  )


# Exportar datos limpios -------------------------------------------------
## Población mensual
export(pob_mes_2010_2023, file = "clean/arg_pob_mensual_2010_2023.rds")

## Población estándar 2022
export(pob_est_2022, file = "clean/arg_pob_est_2022.rds")


# Limpiar environment ----------------------------------------------------
rm(list = ls())

pacman::p_unload("all")
