### Mortalidad por enfermedades no transmisibles y causas externas en
### Argentina (2010–2023): redistribución de defunciones por causas garbage
### Limpieza de los dataset:
## - Proyecciones poblacionales por sexo y grupo etario quinquenal, 2010-2023 (INDEC)
## - Población estándar por sexo y grupo etario, Argentina, Censo 2022 (INDEC)
### Autora: Tamara Ricardo
# Última modificación: 27-03-2026 13:44

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

  # Crear región geográfica DEIS
  mutate(
    region_deis = case_when(
      # Región Centro: CABA, Buenos Aires, Córdoba, Entre Ríos, Santa Fe
      codprov_censo %in% c("02", "06", "14", "30", "82") ~ "Centro",
      # Región NEA: Corrientes, Chaco, Formosa, Misiones
      codprov_censo %in% c("18", "22", "34", "54") ~ "NEA",
      # Región NOA1: Jujuy, Salta
      codprov_censo %in% c("38", "66") ~ "NOA1",
      # Región NOA2: Catamarca, Santiago del Estero, Tucumán
      codprov_censo %in% c("10", "86", "90") ~ "NOA2",
      # Región Cuyo: Mendoza, La Rioja, San Juan, San Luis
      codprov_censo %in% c("46", "50", "70", "74") ~ "Cuyo",
      # Región Patagonia Norte: La Pampa, Neuquén, Río Negro
      codprov_censo %in% c("42", "58", "62") ~ "Patagonia Norte",
      # Región Patagonia Sur: Chubut, Santa Cruz, Tierra del Fuego
      .default = "Patagonia Sur"
    )
  ) |>

  # Agrupar provincias poco pobladas en jurisdicciones
  mutate(
    jurisdiccion = if_else(
      region_deis %in%
        c("NOA1", "NOA2", "Cuyo", "Patagonia Norte", "Patagonia Sur") &
        !prov_nombre %in% c("Mendoza", "Tucumán"),
      region_deis,
      prov_nombre
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

  # Estandarizar nombres de columnas
  clean_names() |>

  # Seleccionar columnas relevantes
  select(
    codprov_censo = prov_1,
    grupo_edad_5 = edad_2,
    x4,
    x5,
    x8,
    x9,
    x12,
    x13,
    x16,
    x17,
    x20,
    x21,
    x24,
    x25,
    x29,
    x30,
    x33,
    x34,
    x37,
    x38,
    x41,
    x42,
    x45,
    x46,
    x49,
    x50,
    x54,
    x55,
    x58,
    x59
  ) |>

  # Renombrar columnas
  rename_with(
    .cols = c(x4:x59),
    .fn = ~ paste0(
      rep(c("Masculino_", "Femenino_"), length(.x) / 2),
      rep(2010:2023, each = 2)
    )
  ) |>

  # Crear id numérico de provincia
  mutate(codprov_censo = str_sub(codprov_censo, 1, 2)) |>

  # Filtrar filas con NAs
  drop_na() |>

  # Filtrar totales
  filter(grupo_edad_5 != "Total") |>

  # Pasar a formato long
  pivot_longer(cols = c(Masculino_2010:Femenino_2023)) |>

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
    )
  ) |>

  # Convertir año y población a numérico
  mutate(across(.cols = c(anio, value), .fns = ~ parse_number(.x))) |>

  # Añadir etiquetas provincia y región DEIS
  left_join(cod_prov) |>

  # Reagrupar datos
  count(
    anio,
    region_deis,
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

  # Filtrar menores de 20 años
  filter(
    between(grupo_edad_5, "20 a 24", "95 a 99") |
      grupo_edad_5 %in% c("100 a 104", "105 y más")
  ) |>

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
    )
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
pob_mes_2010_2022 <- proy_2010_2023 |>
  # Expandir dataset
  expand(
    nesting(region_deis, jurisdiccion),
    sexo,
    grupo_edad,
    fecha = seq.Date(
      from = ymd("2010-01-01"),
      to = ymd("2023-12-01"),
      by = "month"
    )
  ) |>

  # Crear columna para el año
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
  group_by(region_deis, jurisdiccion, sexo, grupo_edad) |>

  # Ordenar por fecha
  arrange(fecha) |>

  # Interpolación lineal
  mutate(
    proy_pob_mes = na.approx(proy_pob, x = fecha, na.rm = FALSE)
  ) |>
  ungroup() |>

  # Añadir población estándar 2022
  left_join(pob_est_2022) |>

  # Reordenar columnas
  select(
    anio,
    mes,
    region_deis,
    jurisdiccion,
    sexo,
    grupo_edad,
    contains("pob")
  )


# Exportar datos limpios -------------------------------------------------
export(pob_mes_2010_2022, file = "clean/arg_pob_mensual_2010_2023.rds")


# Limpiar environment ----------------------------------------------------
rm(list = ls())

pacman::p_unload("all")
