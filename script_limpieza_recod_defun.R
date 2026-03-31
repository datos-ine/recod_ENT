### Mortalidad por enfermedades no transmisibles y causas externas en
### Argentina (2010–2023): redistribución de defunciones por causas garbage
### Limpieza del dataset: Defunciones Generales Mensuales ocurridas y registradas en la
### República Argentina (MSAL-DEIS, 2010-2023)
### Recategorización de causas de muerte, reasignación y redistribución de códigos garbage
### según Teixeira et al. (2021), Soares Filho et al. (2024) y GBD (2019)
### Autora: Tamara Ricardo
### Revisor: Juan I. Irassar
# Última modificación: 31-03-2026 10:18

# Cargar paquetes --------------------------------------------------------
pacman::p_load(
  rio,
  janitor,
  tidyverse
)


# Cargar datos defunciones mensuales (2010-2023) -------------------------
defun_raw <- bind_rows(
  # Período 2010-2015
  import("raw/base_def_10_15_men_4dig.csv"),
  # Período 2016-2021
  import("raw/base_def_16_20_men_4dig.csv"),
  # Período 2022-2023
  import("raw/base_def_21_23_men_4dig.csv")
)


## Explorar datos -----
# Datos fuera período de estudio
tabyl(defun_raw, anio_def) |>
  adorn_pct_formatting(digits = 2)

# Datos ausentes región geográfica
defun_raw |>
  filter(between(anio_def, 2010, 2023)) |>
  tabyl(region) |>
  adorn_pct_formatting()

# Datos ausentes grupo etario
defun_raw |>
  filter(between(anio_def, 2010, 2023) & !str_detect(region, "10.")) |>
  tabyl(grupo_etario) |>
  adorn_pct_formatting(digits = 2)

# Datos ausentes sexo
defun_raw |>
  filter(
    between(anio_def, 2010, 2023) &
      !str_detect(region, "10.") &
      !grupo_etario == "08.sin especificar"
  ) |>
  tabyl(sexo_id) |>
  adorn_pct_formatting(digits = 2)


# Limpiar datos defunciones mensuales (2010-2022) ------------------------
defun <- defun_raw |>
  # Estandarizar nombres de columnas
  rename(
    anio = anio_def,
    mes = mes_def,
    sexo = sexo_id,
    grupo_edad = grupo_etario,
    region_deis = region,
    cie10_cod = cod_causa_muerte_CIE10
  ) |>

  # Filtrar defunciones 2009
  filter(between(anio, 2010, 2023)) |>

  # Filtrar datos ausentes región geográfica
  filter(region_deis != "10.sin especificar.") |>

  # Filtrar datos ausentes grupo etario
  filter(grupo_edad != "08.sin especificar") |>

  # Filtrar datos ausentes sexo
  filter(between(sexo, 1, 2)) |>

  # Estandarizar formato códigos CIE-10
  mutate(cie10_cod = str_to_upper(cie10_cod)) |>

  # Añadir separador de 4to dígito código CIE-10
  mutate(
    cie10_cod = if_else(
      str_detect(cie10_cod, "X$"),
      str_replace(cie10_cod, "X$", ".0"),
      paste0(str_sub(cie10_cod, 1, 3), ".", str_sub(cie10_cod, 4))
    )
  ) |>

  # Modificar etiquetas sexo
  mutate(sexo = if_else(sexo == 1, "Masculino", "Femenino")) |>

  # Modificar etiquetas grupo etario
  mutate(
    grupo_edad = fct_relabel(
      grupo_edad,
      .fun = ~ c(
        "<20 años",
        "20-39 años",
        "40-49 años",
        "50-59 años",
        "60-69 años",
        "70-79 años",
        "≥80 años"
      )
    )
  ) |>

  # Modificar etiquetas región DEIS
  mutate(
    region_deis = case_when(
      str_detect(region_deis, "Cuyo") ~ "Cuyo",
      str_detect(region_deis, "8.Pat.") ~ "Patagonia Norte",
      str_detect(region_deis, "9.Pat.") ~ "Patagonia Sur",
      .default = str_sub(region_deis, 3) |> str_remove("\\.")
    )
  ) |>

  # Modificar etiquetas jurisdicción
  mutate(
    jurisdiccion = case_when(
      str_detect(jurisdiccion, "6") ~ "Buenos Aires",
      str_detect(jurisdiccion, "14") ~ "Córdoba",
      str_detect(jurisdiccion, "30") ~ "Entre Ríos",
      str_detect(jurisdiccion, "90") ~ "Tucumán",
      str_detect(jurisdiccion, "99") ~ region_deis,
      .default = str_remove_all(jurisdiccion, "[0-9[:punct:]]")
    )
  ) |>

  # Reordenar columnas
  select(anio, mes, region_deis, jurisdiccion, sexo, grupo_edad, cie10_cod)


# Paso 1: Agrupar causas objetivo ----------------------------------------
## Diabetes mellitus ------
recod_defun <- defun |>
  mutate(
    paso1 = case_when(
      ### DM1 y DM2
      between(cie10_cod, "E10.0", "E10.1") |
        between(cie10_cod, "E10.3", "E11.1") |
        between(cie10_cod, "E11.3", "E11.9") |
        cie10_cod == "P70.2" ~ "Diabetes",

      ### GC3: Diabetes mellitus
      # E08: No existe

      ### GC4: Diabetes mellitus
      between(cie10_cod, "E12.0", "E12.1") |
        between(cie10_cod, "E12.3", "E13.1") |
        between(cie10_cod, "E13.3", "E14.1") |
        between(cie10_cod, "E14.3", "E14.9") |
        between(cie10_cod, "R73.0", "R73.9") ~ "GC4-Diabetes",

      ### Valor por defecto
      .default = NA
    )
  )


## Enfermedades cardiovasculares (ECV) -----
recod_defun <- recod_defun |>
  mutate(
    paso1 = case_when(
      ### Enfermedades cardiovasculares
      cie10_cod == "B33.2" |
        between(cie10_cod, "G45.0", "G46.8") |
        between(cie10_cod, "I01.0", "I02.0") |
        between(cie10_cod, "I05.0", "I09.9") |
        between(cie10_cod, "I11.0", "I11.9") |
        between(cie10_cod, "I20.0", "I25.9") |
        cie10_cod %in% c("I27.0", "I27.2") |
        between(cie10_cod, "I28.0", "I28.9") |
        between(cie10_cod, "I30.0", "I31.1") |
        between(cie10_cod, "I31.8", "I37.8") |
        between(cie10_cod, "I38.0", "I41.8") |
        # I41.9: No existe
        between(cie10_cod, "I42.1", "I42.8") |
        between(cie10_cod, "I43.0", "I43.8") |
        # I43.9: No existe
        between(cie10_cod, "I47.0", "I48.9") |
        between(cie10_cod, "I51.0", "I51.4") |
        between(cie10_cod, "I60.0", "I63.9") |
        between(cie10_cod, "I65.0", "I67.3") |
        between(cie10_cod, "I67.5", "I67.6") |
        between(cie10_cod, "I68.0", "I68.2") |
        between(cie10_cod, "I69.0", "I69.3") |
        between(cie10_cod, "I70.2", "I70.8") |
        between(cie10_cod, "I71.0", "I73.9") |
        between(cie10_cod, "I77.0", "I83.9") |
        between(cie10_cod, "I86.0", "I89.0") |
        cie10_cod %in% c("I89.9", "K75.1") ~ "ECV",

      ### GC3: Enfermedades cardiovasculares
      cie10_cod %in%
        c("I00.0") |
        # I03 - I04: No existe
        # I14: No existe
        # I16 - I19: No existe
        # I29: No existe
        between(cie10_cod, "I44.0", "I45.9") |
        # I44.8 - I44.9: No existe
        between(cie10_cod, "I49.0", "I49.9") |
        between(cie10_cod, "I51.6", "I52.8") |
        # I53 - I59: No existe
        # I90 - I94: No existe
        # I96: No existe
        # I98.4: No existe
        cie10_cod %in% c("I98.8", "I99.0") ~ "GC3-ECV",

      ### GC4: Enfermedades cardiovasculares
      cie10_cod %in%
        c(
          "I37.9",
          "I42.0",
          "I42.9",
          "I51.5",
          "I64.0", # I64: No tiene decimales
          "I67.8",
          "I67.9",
          "I68.8"
        ) |
        between(cie10_cod, "I69.4", "I69.8") ~
        # I69.9: No existe
        "GC4-ECV",

      ### Valor por defecto
      .default = paso1
    )
  )


## Enfermedades respiratorias crónicas (ERC) -----
recod_defun <- recod_defun |>
  mutate(
    paso1 = case_when(
      ### Enfermedades respiratorias crónicas
      cie10_cod %in%
        c("D86.0", "D86.1", "D86.2", "D86.9", "G47.3") |
        between(cie10_cod, "J30.0", "J35.9") |
        between(cie10_cod, "J37.0", "J39.9") |
        between(cie10_cod, "J41.0", "J46.0") | # J46: No tiene decimales
        between(cie10_cod, "J60.0", "J63.8") |
        between(cie10_cod, "J65.0", "J68.9") |
        cie10_cod %in% c("J70.8", "J70.9", "J82.0") |
        between(cie10_cod, "J84.0", "J84.9") |
        between(cie10_cod, "J92.0", "J92.9") ~
        "ERC",

      ### GC3: Enfermedades respiratorias crónicas
      cie10_cod %in%
        c(
          "J40.0", # J40: No tiene decimales
          "J47.0",
          # J48 - J59: No existe
          "J65.0",
          # J71 - J79: No existe
          # J81.9: No existe
          # J83: No existe
          # J85.9: No existe
          # J87 - J89: No existe
          # J90.9: No existe
          # J93.6: No existe
          # J97: No existe
          "J98.0"
        ) |
        between(cie10_cod, "J98.4", "J99.8") ~ "GC3-ERC",

      ### GC4: Enfermedades respiratorias crónicas
      cie10_cod == "J64.0" ~ # J64: No tiene decimales
        "GC4-ERC",

      .default = paso1
    )
  )


## Neoplasias -----
recod_defun <- recod_defun |>
  mutate(
    paso1 = case_when(
      ### Neoplasias malignas
      between(cie10_cod, "C00.0", "C13.9") |
        between(cie10_cod, "C15.0", "C22.8") |
        between(cie10_cod, "C23.0", "C25.9") |
        between(cie10_cod, "C30.0", "C34.9") |
        between(cie10_cod, "C37.0", "C38.8") |
        between(cie10_cod, "C40.0", "C41.9") |
        between(cie10_cod, "C43.0", "C45.9") |
        between(cie10_cod, "C47.0", "C54.9") |
        between(cie10_cod, "C56.0", "C57.8") |
        cie10_cod == "C58.0" |
        between(cie10_cod, "C60.0", "C63.8") |
        between(cie10_cod, "C64.0", "C68.8") |
        between(cie10_cod, "C69.0", "C69.8") |
        between(cie10_cod, "C70.0", "C73.0") | # C73: No tiene decimales
        between(cie10_cod, "C75.0", "C75.8") |
        between(cie10_cod, "C81.0", "C83.8") |
        between(cie10_cod, "C84.0", "C85.0") |
        between(cie10_cod, "C85.2", "C85.7") |
        # C85.8: No existe
        between(cie10_cod, "C86.0", "C86.6") |
        between(cie10_cod, "C88.0", "C91.0") |
        cie10_cod %in% c("C91.2", "C91.3", "C91.6") |
        between(cie10_cod, "C92.0", "C92.6") |
        cie10_cod %in% c("C93.0", "C93.1", "C93.3") |
        # C93.8: No existe
        between(cie10_cod, "C94.0", "C94.5") |
        between(cie10_cod, "C94.7", "C96.9") |

        ### Neoplasias in situ/benignas
        between(cie10_cod, "D00.1", "D01.3") |
        between(cie10_cod, "D02.0", "D02.3") |
        between(cie10_cod, "D03.0", "D07.2") |
        cie10_cod %in% c("D07.4", "D07.5", "D09.0", "D09.2", "D09.3") |
        # D09.8: No existe
        between(cie10_cod, "D10.0", "D10.7") |
        between(cie10_cod, "D11.0", "D13.7") |
        between(cie10_cod, "D14.0", "D14.3") |
        between(cie10_cod, "D15.0", "D16.9") |
        between(cie10_cod, "D22.0", "D24.0") | # D24: No tiene decimales
        between(cie10_cod, "D26.0", "D28.1") |
        cie10_cod == "D28.7" |
        between(cie10_cod, "D29.0", "D29.8") |
        between(cie10_cod, "D30.0", "D30.7") |
        # D30.8: No existe
        between(cie10_cod, "D31.0", "D35.9") |
        between(cie10_cod, "D36.1", "D36.7") |
        between(cie10_cod, "D37.1", "D37.5") |
        between(cie10_cod, "D38.0", "D38.5") |
        between(cie10_cod, "D39.1", "D39.2") |
        # D39.8: No existe
        between(cie10_cod, "D40.0", "D40.7") |
        # D40.8: No existe
        between(cie10_cod, "D41.0", "D41.7") |
        # D41.8: No existe
        between(cie10_cod, "D42.0", "D44.8") |
        between(cie10_cod, "D45.0", "D48.6") |
        cie10_cod %in% c("K62.0", "K62.1", "K63.5") |
        between(cie10_cod, "N60.0", "N60.9") |
        between(cie10_cod, "N84.0", "N84.1") |
        between(cie10_cod, "N87.0", "N87.9") ~ "Neoplasias",

      ### GC3: Neoplasias
      between(cie10_cod, "C14.0", "C14.8") |
        # C14.9: No existe
        cie10_cod == "C22.9" |
        between(cie10_cod, "C26.0", "C26.9") |
        # C26.2: No existe
        # C27 - C29: No existe
        # C35 - C36: No existe
        between(cie10_cod, "C39.0", "C39.9") |
        # C42: No existe
        between(cie10_cod, "C46.0", "C46.9") |
        cie10_cod %in%
          c("C55.0", "C57.9", "C63.9", "C68.9") | # C55: No tiene decimales
        # C59: No existe
        between(cie10_cod, "C74.0", "C74.9") |
        between(cie10_cod, "C75.9", "C80.9") |
        # C76.9: No existe
        # C80.1 - C80.2: No existe
        cie10_cod %in% c("C83.9", "C85.1", "C85.9", "C94.6") |
        # C87: No existe
        between(cie10_cod, "C97.0", "D00.0") |
        # C97.9 - C99: No existe
        between(cie10_cod, "D01.4", "D01.9") |
        cie10_cod %in%
          c(
            "D02.4",
            # D02.5 - D02.9: No existe
            "D07.3",
            "D07.6",
            # D08: No existe
            "D09.1",
            "D09.7",
            "D09.9",
            "D10.9",
            "D13.9",
            "D14.4"
          ) |
        between(cie10_cod, "D17.0", "D21.9") |
        # D20.9: No existe
        cie10_cod %in% c("D28.9", "D29.9", "D30.9", "D36.0", "D36.9", "D37.0") |
        between(cie10_cod, "D37.6", "D37.9") |
        between(cie10_cod, "D38.6", "D39.0") |
        cie10_cod %in%
          c(
            "D39.7",
            "D39.9",
            "D40.9",
            "D41.9",
            "D44.9",
            "D48.7",
            "D48.8",
            "D48.9"
            # D49: No existe
            # D54: No existe
          ) |
        between(cie10_cod, "N84.2", "N84.8") ~ "GC3-Neoplasias",

      ### GC4: Neoplasias
      cie10_cod %in%
        c(
          "C69.9",
          "C91.1",
          "C91.4",
          "C91.5",
          "C91.7",
          "C91.8",
          "C91.9",
          "C92.7",
          "C92.8",
          "C92.9",
          "C93.2",
          "C93.5",
          "C93.6",
          "C93.7",
          "C93.9"
        ) ~ "GC4-Neoplasias",

      ### Valor por defecto
      .default = paso1
    )
  )


## Accidentes de tránsito -----
recod_defun <- recod_defun |>
  mutate(
    paso1 = case_when(
      ### Accidentes de tránsito
      between(cie10_cod, "V01.0", "V04.9") |
        between(cie10_cod, "V06.0", "V80.9") |
        between(cie10_cod, "V82.0", "V82.9") |
        between(cie10_cod, "V87.2", "V87.3") ~
        "Tránsito",

      ### GC4: Accidentes de tránsito
      between(cie10_cod, "V87.0", "V87.1") |
        between(cie10_cod, "V87.4", "V87.9") |
        between(cie10_cod, "V89.0", "V89.9") ~ "GC4-Tránsito",

      ### Valor por defecto
      .default = paso1
    )
  )


## Suicidio -----
recod_defun <- recod_defun |>
  mutate(
    paso1 = case_when(
      ### Suicidio
      between(cie10_cod, "X60.0", "X64.9") |
        between(cie10_cod, "X66.0", "X83.9") |
        cie10_cod == "Y87.0" ~ "Suicidio",

      ### GC4: Suicidio
      between(cie10_cod, "X84.0", "X84.9") ~ "GC4-Suicidio",

      ### Valor por defecto
      .default = paso1
    )
  )


## Homicidio ------
recod_defun <- recod_defun |>
  mutate(
    paso1 = case_when(
      ### Homicidio
      between(cie10_cod, "X85.0", "Y08.9") |
        cie10_cod == "Y87.1" ~ "Homicidio",

      ### GC4: Homicidio
      between(cie10_cod, "Y09.0", "Y09.9") ~ "GC4-Homicidio",

      ### Valor por defecto
      .default = paso1
    )
  )


# Paso 1: Agrupar causas no objetivo y GC --------------------------------
## CMNN -----
recod_defun <- recod_defun |>
  mutate(
    paso1 = case_when(
      ### Enfermedades transmisibles
      between(cie10_cod, "A00.0", "A09.9") |
        # A10 - A14: No existe
        between(cie10_cod, "A15.0", "A28.9") |
        between(cie10_cod, "A32.0", "A39.9") |
        cie10_cod %in% c("A48.1", "A48.2", "A48.4") |
        # A48.5: No existe
        between(cie10_cod, "A50.0", "A58.0") |
        between(cie10_cod, "A60.0", "A60.9") |
        between(cie10_cod, "A63.0", "A63.8") |
        cie10_cod == "A65.0" |
        between(cie10_cod, "A68.0", "A70.0") |
        between(cie10_cod, "A74.8", "A75.9") |
        between(cie10_cod, "A77.0", "A96.9") |
        between(cie10_cod, "A98.0", "A98.8") |

        between(cie10_cod, "B00.0", "B06.9") |
        # B10: No existe
        between(cie10_cod, "B15.0", "B16.2") |
        cie10_cod %in% c("B17.0", "B17.2", "B19.1") |
        between(cie10_cod, "B20.0", "B27.9") |
        cie10_cod %in% c("B29.4", "B33.0", "B33.1") |
        between(cie10_cod, "B33.3", "B33.8") |
        between(cie10_cod, "B47.0", "B48.8") |
        between(cie10_cod, "B50.0", "B54.0") |
        cie10_cod == "B55.0" |
        between(cie10_cod, "B56.0", "B57.5") |
        between(cie10_cod, "B60.0", "B60.8") |
        # B63: No existe
        between(cie10_cod, "B65.0", "B67.9") |
        between(cie10_cod, "B69.0", "B72.0") |
        between(cie10_cod, "B74.3", "B75.0") |
        between(cie10_cod, "B77.0", "B77.9") |
        between(cie10_cod, "B83.0", "B83.8") |
        between(cie10_cod, "B90.0", "B91.0") |
        cie10_cod == "B94.1" |
        between(cie10_cod, "B95.0", "B95.5") |
        cie10_cod %in% c("B97.4", "B97.5", "B97.6") |

        between(cie10_cod, "G00.0", "G00.8") |
        between(cie10_cod, "G03.0", "G03.8") |
        between(cie10_cod, "G04.0", "G05.8") |
        between(cie10_cod, "G14.0", "G14.6") |
        cie10_cod == "G21.3" |

        between(cie10_cod, "H70.0", "H70.9") |

        cie10_cod %in% c("I02.9", "I98.0", "I98.1") |

        between(cie10_cod, "J00.0", "J02.8") |
        between(cie10_cod, "J03.0", "J03.8") |
        between(cie10_cod, "J04.0", "J04.2") |
        between(cie10_cod, "J05.0", "J06.8") |
        # J07 - J08: No existe
        between(cie10_cod, "J09.0", "J15.8") |
        between(cie10_cod, "J16.0", "J16.9") |
        # J19: No existe
        between(cie10_cod, "J20.0", "J21.9") |

        cie10_cod %in% c("J36.0", "J91.0", "K52.1") |
        between(cie10_cod, "K67.0", "K67.8") |

        cie10_cod %in%
          c(
            "K75.3",
            "K76.3",
            "K77.0",
            "K93.0",
            "K93.1",
            "M03.1",
            "M12.1",
            "M49.0",
            "M49.1",
            "M73.0",
            "M73.1",
            "M89.6",
            "N74.1"
          ) |

        ### Enfermedades nutricionales
        between(cie10_cod, "D50.1", "D50.8") |
        between(cie10_cod, "D51.0", "D52.0") |
        between(cie10_cod, "D52.8", "D53.9") |
        cie10_cod %in% c("D70.3", "D89.3") |

        between(cie10_cod, "E00.0", "E02.0") |
        between(cie10_cod, "E40.0", "E46.9") |
        between(cie10_cod, "E51.0", "E61.9") |
        between(cie10_cod, "E63.0", "E64.0") |
        between(cie10_cod, "E64.2", "E64.9") |

        ### Condiciones maternas
        cie10_cod == "N96.0" |
        between(cie10_cod, "N98.0", "N98.9") |

        between(cie10_cod, "O00.0", "O07.9") |
        # O09: No existe
        between(cie10_cod, "O10.0", "O16.0") | # O16: No tiene decimales
        between(cie10_cod, "O20.0", "O26.9") |
        between(cie10_cod, "O28.0", "O36.9") |
        between(cie10_cod, "O40.0", "O48.1") |
        between(cie10_cod, "O60.0", "O77.9") |
        between(cie10_cod, "O80.0", "O92.7") |
        between(cie10_cod, "O96.0", "P04.2") |

        ### Condiciones neonatales
        between(cie10_cod, "P04.5", "P05.9") |
        between(cie10_cod, "P07.0", "P15.9") |
        between(cie10_cod, "P19.0", "P23.4") |
        between(cie10_cod, "P24.0", "P29.9") |
        between(cie10_cod, "P35.0", "P37.2") |
        between(cie10_cod, "P37.5", "P39.9") |
        between(cie10_cod, "P50.0", "P61.9") |
        between(cie10_cod, "P70.0", "P70.1") |
        between(cie10_cod, "P70.3", "P72.9") |
        between(cie10_cod, "P74.0", "P78.9") |
        between(cie10_cod, "P80.0", "P81.9") |
        between(cie10_cod, "P83.0", "P83.9") |
        between(cie10_cod, "P90.0", "P94.9") | # P93.0: Causas externas GBD 2021
        cie10_cod %in% c("P96.3", "P96.4", "P96.8") |

        ### Otros CMNN específicos
        cie10_cod %in% c("F02.1", "F02.4", "F07.1", "R19.7") |
        between(cie10_cod, "U04.0", "U04.9") |
        between(cie10_cod, "U06.0", "U07.2") | # COVID-19
        between(cie10_cod, "U82.0", "U89.0") |
        between(cie10_cod, "W75.0", "W75.9") |
        between(cie10_cod, "W78.0", "W80.9") |
        between(cie10_cod, "W83.0", "W84.9") |
        between(cie10_cod, "Z16.0", "Z16.3") ~ "CMNN",

      ### GC3: Trasmisibles, maternas/neonatales, nutricionales (CMNN)
      between(cie10_cod, "A31.0", "A31.9") |
        between(cie10_cod, "A42.0", "A44.9") |
        cie10_cod %in%
          c("A49.2", "A64.0", "A99.0", "B17.1", "B17.8", "B17.9", "B19.0") |
        between(cie10_cod, "B19.2", "B19.9") |
        between(cie10_cod, "B37.0", "B46.9") |
        cie10_cod == "B49.0" | # B49: No tiene decimales
        between(cie10_cod, "B55.1", "B55.9") |
        between(cie10_cod, "B58.0", "B59.0") |
        cie10_cod %in% c("B89.0", "B94.2", "J02.9", "J03.9", "J04.3", "J06.9") |
        between(cie10_cod, "O08.0", "O08.9") |
        between(cie10_cod, "O94.0", "O95.0") | # 095: No tiene decimales
        cie10_cod == "P96.9" |
        between(cie10_cod, "R00.0", "R01.2") |
        between(cie10_cod, "R07.1", "R07.9") |
        between(cie10_cod, "R31.0", "R31.9") ~ "GC3-CMNN",

      ### GC4: Trasmisibles, maternas/neonatales, nutricionales (CMNN)
      cie10_cod %in%
        c("B16.9", "B64.0") |
        between(cie10_cod, "B82.0", "B82.9") |
        cie10_cod == "B83.9" |
        between(cie10_cod, "G00.9", "G02.8") |
        cie10_cod == "G03.9" |
        between(cie10_cod, "J17.0", "J17.9") |
        cie10_cod == "J22.0" |
        between(cie10_cod, "P23.5", "P23.9") |
        between(cie10_cod, "P37.3", "P37.4") ~ "GC4-CMNN",

      ### Neumonías inespecíficas
      cie10_cod == "J15.9" |
        between(cie10_cod, "J18.0", "J18.9") ~ "GC4-Neumonías NE",

      ### Valor por defecto
      .default = paso1
    )
  )


## Otras ENT -----
recod_defun <- recod_defun |>
  mutate(
    paso1 = case_when(
      ### Digestivas
      between(cie10_cod, "I84.0", "I85.9") |
        between(cie10_cod, "K20.0", "K20.9") |
        between(cie10_cod, "K22.0", "K22.6") |
        between(cie10_cod, "K22.8", "K29.9") |
        between(cie10_cod, "K31.0", "K31.8") |
        between(cie10_cod, "K35.0", "K38.9") |
        # K39: No existe
        between(cie10_cod, "K40.0", "K46.9") | # K43.0 - K43.9: Causas externas GBD 2021
        between(cie10_cod, "K50.0", "K52.0") | # K52.0: Causas externas GBD 2021
        between(cie10_cod, "K52.2", "K52.9") |
        between(cie10_cod, "K55.0", "K61.4") |
        between(cie10_cod, "K62.2", "K62.9") | # K62.7: Causas externas GBD 2021
        between(cie10_cod, "K64.0", "K64.9") |
        cie10_cod == "K66.8" |
        # K68: No existe
        between(cie10_cod, "K70.0", "K70.3") |
        cie10_cod == "K71.7" |
        between(cie10_cod, "K73.0", "K74.6") |
        cie10_cod == "K75.2" |
        between(cie10_cod, "K75.4", "K76.2") |
        between(cie10_cod, "K76.4", "K76.9") |
        cie10_cod == "K77.8" |
        between(cie10_cod, "K80.0", "K83.9") |
        between(cie10_cod, "K85.0", "K86.9") |
        between(cie10_cod, "K90.0", "K91.9") | # K91.0 - K91.9: Causas externas GBD 2021
        cie10_cod == "K92.8" |
        cie10_cod == "K93.8" |
        between(cie10_cod, "K94.0", "K95.8") |

        ### Neurológicas
        between(cie10_cod, "G10.0", "G13.8") |
        cie10_cod %in% c("G20.0", "G21.0", "G21.1") | # G21.0 - G21.1: Causas externas GBD 2021
        # G20: No tiene decimales
        between(cie10_cod, "G23.0", "G26.0") | # G24.0, G25.1, G25.4, G25.6 y G25.7: Causas externas GBD 2021
        between(cie10_cod, "G30.0", "G31.9") |
        between(cie10_cod, "G35.0", "G37.9") |
        between(cie10_cod, "G40.0", "G41.9") |
        between(cie10_cod, "G61.0", "G61.9") |
        cie10_cod == "G62.1" |
        between(cie10_cod, "G70.0", "G73.7") | # G72.0: Causas externas GBD 2021
        between(cie10_cod, "G90.0", "G90.9") |
        cie10_cod == "G93.7" | # Causas externas GBD 2021
        between(cie10_cod, "G95.0", "G95.9") |
        between(cie10_cod, "G97.0", "G97.9") |

        ### Mentales
        between(cie10_cod, "F00.0", "F02.0") |
        between(cie10_cod, "F02.2", "F02.3") |
        between(cie10_cod, "F02.8", "F03.0") | # F03: No tiene decimales
        cie10_cod == "F24.0" |
        # F49: No existe
        between(cie10_cod, "F50.0", "F50.5") |

        ### Uso de sustancias
        between(cie10_cod, "F10.0", "F16.9") |
        between(cie10_cod, "F18.0", "F18.9") |
        between(cie10_cod, "X45.0", "X45.9") |
        between(cie10_cod, "X65.0", "X65.9") |
        between(cie10_cod, "Y15.0", "Y15.9") |

        ### Renales crónicas
        cie10_cod %in% c("E10.2", "E11.2") |
        between(cie10_cod, "I12.0", "I13.9") |
        between(cie10_cod, "N02.0", "N08.8") |
        cie10_cod == "N15.0" |
        between(cie10_cod, "N18.0", "N18.9") |
        between(cie10_cod, "Q61.0", "Q62.8") |

        ### Piel y subcutáneas
        between(cie10_cod, "I89.1", "I89.8") |
        between(cie10_cod, "L00.0", "L05.9") |
        between(cie10_cod, "L08.0", "L08.9") |
        between(cie10_cod, "L10.0", "L14.0") |
        between(cie10_cod, "L51.0", "L51.9") |
        between(cie10_cod, "L88.0", "L89.9") |
        between(cie10_cod, "L93.0", "L93.2") |
        between(cie10_cod, "L97.0", "L98.4") |

        ### Musculoesqueléticas
        cie10_cod %in% c("I27.1", "I67.7") |
        between(cie10_cod, "M00.0", "M03.0") |
        between(cie10_cod, "M03.2", "M03.6") |
        # M04: No existe
        between(cie10_cod, "M05.0", "M09.8") |
        # M26 - M29: No existe
        between(cie10_cod, "M30.0", "M36.8") |
        # M37 - M39: No existe
        between(cie10_cod, "M40.0", "M43.1") |
        # M44: No existe
        # M55 - M59: No existe
        # M64: No existe
        cie10_cod %in% c("M65.0", "M71.0", "M71.1", "M72.5", "M72.6") |
        between(cie10_cod, "M80.0", "M82.8") |
        cie10_cod %in% c("M86.3", "M86.4", "M87.0", "M87.1") | # M87.1: Causas externas GBD 2021
        between(cie10_cod, "M88.0", "M89.0") |
        cie10_cod %in% c("M89.5", "M89.7", "M89.8", "M89.9") |

        ### Otras ENT
        cie10_cod == "A46.0" |
        between(cie10_cod, "A66.0", "A67.9") |
        between(cie10_cod, "B18.0", "B18.9") |
        cie10_cod == "B86.0" |
        between(cie10_cod, "D25.0", "D25.9") |
        cie10_cod %in% c("D28.2", "D52.1") | # D28.2: Causas externas GBD 2021
        # D54: No existe
        between(cie10_cod, "D55.0", "D59.3") | # D59.0 y D59.2: Causas externas GBD 2021
        between(cie10_cod, "D59.5", "D59.6") | # D59.6: Causas externas GBD 2021
        between(cie10_cod, "D60.0", "D61.9") |
        cie10_cod == "D64.0" |
        between(cie10_cod, "D66.0", "D69.8") |
        between(cie10_cod, "D70.0", "D75.8") |
        between(cie10_cod, "D76.0", "D77.9") |
        cie10_cod %in% c("D86.3", "D86.8") |
        between(cie10_cod, "D89.0", "D89.2") |
        between(cie10_cod, "E03.0", "E07.1") |
        between(cie10_cod, "E16.0", "E16.9") |
        between(cie10_cod, "E20.0", "E32.9") |
        between(cie10_cod, "E34.1", "E34.8") |
        between(cie10_cod, "E65.0", "E68.0") |
        between(cie10_cod, "E70.0", "E85.2") |
        between(cie10_cod, "E88.0", "E89.9") |
        between(cie10_cod, "J70.0", "J70.5") |
        between(cie10_cod, "N00.0", "N01.9") |
        between(cie10_cod, "N10.0", "N12.9") |
        cie10_cod == "N13.6" |
        between(cie10_cod, "N14.0", "N16.8") |
        between(cie10_cod, "N20.0", "N23.0") |
        between(cie10_cod, "N25.0", "N28.1") |
        between(cie10_cod, "N29.0", "N30.3") |
        between(cie10_cod, "N30.8", "N32.0") |
        between(cie10_cod, "N32.3", "N32.4") |
        between(cie10_cod, "N34.0", "N34.3") |
        between(cie10_cod, "N36.0", "N36.9") |
        between(cie10_cod, "N39.0", "N39.2") |
        between(cie10_cod, "N41.0", "N41.9") |
        between(cie10_cod, "N44.0", "N45.9") |
        between(cie10_cod, "N49.0", "N49.9") |
        between(cie10_cod, "N65.0", "N65.1") |
        cie10_cod == "N72.0" |
        between(cie10_cod, "N75.0", "N77.8") |
        between(cie10_cod, "N80.0", "N81.9") |
        between(cie10_cod, "N83.0", "N83.9") |
        between(cie10_cod, "N99.0", "N99.9") |
        between(cie10_cod, "P04.3", "P04.4") |
        between(cie10_cod, "P93.0", "P93.8") |
        between(cie10_cod, "P96.0", "P96.2") |
        cie10_cod == "P96.5" |
        between(cie10_cod, "Q00.0", "Q07.9") |
        between(cie10_cod, "Q10.4", "Q18.9") |
        between(cie10_cod, "Q20.0", "Q28.9") |
        between(cie10_cod, "Q30.0", "Q35.9") |
        between(cie10_cod, "Q37.0", "Q45.9") |
        between(cie10_cod, "Q50.0", "Q87.8") |
        between(cie10_cod, "Q89.0", "Q89.8") |
        between(cie10_cod, "Q90.0", "Q93.9") |
        between(cie10_cod, "Q95.0", "Q99.8") |
        cie10_cod == "R50.2" |
        between(cie10_cod, "R78.0", "R78.5") |
        between(cie10_cod, "R95.0", "R95.9") ~ "Otras ENT",

      ### GC3: Otras ENT
      cie10_cod == "D75.9" |
        # D79: No existe
        between(cie10_cod, "D80.0", "D84.9") |
        # D85: No existe
        # D87 - D88: No existe
        between(cie10_cod, "D89.8", "D89.9") |
        # D90 - D99: No existe
        between(cie10_cod, "E07.8", "E07.9") |
        # E08: No existe
        # E17 - E19: No existe
        cie10_cod == "E34.0" |
        between(cie10_cod, "E34.9", "E35.8") |
        # E36 - E39: No existe
        # E47 - E49: No existe
        # E62: No existe
        # E69: No existe
        cie10_cod %in% c("E87.7", "E90.0") |
        # E91 - E99: No existe
        between(cie10_cod, "F04.0", "F06.1") |
        between(cie10_cod, "F06.5", "F07.0") |
        cie10_cod %in%
          c(
            "F07.8",
            "F07.9",
            # F08: No existe
            "F50.8",
            "F50.9",
            "G09.0", # G09: No tiene decimales
            # G15 - G19: No existe
            "G21.2"
          ) |
        between(cie10_cod, "G21.4", "G22.0") |
        # G27 - G29: No existe
        # G33 - G34: No existe
        # G38 - G39: No existe
        # G42: No existe
        # G48 - G49: No existe
        # G66 - G69: No existe
        # G74 - G79: No existe
        # G84 - G88: No existe
        between(cie10_cod, "G93.8", "G93.9") |
        between(cie10_cod, "G96.0", "G96.9") |
        cie10_cod %in% c("G98.0", "H05.0") | # G98: No tiene decimales
        between(cie10_cod, "K21.0", "K21.9") |
        cie10_cod %in% c("K22.7", "K31.9") |
        # K32 - K34: No existe
        # K47 - K49: No existe
        # K53 - K54: No existe
        between(cie10_cod, "K63.0", "K63.4") |
        between(cie10_cod, "K63.8", "K63.9") |
        # K69: No existe
        between(cie10_cod, "K70.4", "K70.9") |
        # K78 - K79: No existe
        # K84: No existe
        # K88 - K89: No existe
        cie10_cod %in% c("K87.0", "K87.1", "K92.9") |
        # K94 - K99: No existe
        # L06 - L07: No existe
        # L09: No existe
        # L15 - L19: No existe
        # L31 - L39: No existe
        # L76 - L79: No existe
        # N09: No existe
        between(cie10_cod, "N13.0", "N13.5") |
        between(cie10_cod, "N13.7", "N13.9") |
        # N24: No existe
        cie10_cod %in%
          c(
            "N28.8",
            "N28.9",
            "N39.9",
            # N38: No existe
            "N40.0"
          ) |
        # N54 - N59: No existe
        # N65 - N69: No existe
        # N78 - N79: No existe
        between(cie10_cod, "N84.2", "N86.0") |
        between(cie10_cod, "N88.0", "N90.9") |
        between(cie10_cod, "N92.0", "N95.0") |
        between(cie10_cod, "Q10.0", "Q10.3") |
        between(cie10_cod, "Q36.0", "Q36.9") |
        cie10_cod %in% c("Q89.9", "Q99.9") ~ "GC3-Otras ENT",

      ### GC4: Otras ENT
      cie10_cod %in%
        c("E12.2", "E13.2", "E14.2") |
        between(cie10_cod, "G00.9", "G02.8") |
        cie10_cod == "G03.9" ~ "GC4-Otras ENT",

      ### Valor por defecto
      .default = paso1
    )
  )


## Otras CE -----
recod_defun <- recod_defun |>
  mutate(
    paso1 = case_when(
      ### Otras lesiones por transporte
      # V00.0 - V00.8: No existe
      between(cie10_cod, "V05.0", "V05.9") |
        between(cie10_cod, "V81.0", "V81.9") |
        between(cie10_cod, "V83.0", "V86.9") |
        between(cie10_cod, "V88.2", "V88.3") |
        between(cie10_cod, "V90.0", "V98.8") |

        ### Caídas
        between(cie10_cod, "W00.0", "W19.9") |

        ### Otras causas externas
        between(cie10_cod, "L55.0", "L55.8") |
        cie10_cod %in% c("L56.3", "L56.8", "L58.0", "L58.1") |
        between(cie10_cod, "W20.0", "W46.9") |
        between(cie10_cod, "W49.0", "W60.9") |
        # W61 - W63: No existe
        between(cie10_cod, "W64.0", "W70.9") |
        between(cie10_cod, "W73.0", "W74.9") |
        between(cie10_cod, "W77.0", "W77.9") |
        between(cie10_cod, "W81.0", "W82.9") |
        between(cie10_cod, "W85.0", "W94.9") |
        cie10_cod == "W97.9" |
        between(cie10_cod, "W99.0", "X06.9") |
        between(cie10_cod, "X08.0", "X39.9") |
        between(cie10_cod, "X46.0", "X48.9") |
        between(cie10_cod, "X50.0", "X54.9") |
        between(cie10_cod, "X57.0", "X58.9") |
        between(cie10_cod, "Y35.0", "Y85.9") |
        between(cie10_cod, "Y88.0", "Y88.3") |
        between(cie10_cod, "Y89.0", "Y89.1") ~ "Otras CE",

      ### GC4: Otras lesiones por transporte
      between(cie10_cod, "V88.0", "V88.1") |
        between(cie10_cod, "V88.4", "V88.9") |
        cie10_cod == "V99.0" ~ "GC4-Otras CE",

      ### Valor por defecto
      .default = paso1
    )
  )


## GC nivel 2 -----
recod_defun <- recod_defun |>
  mutate(
    paso1 = case_when(
      ### GC2: Cardiovasculares
      cie10_cod == "I10.0" |
        between(cie10_cod, "I15.0", "I15.9") |
        between(cie10_cod, "I27.2", "I27.9") |
        between(cie10_cod, "I70.0", "I70.9") |
        between(cie10_cod, "I74.0", "I74.9") ~ "GC2-ECV",

      ### GC2: Cualquier causa externa, otras ENT, CMNN
      between(cie10_cod, "X59.0", "X59.9") ~ "GC2-CEEC",

      ### GC2: Cualquier causa externa
      cie10_cod %in%
        c(
          "G44.3",
          "G91.3",
          "R58.0",
          "Y24.5",
          "Y24.6",
          "Y24.7",
          "Y25.2",
          "Y26.3",
          "Y27.4",
          "Y27.5",
          "Y27.6",
          "Y28.3",
          "Y28.5"
        ) |
        between(cie10_cod, "Y29.1", "Y30.9") |
        between(cie10_cod, "Y33.0", "Y33.9") |
        cie10_cod %in% c("Y86.0", "Y86.2", "Y86.8", "Y87.2", "Y89.9") |
        between(cie10_cod, "Y90.0", "Y91.9") |
        between(cie10_cod, "Y95.0", "Y98.0") ~ "GC2-CE",

      ### GC2: lesiones por transporte, suicidio, homicidio
      between(cie10_cod, "Y31.0", "Y32.9") ~ "GC2-TSH",

      ### GC2: suicidio, homicidio, otras CE
      between(cie10_cod, "W76.0", "W76.9") ~ "GC2-SHCE",

      ### GC2 generales
      # A14.9: No existe
      # A29: No existe
      between(cie10_cod, "A30.0", "A30.9") |
        # A45: No existe
        # A47: No existe
        cie10_cod == "A48.8" |
        between(cie10_cod, "A49.3", "A49.9") |
        # A61 - A62: No existe
        # A72 - A73: No existe
        # A76: No existe
        between(cie10_cod, "A97.0", "A97.9") |
        between(cie10_cod, "B08.0", "B09.0") |
        # B11 - B14: No existe
        # B28 - B29: No existe
        # B31 - B32: No existe
        between(cie10_cod, "B34.0", "B34.9") |
        # B61 - B62: No existe
        between(cie10_cod, "B68.0", "B68.9") |
        between(cie10_cod, "B73.0", "B74.2") |
        between(cie10_cod, "B76.0", "B76.9") |
        between(cie10_cod, "B78.0", "B81.8") |
        # B84: No existe
        cie10_cod %in%
          c(
            "B92.0",
            # B93: No existe
            "B94.0",
            "B94.8",
            "B94.9"
          ) |
        between(cie10_cod, "B95.6", "B97.3") |
        between(cie10_cod, "B97.7", "B99.9") |
        cie10_cod %in% c("D59.4", "D59.8", "D59.9") |
        between(cie10_cod, "F17.0", "F17.9") |
        cie10_cod %in%
          c(
            "G93.0",
            "G93.3",
            # I50.8: No existe
            "I50.9",
            "I67.4",
            # I75: No existe
            # J81.1: No existe
            "J90.0",
            "J94.0",
            "J94.1",
            "J94.8",
            "J94.9"
          ) |
        between(cie10_cod, "K92.0", "K92.2") |
        between(cie10_cod, "N70.0", "N71.9") |
        between(cie10_cod, "N73.0", "N74.0") |
        between(cie10_cod, "N74.2", "N74.8") |
        cie10_cod == "R03.0" |
        between(cie10_cod, "R04.0", "R06.9") |
        between(cie10_cod, "R09.0", "R09.2") |
        between(cie10_cod, "R09.8", "R10.9") |
        between(cie10_cod, "R13.0", "R13.9") |
        cie10_cod == "R23.0" |
        between(cie10_cod, "S00.0", "T98.3") |
        # W47 - W48: No existe
        # W63: No existe
        # W71 - W72: No existe
        # W82: No existe
        # W95 - W98: No existe
        between(cie10_cod, "Y20.0", "Y24.4") |
        between(cie10_cod, "Y24.8", "Y25.1") |
        between(cie10_cod, "Y25.4", "Y26.2") |
        between(cie10_cod, "Y26.4", "Y27.3") |
        between(cie10_cod, "Y27.8", "Y28.2") |
        cie10_cod %in% c("Y28.4", "Y28.6") |
        between(cie10_cod, "Y28.8", "Y29.2") |
        between(cie10_cod, "Y34.0", "Y34.9") | # GC2-CE
        between(cie10_cod, "Y92.0", "Y94.9") |
        between(cie10_cod, "Y98.1", "Y99.9") ~ "GC2",

      ### Valor por defecto
      .default = paso1
    )
  )


## GC nivel 1 -----
recod_defun <- recod_defun |>
  mutate(
    paso1 = case_when(
      ### GC1 generales
      between(cie10_cod, "A40.0", "A41.9") |
        cie10_cod %in% c("A48.0", "A48.3", "A49.0", "A49.1") |
        between(cie10_cod, "A59.0", "A59.9") |
        between(cie10_cod, "A71.0", "A71.9") |
        cie10_cod %in%
          c(
            "A74.0",
            "B07.0" # B07: No tiene decimales
          ) |
        between(cie10_cod, "B30.0", "B30.9") |
        between(cie10_cod, "B35.0", "B36.9") |
        between(cie10_cod, "B85.0", "B85.4") |
        between(cie10_cod, "B87.0", "B88.9") |
        cie10_cod %in% c("B94.0", "D50.0", "D50.9") |
        between(cie10_cod, "D62.0", "D63.0") |
        cie10_cod == "D63.8" |
        between(cie10_cod, "D64.1", "D65.9") |
        cie10_cod %in% c("D69.9", "E15.0") |
        between(cie10_cod, "E50.0", "E50.9") |
        cie10_cod == "E64.1" |
        between(cie10_cod, "E85.3", "E87.6") |
        cie10_cod %in%
          c(
            "E87.8",
            # E87.9: No existe
            "F06.2",
            "F06.3",
            "F06.4",
            "F07.2",
            "F09.0" # F09: No tiene decimales
          ) |
        between(cie10_cod, "F19.0", "F23.9") |
        between(cie10_cod, "F25.0", "F48.9") |
        # F49: No existe
        between(cie10_cod, "F51.0", "F99.0") |
        between(cie10_cod, "G06.0", "G08.0") |
        between(cie10_cod, "G32.0", "G32.8") |
        between(cie10_cod, "G43.0", "G44.2") |
        between(cie10_cod, "G44.4", "G44.8") |
        between(cie10_cod, "G47.0", "G47.2") |
        between(cie10_cod, "G47.4", "G47.9") |
        between(cie10_cod, "G50.0", "G60.9") |
        cie10_cod == "G62.0" |
        between(cie10_cod, "G62.2", "G64.0") |
        # G65: No existe
        between(cie10_cod, "G80.0", "G83.9") |
        # G89: No existe
        between(cie10_cod, "G91.0", "G91.2") |
        between(cie10_cod, "G91.4", "G92.0") |
        between(cie10_cod, "G93.1", "G93.2") |
        between(cie10_cod, "G93.4", "G93.6") |
        between(cie10_cod, "G94.0", "G94.8") |
        between(cie10_cod, "G99.0", "H04.9") |
        between(cie10_cod, "H05.2", "H69.9") |
        between(cie10_cod, "H71.0", "H95.9") |
        # H96 - H99: No existe
        between(cie10_cod, "I26.0", "I26.9") |
        between(cie10_cod, "I31.2", "I31.3") |
        # I31.4: No existe
        between(cie10_cod, "I46.0", "I46.9") |
        cie10_cod %in%
          c(
            "I50.0",
            "I50.1",
            # I50.4: No existe
            # I76: No existe
            "I95.0",
            "I95.1",
            "I95.8",
            "I95.9"
          ) |
        between(cie10_cod, "J69.0", "J69.9") |
        between(cie10_cod, "J80.0", "J81.0") |
        between(cie10_cod, "J85.0", "J85.3") |
        between(cie10_cod, "J86.0", "J86.9") |
        cie10_cod %in%
          c("J93.0", "J93.1", "J93.8", "J93.9", "J94.2", "J96.0", "J96.9") |
        between(cie10_cod, "J98.1", "J98.3") |
        between(cie10_cod, "K00.0", "K14.9") |
        # K15 - K19: No existe
        cie10_cod == "K30.0" |
        between(cie10_cod, "K65.0", "K66.1") |
        cie10_cod == "K66.9" |
        # K68: No existe
        between(cie10_cod, "K71.0", "K71.6") |
        between(cie10_cod, "K71.8", "K72.9") |
        cie10_cod == "K75.0" |
        between(cie10_cod, "L20.0", "L30.9") |
        between(cie10_cod, "L40.0", "L50.9") |
        between(cie10_cod, "L52.0", "L54.8") |
        cie10_cod %in% c("L56.0", "L56.1", "L56.2", "L56.4", "L56.5") |
        between(cie10_cod, "L57.0", "L57.9") |
        between(cie10_cod, "L59.0", "L68.9") |
        # L69: No existe
        between(cie10_cod, "L70.0", "L75.9") |
        # L76: No existe
        between(cie10_cod, "L80.0", "L87.9") |
        between(cie10_cod, "L90.0", "L92.9") |
        between(cie10_cod, "L94.0", "L95.9") |
        # L96: No existe
        between(cie10_cod, "L98.5", "L99.8") |
        # M04: No existe
        between(cie10_cod, "M10.0", "M12.0") |
        between(cie10_cod, "M12.2", "M25.9") |
        # M26 - M29: No existe
        # M37- M39: No existe
        between(cie10_cod, "M43.2", "M48.9") | # M44: No existe
        between(cie10_cod, "M49.2", "M63.9") | # M55 - M59: No existe
        # M64: No existe
        between(cie10_cod, "M65.1", "M70.9") |
        between(cie10_cod, "M71.2", "M72.4") |
        between(cie10_cod, "M72.8", "M72.9") |
        between(cie10_cod, "M73.8", "M79.9") |
        between(cie10_cod, "M83.0", "M86.2") |
        between(cie10_cod, "M86.5", "M86.9") |
        between(cie10_cod, "M87.2", "M87.9") |
        between(cie10_cod, "M89.1", "M89.4") |
        between(cie10_cod, "M90.0", "M99.9") |
        between(cie10_cod, "N17.0", "N17.9") |
        cie10_cod %in% c("N19.0", "N32.1", "N32.2") | # N19 no tiene decimales
        between(cie10_cod, "N32.8", "N33.8") |
        between(cie10_cod, "N35.0", "N35.9") |
        between(cie10_cod, "N37.0", "N37.8") |
        between(cie10_cod, "N39.3", "N39.8") |
        between(cie10_cod, "N42.0", "N43.4") |
        between(cie10_cod, "N44.1", "N44.8") |
        between(cie10_cod, "N46.0", "N48.9") |
        between(cie10_cod, "N50.0", "N51.8") |
        # N52 - N53: No existe
        between(cie10_cod, "N61.0", "N64.9") |
        between(cie10_cod, "N82.0", "N82.9") |
        between(cie10_cod, "N91.0", "N91.5") |
        between(cie10_cod, "N95.1", "N95.9") |
        between(cie10_cod, "N97.0", "N97.9") |
        between(cie10_cod, "R02.0", "R02.9") |
        cie10_cod %in% c("R03.1", "R07.0", "R09.3") |
        between(cie10_cod, "R11.0", "R12.0") |
        between(cie10_cod, "R14.0", "R19.6") |
        between(cie10_cod, "R19.8", "R22.9") |
        between(cie10_cod, "R23.1", "R30.9") |
        between(cie10_cod, "R32.0", "R50.1") |
        between(cie10_cod, "R50.8", "R57.9") |
        between(cie10_cod, "R58.0", "R72.9") |
        between(cie10_cod, "R74.0", "R77.9") |
        between(cie10_cod, "R78.6", "R94.8") |
        between(cie10_cod, "R96.0", "R99.0") |
        cie10_cod == "U05.0" |
        between(cie10_cod, "U08.0", "U81.9") |
        # U89 - U99: No existe
        between(cie10_cod, "X46.0", "X46.9") | # Causas externas?
        between(cie10_cod, "Z00.0", "Z13.9") |
        # Z14 - Z19: No existe
        between(cie10_cod, "Z20.0", "Z99.9") ~ "GC1",

      ### GC1: Respiratorias crónicas
      cie10_cod == "J96.1" ~ "GC1-ERC",

      ### GC1: Causas externas
      between(cie10_cod, "X40.0", "X44.9") |
        between(cie10_cod, "X49.0", "X49.9") |
        between(cie10_cod, "Y10.0", "Y14.9") |
        between(cie10_cod, "Y16.0", "Y19.9") ~ "GC1-HSCE",

      ### Valor por defecto
      .default = paso1
    )
  )


# Paso 2: Redistribuir GC3 y GC4 -----------------------------------------
## Paso 2.1: Reasignar GC3-GC4 específicos -----
recod_defun <- recod_defun |>
  # Crear variable para grupo de causas
  mutate(
    grupo_causa = if_else(
      str_detect(paso1, "GC|CE"),
      str_remove(paso1, "-.*"),
      paso1
    ),
    .before = paso1
  ) |>

  # Reasignar GC3 y GC4
  mutate(
    paso2.1 = case_when(
      ## Reagrupar GC
      str_detect(paso1, "Neum|GC1|GC2") ~ grupo_causa,

      ## Asignar GC3 y GC4 específicos
      str_detect(paso1, "GC3-|GC4-") ~ str_remove(paso1, ".*-"),

      ## Valor por defecto
      .default = paso1
    )
  )


## Frecuencia muertes ENT por edad y sexo -----
freq_ent <- recod_defun |>
  # Seleccionar filas ENTs
  filter(str_detect(paso2.1, "Dia|ECV|ERC|Neo")) |>

  # Frecuencias
  count(grupo_edad, sexo, paso2.1) |>
  mutate(pct = n / sum(n), .by = c(grupo_edad, sexo))


## Frecuencia muertes CE por edad y sexo -----
freq_ce <- recod_defun |>
  # Seleccionar filas CE
  filter(str_detect(paso2.1, "Hom|Sui|Tr|CE")) |>

  # Frecuencias
  count(grupo_edad, sexo, paso2.1) |>
  mutate(pct = n / sum(n), .by = c(grupo_edad, sexo))


## Frecuencia causas definidas por edad y sexo -----
freq_cd <- recod_defun |>
  # Seleccionar filas CE
  filter(!str_detect(paso2.1, "GC")) |>

  # Frecuencias
  count(grupo_edad, sexo, paso2.1) |>
  mutate(pct = n / sum(n), .by = c(grupo_edad, sexo))


## Paso 2.2: Redistribuir neumonías inespecíficas -----
set.seed(123)

recod_defun <- recod_defun |>
  mutate(
    paso2.2 = case_when(
      ### 50% CMNN
      paso2.1 == "GC4" & runif(n()) <= 0.5 ~ "CMNN",

      ### 50% redistribución multinomial en ENT
      paso2.1 == "GC4" ~ {
        causas <- freq_ent$paso2.1[
          freq_ent$sexo == cur_group()$sexo &
            freq_ent$grupo_edad == cur_group()$grupo_edad
        ]

        probs <- freq_ent$pct[
          freq_ent$sexo == cur_group()$sexo &
            freq_ent$grupo_edad == cur_group()$grupo_edad
        ]

        rep(causas, rmultinom(1, n(), probs))
      },

      ### Valor por defecto
      .default = paso2.1
    ),

    ### Variables de agrupamiento
    .by = c(sexo, grupo_edad)
  )


# Paso 3: Redistribuir GC2 -----------------------------------------------
## Paso 3.1: Redistribuir GC2 específicos -----
set.seed(123)

recod_defun <- recod_defun |>
  mutate(
    paso3.1 = case_when(
      ### GC2: Cardiovascular
      paso1 == "GC2-ECV" ~ "ECV",

      ### GC2: Cualquier causa externa
      paso1 == "GC2-CE" ~ {
        causas <- freq_ce$paso2.1[
          freq_ce$sexo == cur_group()$sexo &
            freq_ce$grupo_edad == cur_group()$grupo_edad
        ]

        probs <- freq_ce$pct[
          freq_ce$sexo == cur_group()$sexo &
            freq_ce$grupo_edad == cur_group()$grupo_edad
        ]

        rep(causas, rmultinom(1, n(), probs))
      },

      ### GC2: Transporte, suicidio, homicidio
      paso1 == "GC2-TSH" ~ {
        causas <- freq_ce$paso2.1[
          freq_ce$sexo == cur_group()$sexo &
            freq_ce$grupo_edad == cur_group()$grupo_edad &
            str_detect(freq_ce$paso2.1, "Tr|Sui|Hom")
        ]

        probs <- freq_ce$pct[
          freq_ce$sexo == cur_group()$sexo &
            freq_ce$grupo_edad == cur_group()$grupo_edad &
            str_detect(freq_ce$paso2.1, "Tr|Sui|Hom")
        ]

        rep(causas, rmultinom(1, n(), probs))
      },

      ### GC2: Suicidio, homicidio, otras CE
      paso1 == "GC2-SHCE" ~ {
        causas <- freq_ce$paso2.1[
          freq_ce$sexo == cur_group()$sexo &
            freq_ce$grupo_edad == cur_group()$grupo_edad &
            str_detect(freq_ce$paso2.1, "Sui|Hom|CE")
        ]

        probs <- freq_ce$pct[
          freq_ce$sexo == cur_group()$sexo &
            freq_ce$grupo_edad == cur_group()$grupo_edad &
            str_detect(freq_ce$paso2.1, "Sui|Hom|CE")
        ]

        rep(causas, rmultinom(1, n(), probs))
      },

      ### Valor por defecto
      .default = paso2.2
    ),

    ### Variables de agrupamiento
    .by = c(sexo, grupo_edad)
  )


## Paso 3.2: Redistribuir GC2 inespecíficos -----
set.seed(123)

recod_defun <- recod_defun |>
  mutate(
    paso3.2 = case_when(
      ### GC2: X59.0 - X59.9
      paso1 == "GC2-CEEC" ~ {
        causas <- freq_cd$paso2.1[
          freq_cd$sexo == cur_group()$sexo &
            freq_cd$grupo_edad == cur_group()$grupo_edad &
            !str_detect(freq_cd$paso2.1, "Dia|ECV|ERC|Neo")
        ]

        probs <- freq_cd$pct[
          freq_cd$sexo == cur_group()$sexo &
            freq_cd$grupo_edad == cur_group()$grupo_edad &
            !str_detect(freq_cd$paso2.1, "Dia|ECV|ERC|Neo")
        ]

        rep(causas, rmultinom(1, n(), probs))
      },

      ### GC2 generales
      paso3.1 == "GC2" & paso1 != "GC2-CEEC" ~ {
        causas <- freq_cd$paso2.1[
          freq_cd$sexo == cur_group()$sexo &
            freq_cd$grupo_edad == cur_group()$grupo_edad
        ]

        probs <- freq_cd$pct[
          freq_cd$sexo == cur_group()$sexo &
            freq_cd$grupo_edad == cur_group()$grupo_edad
        ]

        rep(causas, rmultinom(1, n(), probs))
      },

      ### Valor por defecto
      .default = paso3.1
    ),
    .by = c(sexo, grupo_edad)
  )


# Paso 4: Redistribuir GC1 -----------------------------------------------
## Paso 4.1: Redistribuir GC1 específicos -----
set.seed(123)

recod_defun <- recod_defun |>
  mutate(
    paso4.1 = case_when(
      ### GC1: ERC
      paso1 == "GC1-ERC" ~ "ERC",

      ### GC1: Homicidio, suicidio, otras CE
      paso1 == "GC1-HSCE" ~ {
        causas <- freq_ce$paso2.1[
          freq_ce$sexo == cur_group()$sexo &
            freq_ce$grupo_edad == cur_group()$grupo_edad &
            str_detect(freq_ce$paso2.1, "Sui|Hom|CE")
        ]

        probs <- freq_ce$pct[
          freq_ce$sexo == cur_group()$sexo &
            freq_ce$grupo_edad == cur_group()$grupo_edad &
            str_detect(freq_ce$paso2.1, "Sui|Hom|CE")
        ]

        rep(causas, rmultinom(1, n(), probs))
      },

      ### Valor por defecto
      .default = paso3.2
    ),
    .by = c(sexo, grupo_edad)
  )


## Paso 4.2: Redistribuir GC1 inespecíficos -----
set.seed(123)

recod_defun <- recod_defun |>
  mutate(
    paso4.2 = case_when(
      paso1 == "GC1" ~ {
        causas <- freq_cd$paso2.1[
          freq_cd$sexo == cur_group()$sexo &
            freq_cd$grupo_edad == cur_group()$grupo_edad
        ]

        probs <- freq_cd$pct[
          freq_cd$sexo == cur_group()$sexo &
            freq_cd$grupo_edad == cur_group()$grupo_edad
        ]

        rep(causas, rmultinom(1, n(), probs))
      },

      ### Valor por defecto
      .default = paso4.1
    ),
    .by = c(sexo, grupo_edad)
  )


# Reagrupar datos --------------------------------------------------------
recod_defun <- recod_defun |>
  select(
    anio,
    mes,
    region_deis,
    jurisdiccion,
    sexo,
    grupo_edad,
    grupo_causa,
    paso2 = paso2.2,
    paso3 = paso3.2,
    paso4 = paso4.2
  ) |>

  # Ordenar grupos de causas
  mutate(across(
    .cols = contains(c("causa", "paso")),
    .fns = ~ fct_relevel(
      .x,
      "Neoplasias",
      "Homicidio",
      "Suicidio",
      "Tránsito",
      "CMNN",
      "Otras ENT",
      "Otras CE",
      after = 3
    )
  ))


# Exportar datos limpios -------------------------------------------------
export(recod_defun, file = "clean/arg_defun_mes_2010-2022_recod.rds")

## Limpiar environment ----
rm(list = ls())
pacman::p_unload("all")
