# Conocimientos botánicos durante el día del biólogo ----
# Hecho por Álex Espinosa-Correa

# Cargar los paquetes necesarios ----

if (!require("here")) install.packages("here")
if (!require("tidyverse")) install.packages("tidyverse")
# if (!require("googlesheets4")) install.packages("googlesheets4")
if (!require("janitor")) install.packages("janitor")
if (!require("ethnobotanyR")) install.packages("ethnobotanyR")
if (!require("gt")) install.packages("gt")
if (!require("flextable")) install.packages("flextable")
if (!require("glue")) install.packages("glue")

# Configuración ----

ggplot2::theme_set(
  ggplot2::theme_classic(
    base_size = 18,
    base_family = "Atkinson Hyperlegible"
  )
)

# Cargar los datos ----

raw_data <-
  readr::read_csv(
    here::here(
      "datos",
      "conocimientos_botanicos_responses.csv"
    )
  )

# Limpiar los datos ---

clean_data <-
  raw_data |>
  janitor::clean_names() |>
  dplyr::rename(
    informant = escribe_las_iniciales_de_tu_nombre,
    vinculo = vinculo_con_la_universidad_de_antioquia
  ) |>
  dplyr::select(
    !c(
      "timestamp",
    )
  )

# Dar forma a los datos para ethnobotanyR ----

temporal_data_01 <-
  clean_data |>
  dplyr::select(
    !starts_with("reconoces_")
  ) |>
  tidyr::pivot_longer(
    cols = dplyr::starts_with("conoce_"),
    names_to = c("pregunta", "numero"),
    names_pattern = "(.*)_(\\d+)$",
    values_to = "respuesta"
  )

temporal_data_02 <-
  temporal_data_01 |>
  dplyr::select(
    !pregunta
  ) |>
  dplyr::mutate(
    numero = dplyr::case_when(
      numero == 7 ~ "Papa sangre de fuego",
      numero == 9 ~ "Papa criolla",
      numero == 11 ~ "Monrtiño",
      numero == 13 ~ "Tomate chonto",
      numero == 15 ~ "Tomate amarillo",
      numero == 17 ~ "Balazo",
      numero == 19 ~ "Frijol cargamanto",
      numero == 21 ~ "Frijol petaco",
      .default = numero
    )
  ) |>
  dplyr::rename(
    sp_name = numero
  ) |>
  dplyr::arrange(
    sp_name
  ) |>
  stats::na.omit()

ethnobotanyr_data <-
  temporal_data_02 |>
  tidyr::separate_longer_delim(respuesta, delim = ", ") |>
  dplyr::mutate(valor = 1) |>
  tidyr::pivot_wider(
    names_from = respuesta,
    values_from = valor,
    values_fill = 0
  ) |>
  janitor::clean_names() |>
  dplyr::select(
    c(
      "informant",
      "sexo",
      "edad",
      "vinculo",
      "sp_name",
      "ensalada",
      "sopas_o_guisos",
      "bebidas",
      "harinas_y_masas",
      "salsas_o_condimentos",
      "postre_o_dulce",
      "fritos"
    )
  )

# Creación de vectores ----

vector_informantes <-
  clean_data |>
  dplyr::arrange(
    informant
  ) |>
  dplyr::pull(
    informant
  )

vectos_numero_femenino <-
  clean_data |>
  dplyr::filter(
    sexo == "Femenino"
  ) |>
  dplyr::count() |>
  dplyr::pull()

mediana_edad <-
  clean_data |>
  dplyr::summarise(
    median(edad)
  ) |>
  dplyr::pull()

vector_labels_especies <-
  c(
    "Papa sangre de sol" = "*Solanum tuberum var. indigena*",
    "Papa criolla"       = "*Solanum phureja*",
    "Mortiño"            = "*Tibouchina mollis*",
    "Tomate chonto"      = "*Lycopersicon esculentum*",
    "Tomate amarillo"    = "*Lycopersicon esculentum var. indet*",
    "Balazo"             = "*Monstera deliciosa*",
    "Frijol cargamanto"  = "*Phaseolus vulgari*",
    "Frijol petaco"      = "*Phaseolus coccineus*"
  )


vector_labels_usos <-
  c(
    "bebidas"              = "Bebidas",
    "ensalada"             = "Ensalada",
    "fritos"               = "Fritos",
    "harinas_y_masas"      = "Harina y masas",
    "postre_o_dulce"       = "Postre o dulce",
    "salsas_o_condimentos" = "Salsas o condimentos",
    "sopas_o_guisos"       = "Sopas o guisos"
  )
