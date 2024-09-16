# setup ----

if (!require("here")) install.packages("here")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("googlesheets4")) install.packages("googlesheets4")
if (!require("janitor")) install.packages("janitor")

# Cargar datos ----

raw_data <-
  googlesheets4::read_sheet(
    "1x2GzY7A43BgOOZSg8UMECsgOnKYYl1WsR5-Af35pm-s"
  )

# Limpiar datos ---

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
      "ingresa_tu_correo_si_quieres_recibir_los_resultados_del_analisis_que_hagamos_de_estos_datos"
    )
  )

# Darle forma para ethnobotanyR ----

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
      numero == 7 ~ "papa",
      numero == 9 ~ "papa criolla",
      numero == 11 ~ "jabuticaba",
      numero == 13 ~ "tomate chonto",
      numero == 15 ~ "tomate variedad",
      numero == 17 ~ "balazo",
      numero == 19 ~ "frijol cargamanto",
      numero == 21 ~ "frijol variedad",
      .default = numero
    )
  ) |>
  dplyr::rename(
    sp_name = numero
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
  janitor::clean_names()


ethnobotanyr_data |>
  ggplot2::ggplot() +
  ggplot2::aes(
    x = sp_name
  ) +
  ggplot2::geom_bar()
