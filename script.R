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
      numero == 15 ~ "tomate amarillo",
      numero == 17 ~ "balazo",
      numero == 19 ~ "frijol cargamanto",
      numero == 21 ~ "frijol petaco",
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


porcentaje_especie <-
  ethnobotanyr_data |>
  janitor::tabyl(
    sp_name
  ) |>
  dplyr::as_tibble() |>
  dplyr::mutate(
    value = percent,
    percent = scales::percent(percent),
    sp_name = forcats::fct_reorder(sp_name, n)
  )

#### Gráfico ----

# fig-porcentaje-especie
fig_porcentaje_especie <-
  porcentaje_especie |>
  ggplot2::ggplot() +
  ggplot2::aes(
    x = sp_name,
    y = value * 100,
    label = percent
  ) +
  ggplot2::geom_bar(
    stat = "identity",
    fill = "#0072B2"
  ) +
  ggplot2::geom_text(
    hjust = -0.1
  ) +
  # ggplot2::scale_y_continuous(labels = scales::label_percent()) +
  ggplot2::coord_flip() +
  ggplot2::labs(
    x = NULL,
    y = "Abundancia relativa (%)"
  ) +
  ggplot2::theme_classic() +
  ggplot2::theme(
    # axis.text.y = ggplot2::element_text(face = "italic")
  )
