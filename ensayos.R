



# clean_data |>
#   dplyr::select(
#     id = escribe_las_iniciales_de_tu_nombre,
#     vinculo = vinculo_con_la_universidad_de_antioquia,
#     papa = reconoces_esto_6,
#     usos_papa = conoce_algun_uso_de_la_planta_mostrada_anteriormente_7,
#     papa_capira = reconoces_esto_8,
#     usos_papa_capira = conoce_algun_uso_de_la_planta_mostrada_anteriormente_9
#   )

# tidy_data <-
#   clean_data |>
#   tidyr::pivot_longer(
#     cols = dplyr::starts_with("reconoces_") | dplyr::starts_with("conoce_"),
#     names_to = c("pregunta", "numero"),
#     names_pattern = "(.*)_(\\d+)$",
#     values_to = "respuesta"
#   )


tidy_data <-
  clean_data |>
  # dplyr::select(
  #   !starts_with("conoce_")
  # ) |>
  tidyr::pivot_longer(
    cols = dplyr::starts_with("reconoces_"),
    names_to = c("pregunta", "numero"),
    names_pattern = "(.*)_(\\d+)$",
    values_to = "respuesta"
  )

tidy_data_2 <-
  tidy_data |>
  dplyr::mutate(
    numero = dplyr::case_when(
      numero == 6 ~ "papa",
      numero == 8 ~ "papa criolla",
      numero == 10 ~ "jabuticaba",
      numero == 12 ~ "tomate chonto",
      numero == 14 ~ "tomate variedad",
      numero == 16 ~ "balazo",
      numero == 18 ~ "frijol cargamanto",
      numero == 20 ~ "frijol variedad",
      .default = numero
    )
  )

tidy_data_3 <-
  tidy_data_2 |>
  tidyr::pivot_longer(
    cols = dplyr::starts_with("conoce_"),
    names_to = "preguta_uso",
    values_to = "uso"
  )
