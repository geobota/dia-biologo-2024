



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


prueba <-
  parte_usada_data_long |>
  # dplyr::filter(
  #   # especie %in% vector_especies_principales,
  #   uso != 0
  # ) |>
  dplyr::group_by(
    sp_name,
    parte
  ) |>
  dplyr::rename(
    value = uso
  ) |>
  dplyr::summarise(
    value = base::sum(value, na.rm = TRUE)
  ) |>
  dplyr::ungroup(
  ) |>
  dplyr::mutate(
    percent = scales::percent(value / base::sum(value), accuracy = 0.01),
    value = (value / base::sum(value)) * 100,
    pollen = forcats::fct_reorder(sp_name, value)
  )

prueba |>
  ggplot2::ggplot(
    ggplot2::aes(
      x = parte,
      y = value,
      label = percent
    )
  ) +
  ggplot2::geom_bar(
    stat = "identity",
    fill = "#0072B2"
  ) +
  ggplot2::facet_wrap(
    ~ base::factor(sp_name),
    ncol = 2,
    scales = "free_y",
  ) +
  ggplot2::theme_classic()


parte_usada_percent <- parte_usada_data_long %>%
  group_by(sp_name, parte) %>%
  summarise(uso_total = sum(uso)) %>%
  ungroup() %>%
  group_by(sp_name) %>%
  mutate(percentage = (uso_total / sum(uso_total)) * 100)

# Crear el gráfico con ggplot
ggplot(parte_usada_percent, aes(x = sp_name, y = percentage, fill = parte)) +
  geom_bar(stat = "identity", position = "fill") +
  coord_flip() +
  labs(x = "Especies", y = "Porcentaje de uso", fill = "Parte de la planta") +
  theme_minimal()

parte_usada_percent <- parte_usada_data_long %>%
  group_by(sp_name, parte) %>%
  summarise(uso_total = sum(uso)) %>%
  ungroup() %>%
  group_by(sp_name) %>%
  mutate(percentage = (uso_total / sum(uso_total)) * 100)

# Crear el gráfico con ggplot y facet_wrap
ggplot(parte_usada_percent, aes(x = parte, y = percentage)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ sp_name) +
  labs(x = "Uso de la planta", y = "Porcentaje de uso (%)") +
  theme_minimal()
