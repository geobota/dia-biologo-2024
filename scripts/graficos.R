# Porcentajes de reconomiento ----

#porcentaje-especie
porcentaje_especie <-
  ethnobotanyr_data |>
  janitor::tabyl(
    sp_name
  ) |>
  dplyr::as_tibble() |>
  dplyr::mutate(
    value = percent,
    percent = scales::percent(percent),
    proporcion = n / length(vector_informantes),
    porcentaje = scales::percent(proporcion),
    sp_name = forcats::fct_reorder(sp_name, n)
  )

## Gráfico % reconocimiento----

# fig-porcentaje-especie
fig_porcentaje_especie <-
  porcentaje_especie |>
  ggplot2::ggplot() +
  ggplot2::aes(
    x = sp_name,
    y = proporcion * 100,
    label = porcentaje
  ) +
  ggplot2::geom_bar(
    stat = "identity",
    fill = "#0072B2"
  ) +
  ggplot2::geom_text(
    hjust = 1.1,
    color = "#ffffff"
  ) +
  # ggplot2::scale_y_continuous(
  #   limits = c(0, 17)
  # ) +
  ggplot2::coord_flip() +
  ggplot2::labs(
    x = NULL,
    y = "Porcentaje de informantes (%)"
  )


# Partes usadas por especie ----

## Preparando los datos ----

# usos-data-long
usos_data_long <-
  ethnobotanyr_data |>
  dplyr::select(
    sp_name,
    ensalada:fritos
  ) |>
  tidyr::pivot_longer(
    -sp_name,
    names_to = "uso",
    values_to = "valor"
  ) |>
  dplyr::mutate(
    uso = uso |> stringr::str_replace_all("_", " ") |> stringr::str_to_sentence()
  )

#
# fig_parte_usada_data <-
#   usos_data_long |>
#   dplyr::filter(
#     # especie %in% vector_especies_principales,
#     uso != 0
#   ) |>
#   ggplot2::ggplot() +
#   ggplot2::aes(
#     x = uso
#   ) +
#   ggplot2::geom_bar(
#   ) +
#   ggplot2::facet_wrap(
#     ~ base::factor(sp_name),
#     ncol = 2,
#     scales = "free_y"
#   ) +
#   ggplot2::guides(
#     x = ggplot2::guide_axis(angle = 45)
#   ) +
#   ggplot2::theme_classic() +
#   ggplot2::theme(
#     # strip.text = ggplot2::element_text(face = "italic")
#   )

## Porcentajes de uso ----

# usos-percent
usos_percent <-
  usos_data_long |>
  dplyr::group_by(
    sp_name,
    uso
  ) |>
  dplyr::summarise(
    valor_total = base::sum(valor)
  ) |>
  dplyr::ungroup() |>
  dplyr::group_by(
    sp_name
  ) |>
  dplyr::mutate(
    percentage = (valor_total / base::sum(valor_total)) * 100
  ) |>
  dplyr::ungroup()

## Gráfico ----

# fig-usos-percent
fig_usos_percent <-
  usos_percent |>
  ggplot2::ggplot() +
  ggplot2::aes(
    x = uso,
    y = percentage,
    fill = str_to_sentence(uso)
  ) +
  ggplot2::geom_bar(
    stat = "identity"
  ) +
  ggplot2::facet_wrap(
    ~ sp_name,
    ncol = 2,
    scales = "free_y"
  ) +
  ggplot2::labs(
    x = NULL,
    y = "Porcentaje de uso (%)",
    fill = "Uso de la planta"
  ) +
  ggplot2::guides(
    x = ggplot2::guide_axis(angle = 45)
  ) +
  ggplot2::theme(
    legend.position = "bottom",
    legend.title = ggplot2::element_blank(),
    # legend.text = ggplot2::element_text(size = 12),
    axis.text.x = ggplot2::element_blank()
  ) +
  ggthemes::scale_fill_colorblind()


# usos-data-wide
usos_data_wide <-
  ethnobotanyr_data |>
  dplyr::select(
    informant,
    sp_name,
    ensalada:fritos
  ) |>
  dplyr::arrange(
    informant
  )

# # fig_ethnoChord_partes_especie <-
#   ethnobotanyR::ethnoChord(
#     usos_data_wide,
#     by = "sp_name"
#   )
