parte_usada_data_long <-
  ethnobotanyr_data |>
  dplyr::select(
    sp_name,
    ensalada:fritos
  ) |>
  tidyr::pivot_longer(
    -sp_name,
    names_to = "parte",
    values_to = "uso"
  )
#
# fig_parte_usada_data <-
#   parte_usada_data_long |>
#   dplyr::filter(
#     # especie %in% vector_especies_principales,
#     uso != 0
#   ) |>
#   ggplot2::ggplot() +
#   ggplot2::aes(
#     x = parte
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


parte_usada_percent <-
  parte_usada_data_long |>
  dplyr::group_by(
    sp_name,
    parte
  ) |>
  dplyr::summarise(
    uso_total = base::sum(uso)
  ) |>
  dplyr::ungroup() |>
  dplyr::group_by(
    sp_name
  ) |>
  dplyr::mutate(
    percentage = (uso_total / base::sum(uso_total)) * 100
  )

fig_parte_usada_percent <-
  parte_usada_percent |>
  ggplot2::ggplot() +
  ggplot2::aes(
    x = parte,
    y = percentage,
    fill = parte
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
    x = "Uso de la planta",
    y = "Porcentaje de uso (%)",
    fill = "Uso de la planta"
  ) +
  ggplot2::guides(
    x = ggplot2::guide_axis(angle = 45)
  ) +
  ggplot2::theme_classic() +
  # ggplot2::theme(
  #   legend.position = "bottom"
  # ) +
  ggthemes::scale_fill_colorblind()


parte_usada_data <-
  ethnobotanyr_data |>
  dplyr::select(
    informant,
    sp_name,
    ensalada:fritos
  ) |>
  dplyr::arrange(
    informant
  )

# fig_ethnoChord_partes_especie <-
#   ethnobotanyR::ethnoChord(
#     parte_usada_data,
#     by = "sp_name"
#   )
