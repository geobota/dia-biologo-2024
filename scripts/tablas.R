tibble_nombres_especies <-
  tibble::tribble(
    ~nombre_comun,        ~nombre_cientifico,
    "Papa sangre de sol", "Solanum tuberum var. indigena",
    "Papa criolla",       "Solanum phureja",
    "Mortiño",            "Tibouchina mollis",
    "Tomate chonto",      "Lycopersicon esculentum",
    "Tomate amarillo",    "Lycopersicon esculentum var. indet",
    "Balazo",             "Monstera deliciosa",
    "Frijol cargamanto",  "Phaseolus vulgari",
    "Frijol petaco",      "Phaseolus coccineus"
  ) |> dplyr::arrange(
    nombre_cientifico
  )

tbl_gt_nombres_especies <-
  tibble_nombres_especies |>
  gt::gt() |>
  gt::cols_label(
    nombre_comun = "Nombre común",
    nombre_cientifico = "Nombre científico"
  ) |>
  gt::tab_style(
    style = cell_text(style = "italic"),
    locations = cells_body(columns = nombre_cientifico)
  ) |>
  gt::opt_table_font(
    font = "Atkinson Hyperlegible"
  )


tbl_flextable_nombres_especies <-
  tibble_nombres_especies |>
  flextable::flextable() |>
  flextable::set_header_labels(
    nombre_comun = "Nombre común",
    nombre_cientifico = "Nombre científico"
  ) |>
  flextable::mk_par(
    j = "nombre_cientifico",
    value = flextable::as_paragraph(flextable::as_i(nombre_cientifico)),
  ) |>
  flextable::autofit() |>
  flextable::theme_apa()
