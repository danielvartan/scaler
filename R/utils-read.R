filter_data <- function(data, col_index, values) {
  prettycheck:::assert_tibble(data, min.rows = 1)
  prettycheck:::assert_integerish(col_index)
  prettycheck:::assert_choice(col_index, seq_len(ncol(data)))
  prettycheck:::assert_character(values, min.len = 1)

  ## R CMD Check variable bindings fix (see <https://bit.ly/3z24hbU>)
  . <- .data <- NULL

  out <- dplyr::tibble()
  var <- names(data)[col_index]

  for (i in values) {
    data_i <- data %>% dplyr::filter(.[[var]] == i)
    out <- out |> dplyr::bind_rows(data_i)
  }

  var <- names(data)[1]
  out <- out |> dplyr::arrange(lubridate::dmy_hms(.data[[var]]))

  out
}
