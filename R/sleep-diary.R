get_sleep_diary_type_of_day <- function(data, col_indexes = c(1, 4, 8, 10)) {
  prettycheck:::assert_tibble(data, min.rows = 1)

  prettycheck:::assert_integerish(
    col_indexes,
    lower = 1,
    len = 4,
    unique = TRUE,
    any.missing = FALSE
  )
  prettycheck:::assert_character(
    data[[col_indexes[1]]],
    pattern = paste0(
      "^(0[1-9]|[12][0-9]|3[01])/(0[1-9]|1[0-2])/[0-9]{4}", " ",
      "([0-1][0-9]|2[0-3]):[0-5][0-9]:[0-5][0-9]$"
    ))

  prettycheck:::assert_character(
    data[[col_indexes[2]]], pattern = paste0(
      "^Dia de trabalho$|^Dia livre$|^Dia de descanso$"
    )
  )

  for (i in col_indexes[-c(1, 2)]) {
    prettycheck:::assert_character(
      data[[i]],
      pattern = "^([0-1][0-9]|2[0-3]):[0-5][0-9]:[0-5][0-9]$"
    )
  }

  # R CMD Check variable bindings fix (see: https://bit.ly/3z24hbU)
  # nolint start: object_usage_linter.
  timestamp <- type_of_day <- sprep_time <- sprep_date <- se_time <- NULL
  se_date <- int <- sprep <- se <- subjective_day <- NULL
  # nolint end

  out <-
    data |>
    dplyr::select(dplyr::all_of(col_indexes)) |>
    dplyr::rename_with(
      function(x) c("timestamp", "type_of_day", "sprep_time", "se_time")
      ) |>
    tidyr::drop_na() |>
    dplyr::mutate(
      timestamp = lubridate::dmy_hms(timestamp),
      type_of_day = dplyr::case_when(
        type_of_day == "Dia livre" ~ "Dia de descanso",
        TRUE ~ type_of_day
      ),
      type_of_day = factor(
        type_of_day, levels = c("Dia de trabalho", "Dia de descanso"),
        ordered = FALSE
      ),
      sprep_time = hms::parse_hms(sprep_time),
      se_time = hms::parse_hms(se_time)
    ) |>
    dplyr::filter(
      !(duplicated(lubridate::date(timestamp)) |
          duplicated(lubridate::date(timestamp), fromLast = TRUE))
    ) |>
    dplyr::mutate(
      se_date = dplyr::if_else(
        hms::as_hms(timestamp) > se_time,
        lubridate::date(timestamp),
        lubridate::date(timestamp) - lubridate::days(1)
      ),
      int = lubritime::assign_date(sprep_time, se_time),
      sprep_date = dplyr::if_else(
        lubridate::day(lubridate::int_end(int)) == 1,
        se_date,
        se_date - lubridate::days(1)
      ),
      sprep = lubridate::as_datetime(paste(sprep_date, sprep_time)),
      se = lubridate::as_datetime(paste(se_date, se_time)),
      subjective_day = dplyr::case_when(
        (lubridate::date(timestamp) == sprep_date) &
          (sprep_time < hms::parse_hm("12:00")) ~
          lubridate::date(timestamp) - lubridate::days(1),
        (lubridate::date(timestamp) == sprep_date) &
          (sprep_time >= hms::parse_hm("12:00")) ~
          lubridate::date(timestamp) + lubridate::days(1),
        TRUE ~ lubridate::date(timestamp) - lubridate::days(1)
      )
    ) |>
    dplyr::filter(!(se - sprep) > lubridate::dhours(18)) |>
    dplyr::filter(
      !(duplicated(subjective_day) |
          duplicated(subjective_day, fromLast = TRUE))
    ) |>
    dplyr::arrange(subjective_day) |>
    dplyr::select(subjective_day, type_of_day) |>
    tsibble::as_tsibble(
      index = "subjective_day",
      regular = TRUE
    ) |>
    tsibble::fill_gaps()

  invisible(out)
}

tidy_sleep_diary <- function(data, col_indexes = c(1, 8, 10, 17:26)) {
  prettycheck:::assert_tibble(data, min.rows = 1)
  prettycheck:::assert_integerish(
    col_indexes, lower = 1, len = 13, unique = TRUE, any.missing = FALSE
  )
  prettycheck:::assert_character(
    data[[col_indexes[1]]],
    pattern = paste0(
      "^(0[1-9]|[12][0-9]|3[01])/(0[1-9]|1[0-2])/[0-9]{4}", " ",
      "([0-1][0-9]|2[0-3]):[0-5][0-9]:[0-5][0-9]$"
    ))

  for (i in col_indexes[-1]) {
    prettycheck:::assert_character(
      data[[i]],
      pattern = "^([0-1][0-9]|2[0-3]):[0-5][0-9]:[0-5][0-9]$"
    )
  }

  ## R CMD Check variable bindings fix (see <https://bit.ly/3z24hbU>)
  timestamp <- sprep_time <- sprep_date <- se_time <- se_date <- NULL
  int <- sprep <- se <- NULL

  # names(data) <- paste0("X", seq_len(ncol(data)))

  out <- data |>
    dplyr::select(dplyr::all_of(col_indexes)) |>
    dplyr::rename_with(
      function(x) c("timestamp", "sprep_time", "se_time"),
      .cols = 1:3
    ) |>
    tidyr::drop_na(timestamp, sprep_time, se_time) |>
    dplyr::mutate(
      timestamp = lubridate::dmy_hms(timestamp),
      sprep_time = hms::parse_hms(sprep_time),
      se_time = hms::parse_hms(se_time)
    ) |>
    dplyr::arrange(timestamp) |>
    dplyr::filter(
      !(duplicated(lubridate::date(timestamp)) |
          duplicated(lubridate::date(timestamp), fromLast = TRUE))
    ) |>
    dplyr::mutate(
      se_date = dplyr::if_else(
        hms::as_hms(timestamp) > se_time,
        lubridate::date(timestamp),
        lubridate::date(timestamp) - lubridate::days(1)
        ),
      int = lubritime::assign_date(sprep_time, se_time),
      sprep_date = dplyr::if_else(
        lubridate::day(lubridate::int_end(int)) == 1,
        se_date,
        se_date - lubridate::days(1)
        ),
      sprep = lubridate::as_datetime(paste(sprep_date, sprep_time)),
      se = lubridate::as_datetime(paste(se_date, se_time))
    ) |>
    dplyr::filter(!(se - sprep) > lubridate::dhours(18)) |>
    dplyr::mutate(dplyr::across(
      .cols = 4:13,
      .fns = function(x) {
        dplyr::case_when(
          is.na(x) ~ as.POSIXct(NA),
          hms::as_hms(sprep) <= hms::parse_hms(x) ~
            lubridate::as_datetime(paste0(sprep_date, x)) -
            lubridate::days(1),
          TRUE ~ lubridate::as_datetime(paste0(sprep_date, x))
        )
      }
    ))

  naps <- list(
    nap_1 = c(4, 5), nap_2 = c(6, 7), nap_3 = c(8, 9), nap_4 = c(10, 11),
    nap_5 = c(12, 13)
  )

  binder <- dplyr::tibble()

  for (i in naps) {
    nap <- out |>
      dplyr::select(c(1, i)) %>% # Don't change
      tidyr::drop_na() |>
      dplyr::rename_with(function(x) c("timestamp", "sprep", "se"))

    binder <- binder |> dplyr::bind_rows(nap)
  }

  out <- out |>
    dplyr::select(timestamp, sprep, se) |>
    dplyr::bind_rows(binder) |>
    dplyr::arrange(timestamp)

  invisible(out)
}

actstudio_sleep_diary <- function(data, file) {
  prettycheck:::assert_data_frame(data, min.rows = 1)
  prettycheck:::assert_subset(c("sprep", "se"), names(data))
  prettycheck:::assert_string(file)
  rutils:::require_pkg("readr", "utils")

  ## R CMD Check variable bindings fix (see <https://bit.ly/3z24hbU>)
  sprep <- se <- NULL

  out <- data |>
    dplyr::select(sprep, se) |>
    dplyr::mutate(
      sprep = format(sprep, "%d/%m/%Y %T"),
      se = format(se, "%d/%m/%Y %T")
    ) |>
    dplyr::rename("BED TIME" = sprep, "GET UP TIME" = se)

  readr::write_csv2(out, file, na = "")

  invisible(out)
}

sleep_quality <- function(data, col_index = 14) {
  prettycheck:::assert_tibble(data, min.rows = 1)
  prettycheck:::assert_integerish(col_index, len = 1)
  rutils:::require_pkg("ggplot2")

  # R CMD Check variable bindings fix (see: http://bit.ly/3bliuam) -----
  # nolint start: object_usage_linter.
  timestamp <- sleep_quality_likert <- NULL
  # nolint end

  out <- data |>
    dplyr::select(c(1, col_index)) |>
    dplyr::rename_with(function(x) c("timestamp", "sleep_quality")) |>
    tidyr::drop_na(timestamp) |>
    dplyr::mutate(
      timestamp = lubridate::dmy_hms(timestamp),
      sleep_quality = dplyr::case_when(
        sleep_quality == "Muito ruim" ~ "Very bad",
        sleep_quality == "Ruim" ~ "Fairly bad",
        sleep_quality == "Boa" ~ "Fairly good",
        sleep_quality == "Muito boa" ~ "Very good"
      ),
      sleep_quality = factor(
        sleep_quality,
        levels = c("Very bad", "Fairly bad", "Fairly good", "Very good"),
        ordered = TRUE
      ),
      sleep_quality_likert = as.numeric(sleep_quality)
    )

  out |>
    ggplot2::ggplot(ggplot2::aes(x = timestamp, y = sleep_quality_likert)) +
    ggplot2::geom_point() +
    ggplot2::ylim("Very bad", "Fairly bad", "Fairly good", "Very good") +
    # ggplot2::scale_x_datetime(date_breaks = "3 days") +
    ggplot2::geom_smooth(
      ggplot2::aes(x = timestamp, y = sleep_quality_likert),
      method = "lm", formula = y ~ x,  color = "red"
    ) +
    ggplot2::labs(x = "Time", y = "Subjective sleep quality")
}
