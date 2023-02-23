tidy_sleep_diary <- function(data) {
    checkmate::assert_tibble(data, min.rows = 1)

    ## R CMD Check variable bindings fix (see <https://bit.ly/3z24hbU>)
    timestamp <- bed_time <- bed_date <- get_up_time <- get_up_date <- NULL
    int <- .data <- NULL

    # file <- raw_data("sleep-diary", "subject_id_0.csv")

    names(data) <- paste0("X", seq_len(ncol(data)))

    out <- data %>%
        dplyr::select(1, 8, 10, 17:26) %>%
        dplyr::rename_with(function(x) {
            c("timestamp", "bed_time", "get_up_time")
            },
            .cols = 1:3
            ) %>%
        dplyr::filter(!is.na(timestamp), !is.na(bed_time),
                      !is.na(get_up_time)) %>%
        dplyr::mutate(
            timestamp = lubridate::dmy_hms(timestamp),
            bed_time = hms::parse_hms(bed_time),
            get_up_time = hms::parse_hms(get_up_time),
            int = lubritime::assign_date(get_up_time, timestamp)) %>%
        # ## Remove records made more than 6 hours after waking up.
        # dplyr::filter(!hms::hms(as.numeric(int)) > lubridate::dhours(6)) %>%
        dplyr::mutate(
            get_up_date = dplyr::if_else(
                hms::as_hms(timestamp) > get_up_time,
                lubridate::date(timestamp),
                lubridate::date(timestamp) - lubridate::days(1)),
            int = lubritime::assign_date(bed_time, get_up_time),
            bed_date = dplyr::if_else(
                lubridate::day(lubridate::int_end(int)) == 1,
                get_up_date,
                get_up_date - lubridate::days(1))) %>%
        dplyr::mutate(
            bed_time =
                lubridate::as_datetime(paste(bed_date, bed_time)),
            get_up_time = lubridate::as_datetime(paste(get_up_date,
                                                       get_up_time))) %>%
        ## Remove records with more than 18h of sleep duration.
        dplyr::filter(!(get_up_time - bed_time) > lubridate::dhours(18)) %>%
        dplyr::mutate(dplyr::across(
            dplyr::starts_with("X"),
            function(x) {
                dplyr::case_when(
                    is.na(x) ~ as.POSIXct(NA),
                    hms::as_hms(bed_time) <= hms::parse_hms(x) ~
                        lubridate::as_datetime(paste0(bed_date, x)) -
                        lubridate::days(1),
                    TRUE ~ lubridate::as_datetime(paste0(bed_date, x))
                    )
                }
            ))

    naps <- list(nap_1 = c("X17", "X18"),
                 nap_2 = c("X19", "X20"),
                 nap_3 = c("X21", "X22"),
                 nap_4 = c("X23", "X24"),
                 nap_5 = c("X25", "X26"))

    binder <- dplyr::tibble()

    for (i in naps) {
        nap <- out %>%
            dplyr::select(dplyr::all_of(c("timestamp", i))) %>%
            dplyr::filter(!is.na(.data[[i[1]]]),
                          !is.na(.data[[i[2]]])) %>%
            dplyr::rename_with(
                function(x) c("timestamp", "bed_time", "get_up_time")
                )

        binder <- binder %>% dplyr::bind_rows(nap)
    }

    out <- out %>%
        dplyr::select(timestamp, bed_time, get_up_time) %>%
        dplyr::bind_rows(binder) %>%
        dplyr::arrange(timestamp)

    out
}

actstudio_sleep_diary <- function(data, dir = utils::choose.dir(),
                                  file_name = NULL) {
    checkmate::assert_data_frame(data, min.rows = 1)
    checkmate::assert_subset(c("bed_time", "get_up_time"), names(data))
    checkmate::assert_string(dir, null.ok = TRUE)
    checkmate::assert_string(file_name, null.ok = TRUE)
    gutils:::require_pkg("readr", "utils")

    if (!is.null(dir)) checkmate::assert_directory_exists(dir)

    ## R CMD Check variable bindings fix (see <https://bit.ly/3z24hbU>)
    bed_time <- get_up_time <- NULL

    out <- data %>%
        dplyr::select(bed_time, get_up_time) %>%
        dplyr::mutate(bed_time = format(bed_time, "%d/%m/%Y %T"),
                      get_up_time = format(get_up_time, "%d/%m/%Y %T")) %>%
        dplyr::rename("BED TIME" = bed_time, "GET UP TIME" = get_up_time)

    if (!is.null(file_name) && is.null(dir)) {
        file <- paste0("./inst/extdata/actigraphy/", file_name, ".txt")
        readr::write_csv2(out, file, na = "")
        invisible(out)
    } else if (!is.null(file_name) && !is.null(dir)) {
        file <- file.path(dir, file_name)
        readr::write_csv2(out, file, na = "")
        invisible(out)
    } else {
        out
    }
}

sleep_quality <- function(data) {
    checkmate::assert_tibble(data, min.rows = 1)
    gutils:::require_pkg("ggplot2")

    # R CMD Check variable bindings fix (see: http://bit.ly/3bliuam) -----
    # nolint start: object_usage_linter.
    timestamp <- bed_time <- bed_date <- get_up_time <- get_up_date <- NULL
    int <- sleep_quality_likert <- NULL
    # nolint end

    # file <- raw_data("sleep_diary", "subject_id_0.csv")

    out <- data %>%
        dplyr::select(1, 14) %>%
        dplyr::rename_with(function(x) c("timestamp", "sleep_quality")) %>%
        dplyr::filter(!is.na(timestamp)) %>%
        dplyr::mutate(timestamp = lubridate::dmy_hms(timestamp),
                      sleep_quality = factor(
                          sleep_quality,
                          levels = c("Muito ruim", "Ruim", "Boa", "Muito boa"),
                          ordered = TRUE),
                      sleep_quality_likert = as.numeric(sleep_quality))

    ggplot2::ggplot(out, ggplot2::aes(
        x = timestamp, y = sleep_quality_likert)) +
        ggplot2::geom_point() +
        ggplot2::ylim("Muito ruim", "Ruim", "Boa", "Muito boa") +
        # ggplot2::scale_x_datetime(date_breaks = "3 days") +
        ggplot2::geom_smooth(ggplot2::aes(
            x = timestamp, y = sleep_quality_likert),
            method = "lm",
            formula = y ~ x,
            color = "red") +
        ggplot2::labs(x = "Tempo", y = "Qualidade do sono subjetiva")
}
