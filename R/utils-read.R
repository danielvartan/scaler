load_data <- function(orcid = NULL, email = NULL, col_names = TRUE,
                      file = utils::choose.files()) {
    checkmate::assert_string(orcid,
                             "^[0-9X]{4}-[0-9X]{4}-[0-9X]{4}-[0-9X]{4}",
                             null.ok = TRUE)
    checkmate::assert_string(email, null.ok = TRUE)
    checkmate::assert_flag(col_names)
    checkmate::assert_file_exists(file)
    gutils:::require_pkg("readr")

    out <- file %>%
        readr::read_csv(na = c("", "NA"),
                        col_names = col_names,
                        col_types = readr::cols(.default = "c"))

    if (isFALSE(col_names)) out <- out %>% dplyr::slice(-1)
    if (!is.null(orcid)) out <- out %>% filter_data(3, orcid)
    if (!is.null(email)) out <- out %>% filter_data(2, email)

    out
}

write_data <- function(data, dir = utils::choose.dir(), file_name) {
    checkmate::assert_tibble(data, min.rows = 1)
    checkmate::assert_directory_exists(dir)
    checkmate::assert_string(file_name)
    gutils:::require_pkg("readr")

    file <- normalizePath(file.path(dir, file_name), mustWork = FALSE)

    readr::write_csv(data, file)
}

filter_data <- function(data, col_index, values) {
    checkmate::assert_tibble(data, min.rows = 1)
    checkmate::assert_choice(col_index, seq_len(ncol(data)))
    checkmate::assert_character(values, min.len = 1)

    ## R CMD Check variable bindings fix (see <https://bit.ly/3z24hbU>)
    .data <- NULL

    out <- dplyr::tibble()
    var <- names(data)[col_index]

    for (i in values) {
        data_i <- data %>% dplyr::filter(.data[[var]] == i)
        out <- out %>% dplyr::bind_rows(data_i)
    }

    var <- names(data)[1]
    out <- out %>% dplyr::arrange(lubridate::dmy_hms(.data[[var]]))

    out
}
