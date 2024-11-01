#' Compute the Pittsburgh Sleep Quality Index (PSQI)
#'
#' __UNDER DEVELOPMENT__
#'
#' @export
psqi <- function() {
  invisible(NULL)
}

tidy_psqi <- function(file = utils::choose.files()) {
  prettycheck:::assert_file_exists(file)

  # R CMD Check variable bindings fix (see: http://bit.ly/3bliuam) -----

  timestamp <- cpf <- v1 <- v2 <- v3 <- v4 <- NULL

  file %>%
    readr::read_csv(na = c("", "NA"),
                    col_names = FALSE,
                    col_types = readr::cols(.default = "c"),
                    trim_ws = TRUE,
                    progress = FALSE) %>%
    dplyr::slice(-1) %>%
    dplyr::select(1, 21:46, 78) %>%
    dplyr::rename_with(function(x) {
      c(
        "timestamp", "v1", "v2", "v3", "v4", "v5_a", "v5_b", "v5_c",
        "v5_d", "v5_e", "v5_f", "v5_g", "v5_h", "v5_i", "v5_j_other",
        "v5_j", "v6", "v7", "v8", "v9", "v10", "v10_a", "v10_b",
        "v10_c", "v10_d", "v10_e_other", "v10_e", "cpf"
      )
    }
    ) %>%
    dplyr::filter(!is.na(timestamp)) %>%
    dplyr::mutate(
      timestamp = lubridate::dmy_hms(timestamp),
      cpf = as.numeric(cpf),
      v1 = hms::parse_hms(v1),
      v2 = lubridate::dminutes(as.numeric(v2)),
      v3 = hms::parse_hms(v3),
      v4 = lubridate::as.duration(hms::parse_hms(v4)),
      v6 = factor(dplyr::case_when(
        v6 == "Muito boa" ~ "Very good",
        v6 == "Boa" ~ "Fairly good",
        v6 == "Ruim" ~ "Fairly bad",
        v6 == "Muito ruim" ~ "Very bad",
        TRUE ~ as.character(NA)),
        levels = c("Very good", "Fairly good", "Fairly bad",
                   "Very bad"),
        ordered = TRUE),
      v9 = factor(dplyr::case_when(
        v9 == "Nenhuma dificuldade" ~ "No problem at all",
        v9 == "Um problema leve" ~ "Only a very slight problem",
        v9 == "Um problema razo\u00e1vel" ~ "Somewhat of a problem",
        v9 == "Um grande problema" ~ "A very big problem",
        TRUE ~ as.character(NA)),
        levels = c("No problem at all", "Only a very slight problem",
                   "Somewhat of a problem", "A very big problem"),
        ordered = TRUE),
      v10 = dplyr::case_when(
        v10 == "N\u00e3o" ~ "No bed partner or roommate",
        v10 == "Parceiro ou colega, mas em outro quarto" ~
          "Partner/roommate in other room",
        v10 == "Parceiro no mesmo quarto, mas n\u00e3o na mesma cama" ~
          "Partner in same room, but not same bed",
        v10 == "Parceiro na mesma cama" ~ "Partner in same bed",
        TRUE ~ as.character(NA))) %>%
    dplyr::mutate(dplyr::across(
      dplyr::matches("^v[0-9]+_[a-z]$|^v[7-8]$"),
      function(x) {
        dplyr::case_when(
          x == "Nenhuma no \u00faltimo m\u00eas" ~
            "Not during the past month",
          x == "Menos de 1 vez/semana" ~ "Less than once a week",
          x == "1 ou 2 vezes/semana" ~ "Once or twice a week",
          x == "3 ou mais vezes/semana" ~
            "Three or more times a week",
          TRUE ~ as.character(NA)
        ) %>%
          factor(
            levels = c(
              "Not during the past month",
              "Less than once a week",
              "Once or twice a week",
              "Three or more times a week"
            ),
            ordered = TRUE
          )
      }
    )) %>%
    dplyr::relocate(cpf, .after = timestamp) %>%
    dplyr::arrange(timestamp)
}

validate_psqi <- function(data) {
  invisible(NULL)
}

psqi_component_1 <- function(data) {
  invisible(NULL)
}

psqi_component_2 <- function(data) {
  invisible(NULL)
}

psqi_global_score <- function(data) {
  invisible(NULL)
}
