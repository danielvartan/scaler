#' Compute year age
#'
#' @description\
#'
#' `r lifecycle::badge("experimental")`
#'
#' `age()` computes the year age of a person considering a reference date point.
#'
#' @param birth_date A [`Date`][base::Date()] object with
#' @param reference_date (optional) a [`Date`][base::Date()] or
#'   [`POSIXt`][base::POSIXct()] object indicating a reference date
#'   (default: `base::Sys.Date()`).
#' @param round (optional) a [`logical`][logical()] value indicating if the
#'  the function must return a rounded year age.
#'
#' @return A [`numeric`][base::numeric()] object with a year age.
#' @family utility functions
#' @export
#'
#' @examples
#' ## Scalar example
#'
#' birth_date <- lubridate::as_date("2000-01-01")
#' reference_date <- lubridate::as_date("2020-01-01")
#' age(birth_date, reference_date, round = FALSE)
#' #> [1] 19.99722 # Expected
#' age(birth_date, reference_date, round = TRUE)
#' #> [1] 20
#'
#' # Note that some years are not equal to exactly 365 days.
#'
#' ## Vector example
#'
#' birth_date <- lubridate::as_date(c(
#'     "1800-01-01", "1900-01-01", "2000-01-01"
#'     ))
#' reference_date <- lubridate::as_date("2020-01-01") # length == 1
#' age(birth_date, reference_date)
#' #> [1] 220.00000 120.00000  19.99722 # Expected
#'
#' birth_date <- lubridate::as_date(c(
#'     "1800-01-01", "1900-01-01", "2000-01-01"
#'     ))
#' reference_date <- lubridate::as_date(c( # same length as `birth_date`
#'     "1900-01-01", "2000-01-01", "2020-01-01"
#'     ))
#' age(birth_date, reference_date)
#' #> [1] 100.00000 100.00000  19.99722 # Expected
age <- function(birth_date, reference_date = base::Sys.Date(), round = FALSE) {
  prettycheck:::assert_date(birth_date)
  prettycheck:::assert_multi_class(reference_date, c("Date", "POSIXt"))
  prettycheck:::assert_flag(round)

  if (!(length(reference_date) == 1) &&
      !(length(birth_date) == length(reference_date))) {
    cli::cli_abort(paste0(
      "{.strong {cli::col_red('reference_date')}} must have ",
      "length {.strong 1} or the same lenght as ",
      "{.strong {cli::col_red('birth_date')}}."
    ))
  }

  reference_date <- lubridate::as_date(reference_date)

  out <- lubridate::interval(birth_date, reference_date,
                             tz = "America/Sao_Paulo") %>%
    lubridate::as.period()

  if (isFALSE(round)) {
    years <- lubridate::year(out)
    months <- lubridate::month(out) / 12
    days <- lubridate::day(out) / 30 / 12

    years + months + days
  } else {
    lubridate::year(out)
  }
}
