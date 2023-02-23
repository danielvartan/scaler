placeholder <- function() message("Hello")

age <- function(birth_date) {
    checkmate::assert_date(birth_date, lower = lubridate::date("1900-01-01"),
                           upper = lubridate::today())

    lubridate::interval(birth_date, lubridate::today(),
                                tz = "America/Sao_Paulo") %>%
        lubridate::as.period() %>%
        lubridate::year()
}

pretty_num <- function(x, big_mark = ".", decimal_mark = ",") {
    x %>%
        round(digits = 3) %>%
        format(big.mark = big_mark, decimal.mark = decimal_mark,
               scientific = FALSE, n.small = 3)
}
