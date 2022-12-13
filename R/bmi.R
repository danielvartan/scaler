bmi <- function(weight, height, number = TRUE, cat = FALSE) {
    checkmate::assert_number(weight)
    checkmate::assert_number(height)
    checkmate::assert_flag(number)
    checkmate::assert_flag(cat)

    if (isFALSE(number) && isFALSE(cat)) {
        cli::cli_abort(paste0(
            "{.strong number} and {.strong cat} cannot both be ",
            "{.strong {cli::col_red('FALSE')}}."
        ))
    }

    if (isTRUE(number) && isTRUE(cat)) {
        cli::cli_abort(paste0(
            "{.strong number} and {.strong cat} cannot both be ",
            "{.strong {cli::col_red('TRUE')}}."
        ))
    }

    bmi_number <- weight / (height ** 2)

    if (isTRUE(number)) {
        bmi_number
    } else if (isTRUE(cat)) {
        if (bmi_number < 18.5) {
            "Underweight"
        } else if (bmi_number >= 18.5 && bmi_number < 25) {
            "Normal"
        } else if (bmi_number >= 25 && bmi_number < 30) {
            "Overweight"
        } else if (bmi_number >= 30 && bmi_number < 35) {
            "Obesity class 1"
        } else if (bmi_number >= 35 && bmi_number < 40) {
            "Obesity class 2"
        } else if (bmi_number >= 40) {
            "Obesity class 3"
        }
    }
}
