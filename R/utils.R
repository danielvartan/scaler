placeholder <- function() message("Hello")

pretty_num <- function(x, big_mark = ".", decimal_mark = ",") {
    x %>%
        round(digits = 3) %>%
        format(big.mark = big_mark, decimal.mark = decimal_mark,
               scientific = FALSE, n.small = 3)
}
