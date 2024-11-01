test_that("age() | scalar test", {
  age(
    birth_date = as.Date("1950-01-01"),
    reference_date = as.Date("2000-01-01"),
    round = FALSE
  ) %>%
    expect_equal(50)

  age(
    birth_date = as.Date("1950-01-01"),
    reference_date = as.Date("2000-01-02"),
    round = TRUE
  ) %>%
    expect_equal(50)
})

test_that("age() | vector test", {
  age(
    birth_date = c(as.Date("1950-01-01"), as.Date("1800-01-01")),
    reference_date = as.Date("2000-01-01"),
    round = FALSE
  ) %>%
    expect_equal(c(50, 200))
})

test_that("age() | error test", {
  # prettycheck:::assert_date(birth_date)
  age(birth_date = 1, reference_date = base::Sys.Date(), round = FALSE) %>%
    expect_error("Assertion on 'birth_date' failed")

  # prettycheck:::assert_multi_class(reference_date, c("Date", "POSIXt"))
  age(
    birth_date = as.Date("2020-01-01"), reference_date = 1, round = FALSE
  ) %>%
    expect_error("Assertion on 'reference_date' failed")

  # prettycheck:::assert_flag(round)
  age(
    birth_date = as.Date("2020-01-01"), reference_date = base::Sys.Date(),
    round = 1
  ) %>%
    expect_error("Assertion on 'round' failed")

  # if (!(length(reference_date) == 1) &&
  age(
    birth_date = as.Date("2020-01-01"),
    reference_date = c(base::Sys.Date(), base::Sys.Date()),
    round = TRUE
  ) %>%
    expect_error()
})
