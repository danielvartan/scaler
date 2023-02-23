mctq <- function(file = utils::choose.files()) {
    checkmate::assert_file_exists(file)

    # R CMD Check variable bindings fix (see: http://bit.ly/3bliuam) -----

    timestamp <- cpf <- NULL
    work <- wd <- fd <- NULL
    bt_w <- sprep_w <- slat_w <- so_w <- se_w <- si_w <- gu_w <- NULL
    alarm_w <- wake_before_w <- sd_w <- tbt_w <- le_w <- msw <- NULL
    bt_f <- sprep_f <- slat_f <- so_f <- se_f <- si_f <- gu_f <- NULL
    alarm_f <- reasons_f <- reasons_why_f <- sd_f <- tbt_f <- le_f <- NULL
    msf <- sd_week <- sloss_week <- le_week <- msf_sc <- sjl_rel <- sjl <- NULL

    file %>%
        readr::read_csv(na = c("", "NA"),
                        col_names = FALSE,
                        col_types = readr::cols(.default = "c"),
                        trim_ws = TRUE,
                        progress = FALSE) %>%
        dplyr::slice(-1) %>%
        dplyr::select(1, 18:35, 106) %>%
        dplyr::rename_with(function(x) {
            c(
                "timestamp",
                "work", "wd", "bt_w", "sprep_w", "slat_w", "se_w", "si_w",
                "alarm_w", "wake_before_w", "le_w",

                "bt_f", "sprep_f", "slat_f", "se_f", "si_f", "alarm_f",
                "reasons_why_f", "le_f",

                "cpf"
                )
            }
            ) %>%
        dplyr::filter(!is.na(timestamp)) %>%
        dplyr::mutate(
            timestamp = lubridate::dmy_hms(timestamp, tz = "America/Sao_Paulo"),
            wd = as.integer(wd),
            reasons_f = dplyr::case_when(
                is.na(reasons_why_f) | reasons_why_f == "" ~ as.logical(NA),
                grepl("^n\u00e3o$|^nao$", reasons_why_f, ignore.case = TRUE) ~
                    as.logical(NA),
                TRUE ~ TRUE)
            ) %>%
        dplyr::mutate(
            dplyr::across(dplyr::matches("^bt_|^sprep_|^se_"),
                          ~ hms::parse_hms(.x)),
            dplyr::across(dplyr::matches("^slat_|^si_"),
                          ~ lubridate::dminutes(as.integer(.x))),
            dplyr::across(dplyr::matches("^le_"),
                          ~ lubridate::as.duration(
                              hms::as_hms(
                                  lubridate::parse_date_time(.x, "HMS")
                                  )
                              )
                          ),
            dplyr::across(dplyr::matches("^work$|^alarm_|^wake_before_w$"),
                          ~ dplyr::case_when(
                              tolower(.x) == "sim" ~ TRUE,
                              tolower(.x) == "n\u00e3o" ~ FALSE,
                              TRUE ~ as.logical(NA)))
        ) %>%
        dplyr::mutate(
            fd = mctq::fd(wd),
            so_w = mctq::so(sprep_w, slat_w),
            gu_w = mctq::gu(se_w, si_w),
            sd_w = mctq::sdu(so_w, se_w),
            tbt_w = mctq::tbt(bt_w, gu_w),
            msw = mctq::msl(so_w, sd_w),

            so_f = mctq::so(sprep_f, slat_f),
            gu_f = mctq::gu(se_f, si_f),
            sd_f = mctq::sdu(so_f, se_f),
            tbt_f = mctq::tbt(bt_f, gu_f),
            msf = mctq::msl(so_f, sd_f),

            sd_week = mctq::sd_week(sd_w, sd_f, wd),
            msf_sc = mctq::msf_sc(msf, sd_w, sd_f, sd_week, alarm_f),
            sloss_week = mctq::sloss_week(sd_w, sd_f, wd),
            sjl_rel = mctq::sjl_rel(msw, msf),
            sjl = abs(sjl_rel),
            le_week = mctq::le_week(le_w, le_f, wd)
            ) %>%
        dplyr::relocate(
            timestamp, cpf,

            work, wd, fd,

            bt_w, sprep_w, slat_w, so_w, se_w, si_w, gu_w, alarm_w,
            wake_before_w, sd_w, tbt_w, le_w, msw,

            bt_f, sprep_f, slat_f, so_f, se_f, si_f, gu_f, alarm_f,
            reasons_f, reasons_why_f, sd_f, tbt_f, le_f, msf,

            sd_week, sloss_week, le_week, msf_sc, sjl_rel, sjl
            ) %>%
        dplyr::arrange(timestamp)
}

# test <- mctq()
# as.list(mctq::pretty_mctq(test))
