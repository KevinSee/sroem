#' @title Estimate Correlation Between Reaches
#'
#' @description Estimates correlation between reaches, to be used when calculating standard error across multiple reaches so reaches aren't necessarily treated independently
#'
#' @author Kevin See
#'
#' @inheritParams estimate_redds
#' @param date_nm quoted name of column in {redd_df} listing date of each survey
#' @param cor_redd_nm quoted name of column in {redd_df} listing number of redds found during that survey, to be used in the correlation
#' @param reach_nm quoted name of column in {redd_df} listing name of reach
#' @param make_na_zero should NAs in the correlation matrix be set to 0? Default is `TRUE`
#'
#' @import dplyr lubridate tidyr
#' @return dataframe
#' @export

correlate_rchs <- function(redd_df = NULL,
                           date_nm = "SurveyDate",
                           cor_redd_nm = "NewRedds",
                           reach_nm = "Reach",
                           use = "pairwise.complete.obs",
                           make_na_zero = TRUE,
                           ...) {
  if (is.null(redd_df)) {
    stop("redd data must be supplied")
  }

  n_rchs <-
    redd_df |>
    dplyr::pull({{ reach_nm }}) %>%
    dplyr::n_distinct()

  if (n_rchs == 1) {
    rch_nm <-
      redd_df |>
      dplyr::pull({{ reach_nm }}) %>%
      unique()

    cor_mat <-
      matrix(1, 1, 1,
        dimnames = list(
          rch_nm,
          rch_nm
        )
      )
  } else {
    # check the class of survey date column
    date_class <-
      redd_df |>
      dplyr::pull({{ date_nm }}) #|>
    #   class()
    # if (date_class == "Date") {
    if (inherits(date_class, "Date")) {
      redd_df <-
        redd_df |>
        dplyr::mutate(across(
          {{ date_nm }},
          as.POSIXct
        ))
    }

    cor_mat <-
      redd_df |>
      dplyr::rename(
        redds = {{ cor_redd_nm }},
        reach = {{ reach_nm }},
        surv_date = {{ date_nm }}
      ) |>
      dplyr::mutate(surv_period = lubridate::week(surv_date)) |>
      dplyr::group_by(reach, surv_period) %>%
      dplyr::summarize(
        dplyr::across(redds,
          mean,
          na.rm = T
        )
      ) %>%
      tidyr::pivot_wider(
        names_from = reach,
        values_from = redds,
        names_sort = T
      ) |>
      dplyr::select(-surv_period) |>
      # corrr::correlate(...)
      stats::cor(
        use = use,
        ...
      )
  }

  if (make_na_zero) {
    cor_mat[is.na(cor_mat)] <- 0
  }

  return(cor_mat)
}
