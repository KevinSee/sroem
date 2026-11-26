#' Estimate correlations between reaches
#'
#' Calculates correlations of weekly mean observed redd counts or supplied
#' signed estimation errors. Undefined correlations are set to zero, with a
#' message identifying the affected reach pairs.
#'
#' @param redd_df Data frame containing reach identifiers, survey dates, and
#'   the chosen correlation variable.
#' @param date_nm,cor_redd_nm,reach_nm Names of the survey date, observed
#'   redd count, and reach columns.
#' @param use Missing-observation method passed to [stats::cor()].
#' @param make_na_zero Must be `TRUE`: undefined correlations are treated as
#'   independent. Retained to give existing callers an informative error if
#'   they explicitly request the previous `FALSE` behavior.
#' @param correlation_data Whether to correlate `"counts"` or `"errors"`.
#' @param error_nm Name of a numeric column containing signed estimation
#'   errors, required when `correlation_data = "errors"`. Errors should use
#'   a consistent definition and scale across reaches; uncertainty estimates
#'   alone do not provide estimation errors.
#' @param year_mode `"pooled"` distinguishes calendar year and week;
#'   `"per_year"` calculates a separate matrix for each year; `"legacy"`
#'   averages observations by week across years.
#' @param ... Additional arguments passed to [stats::cor()], such as `method`.
#'
#' @details Weekly values are means of the supplied observations. When
#'   correlating errors, callers must supply appropriate signed error
#'   observations; this function cannot determine true errors from model
#'   standard errors. Correlations of survey-level errors are not necessarily
#'   correlations of reach-level abundance estimation errors.
#'
#' @return A reach-named correlation matrix, or a named list of matrices when
#'   `year_mode = "per_year"`. Diagonal entries are one.
#' @export
correlate_rchs <- function(redd_df = NULL,
                           date_nm = "SurveyDate",
                           cor_redd_nm = "NewRedds",
                           reach_nm = "Reach",
                           use = "pairwise.complete.obs",
                           make_na_zero = TRUE,
                           correlation_data = c("counts", "errors"),
                           error_nm = NULL,
                           year_mode = c("pooled", "per_year", "legacy"),
                           ...) {
  if (is.null(redd_df)) {
    stop("redd data must be supplied.", call. = FALSE)
  }
  if (!identical(make_na_zero, TRUE)) {
    stop("make_na_zero must be TRUE; undefined correlations assume independence.",
      call. = FALSE
    )
  }
  correlation_data <- match.arg(correlation_data)
  year_mode <- match.arg(year_mode)
  use <- match.arg(use, c(
    "everything", "all.obs", "complete.obs", "na.or.complete",
    "pairwise.complete.obs"
  ))
  value_nm <- if (correlation_data == "counts") cor_redd_nm else error_nm
  if (!is.character(value_nm) || length(value_nm) != 1L ||
    is.na(value_nm) || !nzchar(value_nm)) {
    stop("A column name must be supplied for the selected correlation data.",
      call. = FALSE
    )
  }
  missing <- setdiff(c(date_nm, reach_nm, value_nm), names(redd_df))
  if (length(missing) > 0) {
    stop("Missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  if (!is.numeric(redd_df[[value_nm]]) ||
    any(is.infinite(redd_df[[value_nm]]))) {
    stop("Correlation values must be numeric and must not be infinite.",
      call. = FALSE
    )
  }
  if (nrow(redd_df) == 0L || anyNA(redd_df[[reach_nm]]) ||
    anyNA(redd_df[[date_nm]])) {
    stop("Surveys must have non-missing reach identifiers and dates.",
      call. = FALSE
    )
  }

  weekly <- tibble::tibble(
    reach = as.character(redd_df[[reach_nm]]),
    year = lubridate::year(redd_df[[date_nm]]),
    week = lubridate::week(redd_df[[date_nm]]),
    value = redd_df[[value_nm]]
  )
  if (anyNA(weekly$year) || anyNA(weekly$week)) {
    stop("Survey dates must be valid dates.", call. = FALSE)
  }
  if (year_mode == "legacy") {
    weekly$year <- 0L
  }
  weekly <- weekly |>
    dplyr::group_by(year, week, reach) |>
    dplyr::summarise(value = mean(value, na.rm = TRUE), .groups = "drop")

  matrix_for <- function(data, label = NULL) {
    periods <- data |>
      dplyr::distinct(year, week)
    reaches <- sort(unique(data$reach))
    values <- matrix(NA_real_, nrow(periods), length(reaches),
      dimnames = list(NULL, reaches)
    )
    period_ids <- match(
      paste(data$year, data$week), paste(periods$year, periods$week)
    )
    values[cbind(period_ids, match(data$reach, reaches))] <- data$value
    mat <- diag(length(reaches))
    dimnames(mat) <- list(reaches, reaches)
    if (length(reaches) < 2L) {
      return(mat)
    }

    if (use %in% c("complete.obs", "na.or.complete")) {
      values <- values[stats::complete.cases(values), , drop = FALSE]
    }
    all_obs_missing <- use == "all.obs" && anyNA(values)
    undefined <- character()
    for (i in seq_len(length(reaches) - 1L)) {
      for (j in seq.int(i + 1L, length(reaches))) {
        pair <- values[, c(i, j), drop = FALSE]
        if (use == "pairwise.complete.obs") {
          pair <- pair[stats::complete.cases(pair), , drop = FALSE]
        }
        can_correlate <- !all_obs_missing && nrow(pair) >= 2L &&
          !anyNA(pair) &&
          length(unique(pair[, 1])) > 1L && length(unique(pair[, 2])) > 1L
        value <- if (can_correlate) {
          stats::cor(pair[, 1], pair[, 2], use = "everything", ...)
        } else {
          NA_real_
        }
        if (!is.finite(value)) {
          undefined <- c(undefined, paste(reaches[c(i, j)], collapse = " and "))
          value <- 0
        }
        mat[i, j] <- mat[j, i] <- value
      }
    }
    if (length(undefined) > 0) {
      context <- if (is.null(label)) "" else paste0(" for ", label)
      message(
        "Correlations undefined", context, " between reaches: ",
        paste(undefined, collapse = "; "), ". Assuming independence."
      )
    }
    mat
  }

  if (year_mode == "per_year") {
    years <- sort(unique(weekly$year))
    result <- lapply(years, function(year) {
      matrix_for(weekly[weekly$year == year, , drop = FALSE], paste("year", year))
    })
    names(result) <- as.character(years)
    result
  } else {
    matrix_for(weekly)
  }
}
