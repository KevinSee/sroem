#' Summarize estimated redd abundance
#'
#' Estimates redds for each group and summarizes the estimates over a spatial
#' scale. When requested, correlations between reaches contribute to the
#' standard error of each summary.
#'
#' @inheritParams estimate_redds
#' @param summ_vars Character vector of columns identifying each summary.
#' @param use_cor Whether to account for correlations between reaches.
#' @param date_nm,cor_redd_nm,reach_nm Names of the survey date, count, and
#'   reach columns used to estimate correlations.
#' @param use Missing-observation method passed to [stats::cor()].
#' @param method Correlation method: `"pearson"`, `"kendall"`, or `"spearman"`.
#' @param correlation_data Whether to correlate observed `"counts"` or supplied
#'   signed `"errors"`.
#' @param error_nm Name of the signed-error column when using `"errors"`.
#' @param year_mode `"pooled"`, `"per_year"`, or `"legacy"`; see
#'   `correlate_rchs()`. Per-year summaries must each contain one calendar year.
#' @param ... Reserved for compatibility; currently unused.
#'
#' @details Correlations use the weekly aggregation in `correlate_rchs()`.
#'   When a correlation cannot be estimated, it is set to zero and the affected
#'   reach pairs are reported in a message. The diagonal remains one. If a
#'   summary contains multiple estimates for the same reach, their mutual
#'   correlation cannot be inferred from reach-level data and is set to zero.
#'
#' @return A list with `rch_est` and `summ_est`; with `use_cor = TRUE`, also
#'   `cor_df`. Summaries include `strm_se_naive`, the independent-reach
#'   standard error, and `strm_se`, the correlation-adjusted standard error.
#' @export
summarize_redds <- function(redd_df = NULL,
                            species = c("Steelhead", "Spring Chinook"),
                            group_vars = c("river", "reach", "index", "survey_type"),
                            summ_vars = c("river", "location"),
                            new_redd_nm = "new_redds",
                            vis_redd_nm = "visible_redds",
                            net_err_nm = "net_error",
                            net_se_nm = "net_error_se",
                            min_non0_wks = 3,
                            min_redds = 2,
                            gauc = NULL,
                            add_zeros = FALSE,
                            use_cor = FALSE,
                            date_nm = "survey_date",
                            cor_redd_nm = "new_redds",
                            reach_nm = "reach",
                            use = "pairwise.complete.obs",
                            method = c("pearson", "kendall", "spearman"),
                            correlation_data = c("counts", "errors"),
                            error_nm = NULL,
                            year_mode = c("pooled", "per_year", "legacy"),
                            ...) {
  if (is.null(redd_df)) {
    stop("redd data must be supplied.", call. = FALSE)
  }
  if (!reach_nm %in% c(group_vars, summ_vars)) {
    stop("The reach column must be included in group_vars or summ_vars.",
      call. = FALSE
    )
  }
  if (use_cor) {
    method <- match.arg(method)
    correlation_data <- match.arg(correlation_data)
    year_mode <- match.arg(year_mode)
    use <- match.arg(use, c(
      "everything", "all.obs", "complete.obs", "na.or.complete",
      "pairwise.complete.obs"
    ))
    if (correlation_data == "errors" &&
      (!is.character(error_nm) || length(error_nm) != 1L ||
        is.na(error_nm) || !nzchar(error_nm))) {
      stop("error_nm must name the signed estimation-error column.", call. = FALSE)
    }
    value_nm <- if (correlation_data == "counts") cor_redd_nm else error_nm
    required <- c(date_nm, value_nm, reach_nm, summ_vars)
    missing <- setdiff(required, names(redd_df))
    if (length(missing) > 0) {
      stop("Missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
    }
    if (anyNA(redd_df[[reach_nm]])) {
      stop("Reach identifiers must not be missing when using correlations.",
        call. = FALSE
      )
    }
  }

  rch_est <- estimate_redds(
    redd_df,
    species = species,
    group_vars = unique(c(group_vars, summ_vars)),
    new_redd_nm = new_redd_nm,
    vis_redd_nm = vis_redd_nm,
    net_err_nm = net_err_nm,
    net_se_nm = net_se_nm,
    min_non0_wks = min_non0_wks,
    min_redds = min_redds,
    gauc = gauc,
    add_zeros = add_zeros
  )

  summ_est <- rch_est |>
    tidyr::nest(data = dplyr::everything(), .by = dplyr::all_of(summ_vars)) |>
    dplyr::mutate(
      n_rchs = if (reach_nm %in% summ_vars) {
        rep(1, dplyr::n())
      } else {
        purrr::map_dbl(data, ~ dplyr::n_distinct(.x[[reach_nm]]))
      },
      strm_obs = purrr::map_dbl(data, ~ sum(.x$tot_feat, na.rm = TRUE)),
      strm_est = purrr::map_dbl(data, ~ sum(.x$redd_est, na.rm = TRUE)),
      strm_se = purrr::map_dbl(data, ~ sqrt(sum(.x$redd_se^2, na.rm = TRUE)))
    )

  result <- list(rch_est = rch_est, summ_est = summ_est)
  if (!use_cor) {
    return(result)
  }

  cor_df <- redd_df |>
    tidyr::nest(data = dplyr::everything(), .by = dplyr::all_of(summ_vars))
  summary_label <- function(data, i) {
    paste(vapply(summ_vars, function(name) {
      paste0(name, "=", as.character(data[[name]][i]))
    }, character(1)), collapse = ", ")
  }
  cor_df$cor_mat <- purrr::map(seq_len(nrow(cor_df)), function(group) {
    x <- cor_df$data[[group]]
    for (name in intersect(c(reach_nm, date_nm, value_nm), summ_vars)) {
      x[[name]] <- rep(cor_df[[name]][group], nrow(x))
    }
    if (year_mode == "per_year" &&
      length(unique(lubridate::year(x[[date_nm]]))) != 1L) {
      stop("Each summary must contain one calendar year when year_mode is 'per_year'.",
        call. = FALSE
      )
    }
    mat <- withCallingHandlers(
      correlate_rchs(
        x,
        date_nm = date_nm, cor_redd_nm = cor_redd_nm, reach_nm = reach_nm,
        use = use, method = method, correlation_data = correlation_data,
        error_nm = error_nm, year_mode = year_mode
      ),
      message = function(m) {
        message("For ", summary_label(cor_df, group), ": ", conditionMessage(m))
        invokeRestart("muffleMessage")
      }
    )
    if (year_mode == "per_year") mat <- mat[[1L]]
    mat
  })
  cor_df <- cor_df |>
    dplyr::select(dplyr::all_of(summ_vars), cor_mat)

  summ_est <- summ_est |>
    dplyr::rename(strm_se_naive = strm_se) |>
    dplyr::left_join(cor_df, by = summ_vars)
  summ_est$strm_se <- purrr::map_dbl(seq_len(nrow(summ_est)), function(i) {
    x <- summ_est$data[[i]]
    reaches <- if (reach_nm %in% summ_vars) {
      rep(as.character(summ_est[[reach_nm]][i]), nrow(x))
    } else {
      as.character(x[[reach_nm]])
    }
    mat <- summ_est$cor_mat[[i]][reaches, reaches, drop = FALSE]
    duplicates <- duplicated(reaches) | duplicated(reaches, fromLast = TRUE)
    if (any(duplicates)) {
      for (reach in unique(reaches[duplicates])) {
        same <- outer(reaches == reach, reaches == reach, "&")
        mat[same & row(mat) != col(mat)] <- 0
        message(
          "Correlation could not be estimated between multiple estimates of reach ",
          reach, " for ", summary_label(summ_est, i), ". Assuming zero correlation."
        )
      }
    }
    if (anyNA(x$redd_se)) {
      warning("A reach standard error is missing; the summary standard error is unavailable.",
        call. = FALSE
      )
      return(NA_real_)
    }
    variance <- as.numeric(crossprod(x$redd_se, mat %*% x$redd_se))
    if (!is.finite(variance) || variance < -1e-10) {
      stop("Correlation matrix produced an invalid summary variance.", call. = FALSE)
    }
    sqrt(max(variance, 0))
  })
  result$summ_est <- summ_est
  result$cor_df <- cor_df
  result
}
