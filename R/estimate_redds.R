#' Estimate redd abundance by reach
#'
#' Fits a Gaussian area-under-the-curve (GAUC) model when requested and
#' eligible; otherwise adjusts the total observed count by mean net error.
#' Surveys within each group must be in chronological order. GAUC uses their
#' row positions as equally spaced time units.
#'
#' @param redd_df Data frame of surveys, counts, and observer-error estimates.
#' @param species `"Steelhead"` or `"Spring Chinook"`. By default GAUC is
#'   requested for steelhead and not requested for spring Chinook.
#' @param group_vars Character vector of columns identifying each estimate.
#' @param new_redd_nm,vis_redd_nm Names of the new and visible redd columns.
#' @param net_err_nm,net_se_nm Names of the net-error estimate and standard
#'   error columns. Either column may be absent. Net error is observed count
#'   divided by true count minus one.
#' @param min_non0_wks Minimum number of surveys with a positive new count.
#' @param min_redds Minimum total observed count required to use GAUC.
#' @param gauc Whether to attempt GAUC. `NULL` selects the species default.
#' @param add_zeros Whether to add zero-count observations at nonzero ends.
#' @param ... Additional arguments passed to `fit_gauc()`.
#'
#' @details Mean net error and its standard error are calculated from surveys
#'   with visible redds. If no net-error estimate is available for a group,
#'   both are set to zero and a message identifies the affected reaches or
#'   grouping values. If the estimate is available but its standard error is
#'   missing, the standard error remains missing.
#'
#' @return A tibble with one row per group, including observed totals,
#'   `redd_est`, `redd_se`, nested survey data, and logical `gauc`, which is
#'   `TRUE` only when the GAUC estimate was used. When GAUC was attempted,
#'   `GAUC` is retained as an alias for compatibility, along with fit
#'   diagnostics. Failed fits fall back to adjusted total counts and are
#'   reported in `any_error` and a warning.
#' @export
estimate_redds <- function(redd_df = NULL,
                           species = c("Steelhead", "Spring Chinook"),
                           group_vars = c("river", "reach", "index", "survey_type"),
                           new_redd_nm = "new_redds",
                           vis_redd_nm = "visible_redds",
                           net_err_nm = "net_error",
                           net_se_nm = "net_error_se",
                           min_non0_wks = 3,
                           min_redds = 2,
                           gauc = NULL,
                           add_zeros = FALSE,
                           ...) {
  if (is.null(redd_df)) {
    stop("Redd data must be supplied.", call. = FALSE)
  }
  species <- match.arg(species)
  if (is.null(gauc)) {
    gauc <- species == "Steelhead"
  }

  summary_input <- redd_df |>
    dplyr::select(
      dplyr::all_of(group_vars),
      new_redds = dplyr::all_of(new_redd_nm),
      vis_redds = dplyr::all_of(vis_redd_nm)
    )
  summary_input$net_err <- if (net_err_nm %in% names(redd_df)) {
    redd_df[[net_err_nm]]
  } else {
    rep(NA_real_, nrow(redd_df))
  }
  summary_input$net_se <- if (net_se_nm %in% names(redd_df)) {
    redd_df[[net_se_nm]]
  } else {
    rep(NA_real_, nrow(redd_df))
  }

  redd_results <- summary_input |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
    dplyr::summarise(
      n_weeks = dplyr::n(),
      n_non0_wks = sum(new_redds > 0, na.rm = TRUE),
      tot_feat = sum(new_redds, na.rm = TRUE),
      err_est = mean(net_err[vis_redds > 0], na.rm = TRUE),
      err_se = mean(net_se[vis_redds > 0], na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::left_join(
      redd_df |>
        dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
        tidyr::nest(),
      by = group_vars
    )

  group_label <- function(i) {
    values <- vapply(group_vars, function(name) {
      paste0(name, "=", as.character(redd_results[[name]][i]))
    }, character(1))
    if (!"reach" %in% group_vars && "reach" %in% names(redd_results$data[[i]])) {
      reaches <- unique(as.character(redd_results$data[[i]]$reach))
      values <- c(values, paste0("reach=", paste(reaches, collapse = ", ")))
    }
    paste(values, collapse = ", ")
  }

  missing_error <- which(is.na(redd_results$err_est))
  missing_se <- which(!is.na(redd_results$err_est) & is.na(redd_results$err_se))
  if (length(missing_se) > 0) {
    warning(
      "Observer-error standard error unavailable for: ",
      paste(vapply(missing_se, group_label, character(1)), collapse = "; "),
      call. = FALSE
    )
  }
  if (length(missing_error) > 0) {
    message(
      "Assuming no net observer error for: ",
      paste(vapply(missing_error, group_label, character(1)), collapse = "; ")
    )
    redd_results$err_est[missing_error] <- 0
    redd_results$err_se[missing_error] <- 0
  }

  if (gauc) {
    fit_one <- purrr::safely(function(x, err_est, err_se) {
      mod_df <- data.frame(redds = x[[new_redd_nm]], day = seq_len(nrow(x)))
      if (add_zeros) {
        if (mod_df$redds[1] != 0) {
          mod_df <- rbind(data.frame(redds = 0, day = 0), mod_df)
          mod_df$day <- seq_len(nrow(mod_df))
        }
        if (mod_df$redds[nrow(mod_df)] != 0) {
          mod_df <- rbind(
            mod_df, data.frame(redds = 0, day = max(mod_df$day) + 1)
          )
        }
      }
      result <- fit_gauc(data = mod_df, v = err_est, v_se = err_se, ...)
      c(as.list(result), list(mod_data = mod_df))
    })
    fits <- purrr::pmap(
      list(redd_results$data, redd_results$err_est, redd_results$err_se),
      fit_one
    )
    redd_results$any_error <- purrr::map(fits, "error")
    redd_results$gauc_list <- purrr::map(fits, "result")
    failed <- which(!purrr::map_lgl(redd_results$any_error, is.null))
    if (length(failed) > 0) {
      details <- vapply(failed, function(i) {
        paste0(group_label(i), ": ", conditionMessage(redd_results$any_error[[i]]))
      }, character(1))
      warning(
        "GAUC fitting failed; using adjusted totals for: ",
        paste(details, collapse = "; "),
        call. = FALSE
      )
    }

    redd_results$converged <- purrr::map_lgl(
      redd_results$gauc_list, ~ isTRUE(.x$model$converged)
    )
    redd_results$correct_curve <- purrr::map_lgl(
      redd_results$gauc_list,
      ~ isTRUE(length(.x$beta) == 3 && all(is.finite(.x$beta)) &&
        .x$beta[1] < 0 && .x$beta[2] > 0 && .x$beta[3] < 0)
    )
    finite_estimate <- purrr::map_lgl(
      redd_results$gauc_list,
      ~ is.numeric(.x$E) && length(.x$E) == 1 && is.finite(.x$E)
    )
    redd_results$gauc <- redd_results$converged & redd_results$correct_curve &
      redd_results$n_non0_wks >= min_non0_wks &
      redd_results$tot_feat >= min_redds & finite_estimate
    redd_results$GAUC <- redd_results$gauc
  } else {
    redd_results$gauc <- rep(FALSE, nrow(redd_results))
  }

  redd_results$redd_est <- purrr::map_dbl(seq_len(nrow(redd_results)), function(i) {
    if (redd_results$gauc[i]) {
      redd_results$gauc_list[[i]]$E
    } else {
      redd_results$tot_feat[i] / (redd_results$err_est[i] + 1)
    }
  })
  redd_results$redd_se <- purrr::map_dbl(seq_len(nrow(redd_results)), function(i) {
    if (redd_results$gauc[i]) {
      redd_results$gauc_list[[i]]$E_se
    } else {
      msm::deltamethod(
        ~ x1 / (x2 + 1),
        mean = c(redd_results$tot_feat[i], redd_results$err_est[i]),
        cov = diag(c(0, redd_results$err_se[i])^2)
      )
    }
  })
  redd_results$redd_est <- janitor::round_half_up(redd_results$redd_est)
  redd_results
}
