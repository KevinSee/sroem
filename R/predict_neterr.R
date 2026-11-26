#' Predict redd-survey net error
#'
#' Predicts observed count divided by true count minus one using a published
#' steelhead or spring Chinook model. Numeric model covariates are centered and
#' scaled using the original model-data statistics.
#'
#' @param redd_df Data frame with the selected model's covariates. If needed,
#'   `exp_sp_total_log` is calculated from `exp_sp_total`, and
#'   `naive_density_km` from `visible_redds / reach_length_km`.
#' @param species `"Steelhead"` or `"Spring Chinook"`.
#' @param num_obs Steelhead model to use: `"two"` or `"one"`.
#' @param err_ceiling,err_floor Whether to cap predicted net errors above or
#'   below the respective threshold.
#' @param err_ceiling_quant,err_floor_quant Training-response quantiles used
#'   for the ceiling and floor when explicit limits are not supplied.
#' @param err_max,err_min Optional explicit ceiling and floor.
#'
#' @return A tibble containing the original columns, derived covariates when
#'   needed, `net_error`, `net_error_se`, and enabled threshold indicators.
#'   Missing model covariates produce missing predictions. Thresholds affect
#'   the predicted error, not its standard error.
#' @export
predict_neterr <- function(redd_df = NULL,
                           species = c("Steelhead", "Spring Chinook"),
                           num_obs = c("two", "one"),
                           err_ceiling = TRUE,
                           err_ceiling_quant = 0.95,
                           err_floor = TRUE,
                           err_floor_quant = 0.05,
                           err_max = NULL,
                           err_min = NULL) {
  if (is.null(redd_df)) {
    stop("Redd data not supplied.", call. = FALSE)
  }
  species <- match.arg(species)
  num_obs <- match.arg(num_obs)

  if (species == "Spring Chinook") {
    net_err_mod <- chnk_net_mod
    covar_center <- chnk_covar_center
  } else if (num_obs == "one") {
    net_err_mod <- one_obs_net_mod
    covar_center <- one_obs_covar_center
  } else {
    net_err_mod <- two_obs_net_mod
    covar_center <- two_obs_covar_center
  }

  terms <- attr(net_err_mod$terms, "term.labels")
  if ("exp_sp_total_log" %in% setdiff(terms, names(redd_df)) &&
    "exp_sp_total" %in% names(redd_df)) {
    redd_df$exp_sp_total_log <- log(redd_df$exp_sp_total)
  }
  if ("naive_density_km" %in% setdiff(terms, names(redd_df)) &&
    all(c("visible_redds", "reach_length_km") %in% names(redd_df))) {
    redd_df$naive_density_km <- redd_df$visible_redds / redd_df$reach_length_km
  }
  missing <- setdiff(terms, names(redd_df))
  if (length(missing) > 0) {
    stop("Missing covariates in dataset: ", paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  scaled <- redd_df
  metrics <- intersect(terms, covar_center$metric)
  for (metric in metrics) {
    center <- covar_center[match(metric, covar_center$metric), ]
    scaled[[metric]] <- (scaled[[metric]] - center$mean) / center$sd
  }
  prediction <- stats::predict(
    net_err_mod,
    newdata = scaled, type = "link", se.fit = TRUE,
    na.action = stats::na.pass
  )
  result <- tibble::as_tibble(redd_df)
  result$net_error <- unname(prediction$fit)
  result$net_error_se <- unname(prediction$se.fit)
  names(result$net_error) <- names(result$net_error_se) <-
    as.character(seq_len(nrow(result)))

  if (err_ceiling) {
    ceiling <- if (is.null(err_max)) {
      stats::quantile(net_err_mod$y, err_ceiling_quant)
    } else {
      err_max
    }
    result$exceed_ceiling <- unname(result$net_error > ceiling)
    result$net_error <- dplyr::if_else(
      result$net_error > ceiling, ceiling, result$net_error
    )
  }
  if (err_floor) {
    floor <- if (is.null(err_min)) {
      stats::quantile(net_err_mod$y, err_floor_quant)
    } else {
      err_min
    }
    result$exceed_floor <- unname(result$net_error < floor)
    result$net_error <- dplyr::if_else(
      result$net_error < floor, floor, result$net_error
    )
  }
  result
}
