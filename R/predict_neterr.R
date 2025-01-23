#' @title Predict Net Error
#'
#' @description Predict the net error of a steelhead or Spring Chinook redd survey, based on published model
#'
#' @author Kevin See
#'
#' @param redd_df dataframe containing redd data including columns with covariates in selected observer error model
#' @param species which species is being analyzed? This function chooses the appropriate error model for this species. Choices are \code{Steelhead} or \code{Chinook}, with \code{Steelhead} being the default.
#' @param num_obs if species is \code{Steelhead}, which error model to use, for one or two observers? Default is \code{two}.
#' @param err_ceiling should the predictions have a ceiling (maximum value) for the predicted net error? Default is \code{TRUE}.
#' @param err_ceiling_quant if \code{err_ceiling} is \code{TRUE}, and \code{err_max} is \code{NULL} (the default), use this quantile of the observed net error in the initial model dataset to set that maximum value of predicted net errors. Default value is \code{0.95} for the 95th quantile.
#' @param err_floor should the predictions have a floor (minimum value) for the predicted net error? Default is \code{TRUE}.
#' @param err_floor_quant if \code{err_floor} is \code{TRUE}, and \code{err_min} is \code{NULL} (the default), use this quantile of the observed net error in the initial model dataset to set that minimum value of predicted net errors. Default value is \code{0.05} for the 5th quantile.
#' @param err_max if \code{err_ceiling} is \code{TRUE}, use this to manually set a maximum value for predicted net errors.
#' @param err_min if \code{err_floor} is \code{TRUE}, use this to manually set a minimum value for predicted net errors.
#'
#' @import dplyr tidyr tibble
#' @return tibble
#' @export

predict_neterr <- function(redd_df = NULL,
                           species = c("Steelhead", "Spring Chinook"),
                           num_obs = c("two", "one"),
                           err_ceiling = T,
                           err_ceiling_quant = 0.95,
                           err_floor = T,
                           err_floor_quant = 0.05,
                           err_max = NULL,
                           err_min = NULL) {
  if (is.null(redd_df)) {
    stop("Redd data not supplied.")
  }

  species <- match.arg(species)
  num_obs <- match.arg(num_obs)

  if (species == "Steelhead") {
    if (num_obs == "two") {
      net_err_mod <- two_obs_net_mod
      covar_center <- two_obs_covar_center
    }

    if (num_obs == "one") {
      net_err_mod <- one_obs_net_mod
      covar_center <- one_obs_covar_center
    }
  }

  if (species == "Spring Chinook") {
    net_err_mod <- chnk_net_mod
    covar_center <- chnk_covar_center
  }

  if (sum(!attr(net_err_mod$terms, "term.labels") %in% names(redd_df)) > 0) {
    miss_covar <- attr(net_err_mod$terms, "term.labels")[!attr(net_err_mod$terms, "term.labels") %in% names(redd_df)]

    if ("exp_sp_total_log" %in% miss_covar & "exp_sp_total" %in% names(redd_df)) {
      redd_df <- redd_df |>
        dplyr::mutate(exp_sp_total_log = log(exp_sp_total))

      miss_covar <- attr(net_err_mod$terms, "term.labels")[!attr(net_err_mod$terms, "term.labels") %in% names(redd_df)]
    }

    if ("naive_density_km" %in% miss_covar & sum(c("visible_redds", "reach_length_km") %in% names(redd_df)) == 2) {
      redd_df <- redd_df |>
        dplyr::mutate(naive_density_km = visible_redds / reach_length_km)

      miss_covar <- attr(net_err_mod$terms, "term.labels")[!attr(net_err_mod$terms, "term.labels") %in% names(redd_df)]
    }

    if (length(miss_covar) > 0) {
      stop(paste("Missing covariates in dataset:", paste(miss_covar, collapse = ", ")))
    }
  }

  pred_df <- redd_df %>%
    dplyr::mutate(data_id = 1:n()) %>%
    tidyr::pivot_longer(dplyr::any_of(covar_center$metric),
      names_to = "metric",
      values_to = "value"
    ) %>%
    dplyr::left_join(covar_center,
      by = "metric"
    ) %>%
    dplyr::mutate(value = (value - mean) / sd) %>%
    dplyr::select(-mean, -sd) %>%
    tidyr::pivot_wider(
      names_from = "metric",
      values_from = "value"
    ) %>%
    dplyr::bind_cols(predict(net_err_mod,
      newdata = .,
      backtransform = T,
      type = "link",
      se.fit = T
    ) %>%
      tibble::as_tibble() %>%
      dplyr::select(
        net_error = fit,
        net_error_se = se.fit
      ))

  pred_df <- pred_df %>%
    dplyr::select(-dplyr::any_of(covar_center$metric)) %>%
    dplyr::left_join(redd_df %>%
      dplyr::mutate(data_id = 1:n())) %>%
    dplyr::select(-data_id) %>%
    dplyr::select(
      dplyr::any_of(names(redd_df)),
      dplyr::everything()
    ) %>%
    suppressMessages()



  # check if any predictions are above threshold
  if (err_ceiling) {
    # set threshold
    if (!is.null(err_max)) {
      err_thres_max <- err_max
    } else {
      err_thres_max <- quantile(net_err_mod$y, err_ceiling_quant)
    }

    # shrink any predicted values to the threshold, if they exceed it
    pred_df <- pred_df |>
      dplyr::mutate(
        exceed_ceiling = dplyr::if_else(net_error > err_thres_max,
          T, F
        ),
        dplyr::across(
          net_error,
          ~ dplyr::if_else(. > err_thres_max,
            err_thres_max,
            .
          )
        )
      )
  }

  # check if any predictions are below threshold
  if (err_floor) {
    # set threshold
    if (!is.null(err_min)) {
      err_thres_min <- err_min
    } else {
      err_thres_min <- quantile(net_err_mod$y, err_floor_quant)
    }

    # shrink any predicted values to the threshold, if they exceed it
    pred_df <- pred_df |>
      dplyr::mutate(
        exceed_floor = dplyr::if_else(net_error < err_thres_min,
          T, F
        ),
        dplyr::across(
          net_error,
          ~ dplyr::if_else(. < err_thres_min,
            err_thres_min,
            .
          )
        )
      )
  }

  return(pred_df)
}
