#' Compare model and prediction covariates
#'
#' Returns numeric covariates used in the selected observer-error model in
#' long form, optionally including observed and predicted net error. Only
#' prediction rows with a non-missing net-error prediction are included.
#'
#' @inheritParams predict_neterr
#' @param z_score If `TRUE`, express covariates using the model's training
#'   means and standard deviations; net error is never standardized.
#' @param incl_neterr If `TRUE`, include net error once for each dataset.
#'
#' @return A tibble with `covariate`, `value`, and `source` columns. The
#'   sources are `"Model Data"` and `"Predictive Data"`.
#' @export
compare_covars <- function(redd_df = NULL,
                           species = c("Steelhead", "Spring Chinook"),
                           num_obs = c("two", "one"),
                           z_score = FALSE,
                           incl_neterr = TRUE) {
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
  predictions <- redd_df |>
    dplyr::select(-dplyr::contains("net_error")) |>
    sroem::predict_neterr(species = species, num_obs = num_obs) |>
    dplyr::filter(!is.na(net_error))

  model_data <- net_err_mod$data |>
    dplyr::select(dplyr::any_of(terms)) |>
    dplyr::select(dplyr::where(is.numeric)) |>
    tidyr::pivot_longer(dplyr::everything(),
      names_to = "covariate", values_to = "value"
    ) |>
    dplyr::mutate(source = "Model Data")

  prediction_data <- predictions |>
    dplyr::select(dplyr::any_of(terms)) |>
    dplyr::select(dplyr::where(is.numeric)) |>
    tidyr::pivot_longer(dplyr::everything(),
      names_to = "covariate", values_to = "value"
    ) |>
    dplyr::mutate(source = "Predictive Data")

  centers <- dplyr::rename(covar_center, covariate = metric)
  if (z_score) {
    prediction_data <- prediction_data |>
      dplyr::left_join(centers, by = "covariate") |>
      dplyr::mutate(value = dplyr::if_else(
        !is.na(mean), (value - mean) / sd, value
      )) |>
      dplyr::select(-mean, -sd)
  } else {
    model_data <- model_data |>
      dplyr::left_join(centers, by = "covariate") |>
      dplyr::mutate(value = value * sd + mean) |>
      dplyr::select(-mean, -sd)
  }

  result <- dplyr::bind_rows(model_data, prediction_data)
  if (incl_neterr) {
    result <- dplyr::bind_rows(
      result,
      tibble::tibble(
        covariate = "net_error", value = net_err_mod$data$net_error,
        source = "Model Data"
      ),
      tibble::tibble(
        covariate = "net_error", value = predictions$net_error,
        source = "Predictive Data"
      )
    )
  }
  result
}
