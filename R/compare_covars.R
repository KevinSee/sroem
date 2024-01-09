#' @title Covariate Comparison
#'
#' @description Compares covariates of model dataset with predictive dataset
#'
#' @author Kevin See
#'
#' @inheritParams predict_neterr
#' @param z_score Should the values be z-scored? Default is \code{FALSE}.
#' @param incl_neterr Should the net error values and predictions be included? Default is \code{TRUE}.
#'
#' @import dplyr
#' @importFrom tidyr pivot_longer
#' @return dataframe
#' @export

compare_covars <- function(redd_df = NULL,
                           species = c("Steelhead", "Spring Chinook"),
                           num_obs = c("two", "one"),
                           z_score = F,
                           incl_neterr = T) {
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


  if (!z_score) {
    comp_df <- net_err_mod$data |>
      dplyr::select(dplyr::any_of(attr(net_err_mod$terms, "term.labels"))) |>
      dplyr::select(dplyr::where(is.numeric)) |>
      tidyr::pivot_longer(cols = dplyr::everything(),
                          names_to = "covariate",
                          values_to = "value") |>
      dplyr::mutate(source = "Model Data") |>
      dplyr::left_join(covar_center |>
                         dplyr::rename(covariate = metric),
                       by = dplyr::join_by(covariate)) |>
      dplyr::mutate(
        dplyr::across(value,
                      ~ dplyr::if_else(covariate != "net_error",
                                       . * sd + mean,
                                       .))) |>
      dplyr::select(-c(mean, sd)) |>
      dplyr::bind_rows(redd_df |>
                         dplyr::select(-contains("net_error")) |>
                         sroem::predict_neterr(species = species,
                                               num_obs = num_obs) |>
                         dplyr::filter(!is.na(net_error)) |>
                         dplyr::select(dplyr::any_of(attr(net_err_mod$terms, "term.labels"))) |>
                         dplyr::select(dplyr::where(is.numeric)) |>
                         tidyr::pivot_longer(cols = dplyr::everything(),
                                             names_to = "covariate",
                                             values_to = "value") |>
                         dplyr::mutate(source = "Predictive Data"))
  } else {
    comp_df <- net_err_mod$data |>
      dplyr::select(dplyr::any_of(attr(net_err_mod$terms, "term.labels")),
                    net_error) |>
      dplyr::select(dplyr::where(is.numeric)) |>
      tidyr::pivot_longer(cols = dplyr::everything(),
                          names_to = "covariate",
                          values_to = "value") |>
      dplyr::mutate(source = "Model Data") |>
      bind_rows(redd_df |>
                  dplyr::select(-contains("net_error")) |>
                  sroem::predict_neterr(species = species,
                                        num_obs = num_obs) |>
                  dplyr::filter(!is.na(net_error)) |>
                  dplyr::select(dplyr::any_of(attr(net_err_mod$terms, "term.labels"))) |>
                  dplyr::select(dplyr::where(is.numeric)) |>
                  tidyr::pivot_longer(cols = dplyr::everything(),
                                      names_to = "covariate",
                                      values_to = "value") |>
                  dplyr::mutate(source = "Predictive Data") |>
                  dplyr::left_join(covar_center |>
                                     rename(covariate = metric),
                                   by = "covariate") |>
                  dplyr::mutate(across(value,
                                       ~ if_else(!is.na(mean),
                                                 (. - mean) / sd,
                                                 .))) |>
                  dplyr::select(-c(mean, sd)))
  }

  if(incl_neterr) {
    comp_df <- comp_df |>
      bind_rows(net_err_mod$data |>
                  dplyr::select(value = net_error) |>
                  dplyr::mutate(covariate = "net_error",
                                source = "Model Data")) |>
      bind_rows(redd_df |>
                  dplyr::select(-contains("net_error")) |>
                  sroem::predict_neterr(species = species,
                                        num_obs = num_obs) |>
                  dplyr::filter(!is.na(net_error)) |>
                  dplyr::select(value = net_error) |>
                  dplyr::mutate(covariate = "net_error",
                                source = "Predictive Data"))
  }


  return(comp_df)

}
