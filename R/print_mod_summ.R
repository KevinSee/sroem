#' @title Print Model Summary
#'
#' @description Shows estimated coefficients for selected observer error model
#'
#' @author Kevin See
#'
#' @inheritParams predict_neterr
#'
#' @importFrom broom tidy
#' @return dataframe
#' @export

print_mod_summ <- function(species = c("Steelhead", "Spring Chinook"),
                           num_obs = c("two", "one")) {
  species <- match.arg(species)
  num_obs <- match.arg(num_obs)

  if (species == "Steelhead") {
    if (num_obs == "two") {
      net_err_mod <- two_obs_net_mod
      # covar_center <- two_obs_covar_center
    }

    if (num_obs == "one") {
      net_err_mod <- one_obs_net_mod
      # covar_center <- one_obs_covar_center
    }
  }

  if (species == "Spring Chinook") {
    net_err_mod <- chnk_net_mod
    # covar_center <- chnk_covar_center
  }

  broom::tidy(net_err_mod)
}
