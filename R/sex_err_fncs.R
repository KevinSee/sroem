#' @title Simulate sexing error data
#'
#' @description This function simulates some data from a scenario when initial sex calls are error-prone to some degree, but different data streams can provide insight into what the sex call error rates are. It is meant to mimic data from an initial sample, like adult steelhead sampling at Priest Rapids Dam, with sex-specific error rates, and subsequent broodstock collections of some of those fish later, when sex calls are known without error.
#'
#' @author Kevin See
#'
#' @param n_samp Total number of individuals in initial sample
#' @param p_true True proportion of group X
#' @param x_err Error rate of identifying group X. Of all the actual individuals in group X, this proportion will be identified as group Y.
#' @param y_err Error rate of identifying group Y. Of all the actual individuals in group Y, this proportion will be identified as group X.
#' @param b_x Number of group X individuals in second sample (e.g., broodstock collection)
#' @param b_y Number of group Y individuals in second sample (e.g., broodstock collection)
#' @param n_sim How many simulated data sets should be created? Default value is `1`
#' @param seed if set to a numeric value, it invokes the `set.seed()` function to make the data reproducible
#'
#' @export
#' @return a data.frame

sim_sex_err <- function(
    n_samp = 1,
    p_true = NULL,
    x_err = NULL,
    y_err = NULL,
    b_x = NULL,
    b_y = NULL,
    n_sim = 1,
    seed = NA)
{

  if(!is.na(seed)) {
    set.seed(seed)
  }

  # true number of X and Y from sample
  x_true = rbinom(n_sim, n_samp, p_true)
  # x_true = round_half_up(n_samp * p_true)
  y_true = n_samp - x_true

  # actual observed in sample
  # X's misidentified as Y
  x_samp_err <- rbinom(n_sim, x_true, x_err)

  # Y's misidentified as X
  y_samp_err <- rbinom(n_sim, y_true, y_err)

  # observed data
  samp_x <-
    x_true -
    x_samp_err +
    y_samp_err

  samp_y <-
    y_true -
    y_samp_err +
    x_samp_err

  # known mis-IDs in broodstock data
  false_x <- rbinom(n_sim, b_x, x_err)
  false_y <- rbinom(n_sim, b_y, y_err)

  return(list(samp_x = samp_x,
              samp_y = samp_y,
              b_x = b_x,
              false_x = false_x,
              b_y = b_y,
              false_y = false_y) |>
           as_tibble())
}

#' @title Adjust Observation-Error Proportions
#'
#' @description This function simulates some data from a scenario when initial sex calls are error-prone to some degree, but different data streams can provide insight into what the sex call error rates are. It is meant to mimic data from an initial sample, like adult steelhead sampling at Priest Rapids Dam, with sex-specific error rates, and subsequent broodstock collections of some of those fish later, when sex calls are known without error.
#'
#' @author Kevin See
#'
#' @inheritParams sim_sex_err
#' @param false_x how many individuals from group `b_x` were mis-identified as Y's in initial sample?
#' @param false_y how many individuals from group `b_y` were mis-identified as X's in initial sample?
#' @param samp_x number of individuals in initial sample identified as X.
#' @param samp_y number of individuals in initial sample identified as Y.
#' @param n_boot how many bootstrap samples should be used? Default is `1000`.
#' @param results should this function return a dataframe of estimates based on bootstrap samples (`estimate`), or a dataframe of those bootstrap samples (`bootstrap`)? Default is `estimate`.
#'
#' @export
#' @return a data.frame

est_adj_prop <-
  function(b_x,
           b_y,
           false_x,
           false_y,
           samp_x,
           samp_y,
           n_boot = 1000,
           seed = NA,
           results = c("estimate",
                       "bootstrap")) {

    if(!is.na(seed)) {
      set.seed(seed)
    }

    results = match.arg(results)

    # total number in sample
    samp_n = samp_x + samp_y

    # estimate error rate of X calls
    err_est_x <- false_x / b_x
    err_se_x <- sqrt(((err_est_x * (1 - err_est_x)) / b_x))

    # estimate error rate of Y calls
    err_est_y <- false_y / b_y
    err_se_y <- sqrt(((err_est_y * (1 - err_est_y)) / b_y))

    #------------------------------------
    # bootstrap lots of simulated data
    # convert mis-ID rates to Beta distribution
    sig2_x = err_se_x^2
    kappa_x = (err_est_x * (1 - err_est_x)) / sig2_x - 1
    alpha_x = err_est_x * kappa_x
    beta_x = (1 - err_est_x) * kappa_x

    sig2_y = err_se_y^2
    kappa_y = (err_est_y * (1 - err_est_y)) / sig2_y - 1
    alpha_y = err_est_y * kappa_y
    beta_y = (1 - err_est_y) * kappa_y

    # put into dataframe and calculate estimates from each bootstrap sample
    boot_df <-
      tibble(boot = 1:n_boot,
             # samp_x = samp_x,
             # samp_y = samp_y,
             samp_x = rbinom(n_boot,
                             samp_n,
                             samp_x / samp_n),
             samp_y = samp_n - samp_x,
             samp_n = samp_n,
             err_x = rbeta(n_boot,
                           alpha_x,
                           beta_x),
             err_y = rbeta(n_boot,
                           alpha_y,
                           beta_y)) |>
      mutate(across(starts_with("err"),
                    ~ tidyr::replace_na(., 0))) |>
      # estimate proportion of X in sample
      rowwise() |>
      mutate(prop_x_boot =
               ((samp_x / samp_n) - err_y) /
               (1 - err_y - err_x),
             across(prop_x_boot,
                    ~ case_when(. > 1 ~ 1,
                                . < 0 ~ 0,
                                .default = .)),
             prop_x_var =
               prop_x_boot * (1 - prop_x_boot) / samp_n,
             x_boot =
               prop_x_boot * samp_n,
             across(x_boot,
                    ~ janitor::round_half_up(.)),
             y_boot = samp_n - x_boot,
             x_se = msm::deltamethod(~ ((x1 / x2) - x3) /
                                       (1 - x3 - x4) * x2,
                                     mean = c(samp_x,
                                              samp_n,
                                              err_y,
                                              err_x),
                                     cov = diag(c(0, 0,
                                                  err_se_y,
                                                  err_se_x)^2)),
             x_y_ratio = prop_x_boot / (1 - prop_x_boot),
             x_y_var = (samp_n / (samp_n - x_boot)^2) * x_se^2,
             # x_y_var = (1 / (1 - prop_x_boot)^2) * prop_x_var,
             x_y_se = sqrt(x_y_var)) |>
      ungroup()

    if(results == "bootstrap") {
      return(boot_df)
    } else {

      est_df <-
        boot_df |>
        mutate(x_y_est = map_dbl(boot_sim,
                                 .f = function(x) {
                                   # res = mean(x$x_y_ratio)
                                   # if(is.na(res) |
                                   #    res == Inf) {
                                   #   res = median(x$x_y_ratio)
                                   # }
                                   res = median(x$x_y_ratio)
                                   return(res)
                                 }),
               x_y_se = map_dbl(boot_sim,
                                .f = function(x) {
                                  res = sd(x$x_y_ratio)
                                  if(is.na(res) |
                                     res == Inf) {
                                    res = sd(x$x_y_ratio[x$prop_x_boot < 1], na.rm = T)
                                  }
                                  # if(is.na(res) |
                                  #    res == Inf) {
                                  #   res = mean(x$x_y_se[x$prop_x_boot < 1], na.rm = T)
                                  # }
                                  return(res)
                                })
        )

      return(est_df)
    }
  }
