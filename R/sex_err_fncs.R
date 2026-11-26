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

  return(list(sim_id = 1:n_sim,
              true_x = x_true,
              true_y = y_true,
              samp_x = samp_x,
              samp_y = samp_y,
              b_x = b_x,
              false_x = false_x,
              b_y = b_y,
              false_y = false_y) |>
           as_tibble())
}

#' Bootstrap adjusted proportions with sex-classification error
#'
#' Simulates adjusted proportions using sex-specific error rates estimated
#' from a subsample whose true sex is known. Each `b_x` individual is truly X;
#' `false_x` of them were called Y. The equivalent applies to Y.
#'
#' @param b_x,b_y Numbers of truly X and Y individuals in the known-sex
#'   subsample.
#' @param false_x,false_y Numbers in each true-sex group whose field call was
#'   incorrect.
#' @param samp_x,samp_y Numbers called X and Y in the original sample.
#' @param n_boot Number of accepted bootstrap samples.
#' @param seed Optional random seed; `NA` leaves the random seed unchanged.
#' @param prior Positive beta prior shape parameters for each non-fixed error
#'   rate. The default `c(0.5, 0.5)` is the Jeffreys prior for a binomial
#'   probability. The posterior is beta(`false + prior[1]`,
#'   `b - false + prior[2]`).
#' @param zero_error Character vector containing `"x"`, `"y"`, both, or
#'   neither. The listed groups have an error rate fixed at exactly zero with
#'   no uncertainty; their observed false count must also be zero. Use this
#'   only when external knowledge justifies the assumption, not merely because
#'   no errors happened to be observed.
#' @param max_batches Maximum number of batches of `n_boot` draws. If too few
#'   mathematically valid samples are found, the function stops with an error.
#'
#' @details Error rates are conditional on true sex, not on the field call.
#'   For a group with no known-sex observations, a non-fixed rate is determined
#'   entirely by the prior and a warning is issued. Error rates whose sum is
#'   near one make the adjustment poorly identified; supplying a prior does
#'   not resolve that problem. The adjusted-ratio standard error applies the
#'   delta method to the unrounded adjusted count; as in the original code,
#'   it reflects error-rate uncertainty rather than bootstrap sampling
#'   variability.
#'
#' @return A tibble with `n_boot` rows, containing the sampled field counts,
#'   sampled error rates, adjusted proportions and counts, and uncertainty
#'   estimates. Adjusted X:Y ratios can be infinite when the adjusted
#'   proportion of Y is zero.
#' @export
boot_adj_prop <- function(b_x,
                          b_y,
                          false_x,
                          false_y,
                          samp_x,
                          samp_y,
                          n_boot = 1000,
                          seed = NA,
                          prior = c(0.5, 0.5),
                          zero_error = character(),
                          max_batches = 1000) {
  counts <- list(
    b_x = b_x, b_y = b_y, false_x = false_x, false_y = false_y,
    samp_x = samp_x, samp_y = samp_y, n_boot = n_boot, max_batches = max_batches
  )
  valid_count <- vapply(counts, function(x) {
    is.numeric(x) && length(x) == 1L && is.finite(x) && x >= 0 && x == floor(x)
  }, logical(1))
  if (!all(valid_count)) {
    stop("Counts, n_boot, and max_batches must be non-negative whole numbers.",
      call. = FALSE
    )
  }
  if (n_boot < 1 || max_batches < 1 || samp_x + samp_y == 0) {
    stop("n_boot, max_batches, and the total sample size must be positive.",
      call. = FALSE
    )
  }
  if (false_x > b_x || false_y > b_y) {
    stop("False counts cannot exceed their known-sex subsample sizes.",
      call. = FALSE
    )
  }
  if (!is.numeric(prior) || length(prior) != 2L ||
    any(!is.finite(prior)) || any(prior <= 0)) {
    stop("prior must contain two positive finite beta shape parameters.",
      call. = FALSE
    )
  }
  if (!is.character(zero_error) || anyNA(zero_error) ||
    any(!zero_error %in% c("x", "y"))) {
    stop("zero_error must contain only 'x' and/or 'y'.", call. = FALSE)
  }
  if (("x" %in% zero_error && false_x > 0) ||
    ("y" %in% zero_error && false_y > 0)) {
    stop("An error rate cannot be fixed at zero when false calls were observed.",
      call. = FALSE
    )
  }
  prior_only <- c(
    if (b_x == 0 && !"x" %in% zero_error) "x",
    if (b_y == 0 && !"y" %in% zero_error) "y"
  )
  if (length(prior_only) > 0) {
    warning(
      "No known-sex observations for ", paste(prior_only, collapse = ", "),
      "; the corresponding error rate is determined entirely by the prior.",
      call. = FALSE
    )
  }
  if (length(seed) != 1L || (!is.na(seed) && (!is.numeric(seed) || !is.finite(seed)))) {
    stop("seed must be one finite numeric value or NA.", call. = FALSE)
  }
  if (!is.na(seed)) {
    set.seed(seed)
  }

  posterior <- function(b, false, fixed) {
    if (fixed) {
      return(list(alpha = NA_real_, beta = NA_real_, se = 0, fixed = TRUE))
    }
    alpha <- false + prior[1]
    beta <- b - false + prior[2]
    total <- alpha + beta
    if (!is.finite(total)) {
      stop("Posterior beta parameters are too large.", call. = FALSE)
    }
    list(
      alpha = alpha, beta = beta, fixed = FALSE,
      se = sqrt((alpha / total) * (beta / total) / (total + 1))
    )
  }
  x_post <- posterior(b_x, false_x, "x" %in% zero_error)
  y_post <- posterior(b_y, false_y, "y" %in% zero_error)
  sample_error <- function(post) {
    if (post$fixed) {
      rep(0, n_boot)
    } else {
      stats::rbeta(n_boot, post$alpha, post$beta)
    }
  }

  samp_n <- samp_x + samp_y
  batches <- vector("list", max_batches)
  accepted <- 0L
  for (batch in seq_len(max_batches)) {
    obs_x <- stats::rbinom(n_boot, samp_n, samp_x / samp_n)
    err_x <- sample_error(x_post)
    err_y <- sample_error(y_post)
    observed_prop <- obs_x / samp_n
    denominator <- 1 - err_x - err_y
    keep <- denominator != 0 &
      observed_prop >= pmin(err_y, 1 - err_x) &
      observed_prop <= pmax(err_y, 1 - err_x)
    batches[[batch]] <- tibble::tibble(
      samp_x = obs_x[keep], samp_y = samp_n - obs_x[keep], samp_n = samp_n,
      err_x = err_x[keep], err_y = err_y[keep]
    )
    accepted <- accepted + sum(keep)
    if (accepted >= n_boot) {
      break
    }
  }
  if (accepted < n_boot) {
    stop(
      "Only ", accepted, " valid bootstrap draws were obtained after ",
      max_batches, " batches; ", n_boot, " are required. Check the observed ",
      "counts and error assumptions.",
      call. = FALSE
    )
  }

  boot_df <- dplyr::bind_rows(batches[seq_len(batch)])
  boot_df <- boot_df[sample.int(nrow(boot_df), n_boot), ]
  boot_df <- dplyr::mutate(
    boot_df,
    boot_id = dplyr::row_number(),
    prop_x_boot = (samp_x / samp_n - err_y) / (1 - err_x - err_y),
    prop_x_var = prop_x_boot * (1 - prop_x_boot) / samp_n,
    x_boot = janitor::round_half_up(prop_x_boot * samp_n),
    y_boot = samp_n - x_boot,
    x_se = sqrt(
      (samp_n * (samp_x / samp_n - 1 + err_x) /
        (1 - err_x - err_y)^2 * y_post$se)^2 +
        (samp_n * (samp_x / samp_n - err_y) /
          (1 - err_x - err_y)^2 * x_post$se)^2
    ),
    x_y_ratio = prop_x_boot / (1 - prop_x_boot),
    x_y_var = (x_se / (samp_n * (1 - prop_x_boot)^2))^2,
    x_y_se = sqrt(x_y_var)
  )
  dplyr::relocate(boot_df, boot_id)
}

#' @title Adjust Observation-Error Proportions
#'
#' @description This function simulates some data from a scenario when initial sex calls are error-prone to some degree, but different data streams can provide insight into what the sex call error rates are. It is meant to mimic data from an initial sample, like adult steelhead sampling at Priest Rapids Dam, with sex-specific error rates, and subsequent broodstock collections of some of those fish later, when sex calls are known without error.
#'
#' @author Kevin See
#'
#' @param boot_df bootstrap results of calling the function `boot_adj_prop()`.
#' @param estimate if `results == estimate`, should an estimate of the ratio of X:Y be returned (`x_y_ratio`), or an estimate of the proportion of X (`prop_x`)? Default is `x_y_ratio`.
#' @param ... Arguments passed to [boot_adj_prop()] if `boot_df` is `NULL`.
#'
#' @import dplyr msm
#' @export
#' @return a data.frame

est_adj_prop <-
  function(boot_df = NULL,
           estimate = c("x_y_ratio",
                        "prop_x"),
           ...) {

    estimate = match.arg(estimate)

    if(is.null(boot_df)) {
      boot_df <-
        boot_adj_prop(...)
    }

    if(estimate == "x_y_ratio") {

      est_df <-
        boot_df |>
        dplyr::summarize(
          dplyr::across(c(prop_x_boot,
                          prop_x_var,
                          x_y_ratio,
                          x_y_se),
                        median)) |>
        dplyr::bind_cols(
          boot_df |>
            dplyr::summarize(
              dplyr::across(c(prop_x_boot,
                              x_y_ratio),
                            sd)) |>
            dplyr::rename(
              prop_se = prop_x_boot,
              x_y_sd = x_y_ratio)
        ) |>
        mutate(across(x_y_se,
                      ~ case_when(. == 0 ~ x_y_sd,
                                  .default = .))) |>
        select(x_y_est = x_y_ratio,
               x_y_se)
      # summarize(x_y_est = median(x_y_ratio),
      #           x_y_se = case_when(sum(prop_x_boot >= 1) > 0 ~ sd(x_y_ratio[prop_x_boot < 1], na.rm = T),
      #                              .default = sd(x_y_ratio))
      # mutate(x_y_est = map_dbl(boot_sim,
      #                          .f = function(x) {
      #                            # res = mean(x$x_y_ratio)
      #                            # if(is.na(res) |
      #                            #    res == Inf) {
      #                            #   res = median(x$x_y_ratio)
      #                            # }
      #                            res = median(x$x_y_ratio)
      #                            return(res)
      #                          }),
      #        x_y_se = map_dbl(boot_sim,
      #                         .f = function(x) {
      #                           res = sd(x$x_y_ratio)
      #                           if(is.na(res) |
      #                              res == Inf) {
      #                             res = sd(x$x_y_ratio[x$prop_x_boot < 1], na.rm = T)
      #                           }
      #                           # if(is.na(res) |
      #                           #    res == Inf) {
      #                           #   res = mean(x$x_y_se[x$prop_x_boot < 1], na.rm = T)
      #                           # }
      #                           return(res)
      #                         })
      # )
    } else if(estimate == "prop_x") {
      est_df <-
        boot_df |>
        summarize(prop_x = median(prop_x_boot),
                  prop_x_sd = sd(prop_x_boot),
                  prop_x_se = sqrt(median(prop_x_var))) |>
        mutate(across(prop_x_se,
                      ~ case_when(prop_x_sd == 0 ~ .,
                                  prop_x_se == 0 ~ prop_x_sd,
                                  .default = prop_x_sd))) |>
        select(prop_x,
               prop_x_se)
      # prop_x_se = sqrt(median(prop_x_var)))
      # prop_x_se = case_when(sum(prop_x_boot >= 1) > 0 ~ sd(prop_x_boot[prop_x_boot < 1], na.rm = T),
      #                       .default = sd(prop_x_boot)))
    }

    return(est_df)
  }
