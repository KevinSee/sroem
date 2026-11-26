#' Fit a Gaussian area-under-the-curve model
#'
#' Fits a quadratic log-linear model to redd counts and adjusts the integrated
#' curve for redd life and observer net error.
#'
#' @param data Data frame containing `redds` (counts) and `day` (survey time).
#'   Times must be expressed in the units used for `SL`.
#' @param SL Mean operational redd life, in the same time units as `day`.
#' @param SL_se Standard error of `SL`.
#' @param v Mean observer net error: observed count divided by true count,
#'   minus one. Zero represents no net error.
#' @param v_se Standard error of `v`.
#' @param model_fam Either `"quasipoisson"` or `"negative.binomial"`. The
#'   latter uses the rounded quasi-Poisson dispersion as the negative-binomial
#'   size parameter, as intended by the original implementation.
#'
#' @return A list containing adjusted abundance `E` and its standard error
#'   `E_se`, unadjusted area `Fg` and its standard error `F_se`, coefficients
#'   `beta`, curve parameters `Ncurve`, and the fitted `model`. Curve-derived
#'   quantities are `NA` if the quadratic is not concave or is unidentified.
#' @export
fit_gauc <- function(data = NULL,
                     SL = 1,
                     SL_se = 0,
                     v = 1,
                     v_se = 0,
                     model_fam = c("quasipoisson", "negative.binomial")) {
  if (is.null(data) || !all(c("redds", "day") %in% names(data))) {
    stop("data must contain redds and day columns.", call. = FALSE)
  }
  model_fam <- match.arg(model_fam)

  g_pois <- stats::glm(
    redds ~ day + I(day^2),
    data = data,
    family = stats::quasipoisson
  )

  g <- g_pois
  if (model_fam == "negative.binomial") {
    theta <- round(summary(g_pois)$dispersion)
    if (!is.finite(theta) || theta <= 0) {
      stop("The rounded quasi-Poisson dispersion must be positive.", call. = FALSE)
    }
    g <- stats::glm(
      redds ~ day + I(day^2),
      data = data,
      family = MASS::negative.binomial(theta)
    )
  }

  x <- stats::coef(g)
  Fg <- F_se <- E <- E_se <- NA_real_
  params <- c(tau2 = NA_real_, ms = NA_real_, a = NA_real_)

  if (length(x) == 3 && all(is.finite(x)) && x[3] < 0) {
    Fg <- sqrt(-pi / x[3]) * exp(x[1] - x[2]^2 / (4 * x[3]))
    E <- Fg / (SL * (v + 1))

    covariance <- stats::vcov(g)
    if (identical(dim(covariance), c(3L, 3L)) &&
      all(is.finite(covariance))) {
      F_se <- msm::deltamethod(
        ~ sqrt(-pi / x3) * exp(x1 - x2^2 / (4 * x3)),
        mean = x,
        cov = covariance
      )
      E_se <- msm::deltamethod(
        ~ x1 / (x2 * (x3 + 1)),
        mean = c(Fg, SL, v),
        cov = diag(c(F_se, SL_se, v_se)^2)
      )
    }

    tau2 <- -1 / (2 * x[3])
    ms <- x[2] * tau2
    params <- c(tau2 = tau2, ms = ms, a = exp(x[1] + ms^2 / (2 * tau2)))
  } else {
    warning("The fitted quadratic does not define a finite Gaussian curve.",
      call. = FALSE
    )
  }

  list(
    E = E, E_se = E_se, Fg = Fg, F_se = F_se,
    beta = x, Ncurve = params, model = g
  )
}
