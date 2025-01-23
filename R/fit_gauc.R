#' @title Gaussian Area-Under-the-Curve
#'
#' @description Fit a Gaussian area-under-the-curve model to redd counts, incorporating estimates of observer error if possible.
#'
#' @author Kevin See
#'
#' @param data dataframe containing columns named redds and day (Julian days)
#' @param v overall mean observer error
#' @param v_se standard deviation of overall observer error
#' @param model_fam model family. Must be either `quasipoisson` or `negative.binomial`.
#'
#'
#' @import dplyr msm
#' @return list
#' @export

fit_gauc <- function(data = NULL,
                     SL = 1,
                     SL_se = 0,
                     v = 1,
                     v_se = 0,
                     model_fam = c("quasipoisson", "negative.binomial")) {
  stopifnot(
    !is.null(data),
    sum(c("redds", "day") %in% names(data)) == 2
  )

  model_fam <- match.arg(model_fam)

  g_pois <-
    glm(
      redds ~ day + I(day^2),
      data = data,
      family = quasipoisson
    )

  if (model_fam == "quasipoisson") g <- g_pois

  if (model_fam == "negative.binomial") {
    g <-
      glm(
        redds ~ day + I(day^2),
        data = data,
        family = negative.binomial(round(summary(g.pois)$dispersion))
      )
  }

  x <- coef(g)

  # F is redd days
  Fg <- sqrt(-pi / x[3]) * exp(x[1] - x[2]^2 / (4 * x[3]))
  # adjust redd days for operational redd life & net error
  E <- Fg / (SL * (v + 1))
  # Obtain std error of estimated redd-days, using delta method
  # check that vcov is correct dim
  if (sum(dim(vcov(g)) == c(3, 3)) == 2) {
    F_se <- msm::deltamethod(~ sqrt(-pi / x3) * exp(x1 - x2^2 / (4 * x3)),
      mean = x,
      cov = vcov(g)
    )

    # Include uncertainty in redd-days, stream-life and observer efficiency
    E_se <- msm::deltamethod(~ x1 / (x2 * (x3 + 1)),
      mean = c(Fg, SL, v),
      cov = diag(c(F_se, SL_se, v_se))^2
    )
  } else {
    E_se <- NA
  }

  tau2 <- -1 / (2 * x[3])
  ms <- x[2] * tau2
  a <- exp(x[1] + ms^2 / (2 * tau2))
  params <- c(
    "tau2" = tau2,
    "ms" = ms,
    "a" = a
  )

  return(
    list(
      E = E,
      E_se = E_se,
      Fg = Fg,
      F_se = F_se,
      beta = x,
      Ncurve = params,
      model = g
    )
  )
}
