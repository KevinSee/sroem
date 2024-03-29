% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/predict_neterr.R
\name{predict_neterr}
\alias{predict_neterr}
\title{Predict Net Error}
\usage{
predict_neterr(
  redd_df = NULL,
  species = c("Steelhead", "Spring Chinook"),
  num_obs = c("two", "one"),
  err_ceiling = T,
  err_ceiling_quant = 0.95,
  err_floor = T,
  err_floor_quant = 0.05,
  err_max = NULL,
  err_min = NULL
)
}
\arguments{
\item{redd_df}{dataframe containing redd data including columns with covariates in selected observer error model}

\item{species}{which species is being analyzed? This function chooses the appropriate error model for this species. Choices are \code{Steelhead} or \code{Chinook}, with \code{Steelhead} being the default.}

\item{num_obs}{if species is \code{Steelhead}, which error model to use, for one or two observers? Default is \code{two}.}

\item{err_ceiling}{should the predictions have a ceiling (maximum value) for the predicted net error? Default is \code{TRUE}.}

\item{err_ceiling_quant}{if \code{err_ceiling} is \code{TRUE}, and \code{err_max} is \code{NULL} (the default), use this quantile of the observed net error in the initial model dataset to set that maximum value of predicted net errors. Default value is \code{0.95} for the 95th quantile.}

\item{err_floor}{should the predictions have a floor (minimum value) for the predicted net error? Default is \code{TRUE}.}

\item{err_floor_quant}{if \code{err_floor} is \code{TRUE}, and \code{err_min} is \code{NULL} (the default), use this quantile of the observed net error in the initial model dataset to set that minimum value of predicted net errors. Default value is \code{0.05} for the 5th quantile.}

\item{err_max}{if \code{err_ceiling} is \code{TRUE}, use this to manually set a maximum value for predicted net errors.}

\item{err_min}{if \code{err_floor} is \code{TRUE}, use this to manually set a minimum value for predicted net errors.}
}
\value{
tibble
}
\description{
Predict the net error of a steelhead or Spring Chinook redd survey, based on published model
}
\author{
Kevin See
}
