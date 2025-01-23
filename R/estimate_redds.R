#' @title Estimate Redds
#'
#' @description Estimates true number of redds across a series of reaches, using a Gaussian area-under-the-curve model where possible and incorporating estimates of observer error if possible.
#'
#' @author Kevin See
#'
#' @param redd_df dataframe containing redd data, including estimates of observer error
#' @param species which species is being analyzed? This function chooses the appropriate error model for this species. Choices are \code{Steelhead} or \code{Chinook}, with \code{Steelhead} being the default.
#' @param group_vars vector of column names from {redd_df} to group results by
#' @param new_redd_nm quoted name of column in {redd_df} listing number of new redds found during that survey
#' @param vis_redd_nm quoted name of column in {redd_df} listing number of visible redds present during that survey
#' @param net_err_nm quoted name of column in {redd_df} listing estimate of net error for that survey
#' @param net_se_nm quoted name of column in {redd_df} listing standard error of net error estimate for that survey
#' @param min_non0_wks minimum number of weeks with at least one new redd observed
#' @param min_redds minimum number of total redds observed
#' @param gauc \code{TRUE/FALSE} should a Gaussian area-under-the-curve model be applied? If \code{FALSE}, the total number of new redds is divided by the mean estimate of observer error. By default \code{gauc} is \code{TRUE} for steelhead, and \code{FALSE} for Spring Chinook.
#' @param add_zeros should leading and trailing zero counts be added if the first and last counts aren't already zero? Default is {FALSE}.
#'
#'
#' @import dplyr purrr
#' @importFrom msm deltamethod
#' @return dataframe
#' @export

estimate_redds <- function(redd_df = NULL,
                           species = c("Steelhead", "Spring Chinook"),
                           group_vars = c(river, reach, index, survey_type),
                           new_redd_nm = "new_redds",
                           vis_redd_nm = "visible_redds",
                           net_err_nm = "net_error",
                           net_se_nm = "net_error_se",
                           min_non0_wks = 3,
                           min_redds = 2,
                           gauc = NULL,
                           add_zeros = F,
                           ...) {
  if (is.null(redd_df)) {
    stop("Redd data must be supplied")
  }

  species <- match.arg(species)


  # by default, use GAUC for steelhead, not for Spring Chinook
  if (is.null(gauc)) {
    if (species == "Steelhead") {
      gauc <- T
    }
    if (species == "Spring Chinook") {
      gauc <- F
    }
  }

  redd_results <- redd_df %>%
    dplyr::select(all_of({{ group_vars }}),
      new_redds = {{ new_redd_nm }},
      vis_redds = {{ vis_redd_nm }},
      net_err = {{ net_err_nm }},
      net_se = {{ net_se_nm }}
    ) %>%
    dplyr::group_by(across({{ group_vars }})) %>%
    dplyr::summarise(
      n_weeks = n(),
      n_non0_wks = sum(new_redds > 0, na.rm = T),
      tot_feat = sum(new_redds, na.rm = T),
      err_est = mean(net_err[vis_redds > 0], na.rm = T),
      err_se = mean(net_se[vis_redds > 0], na.rm = T),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      err_se = dplyr::if_else(is.na(err_est), 0, err_se),
      err_est = dplyr::if_else(is.na(err_est), 0, err_est)
    ) %>%
    dplyr::full_join(
      redd_df %>%
        dplyr::group_by(across({{ group_vars }})) %>%
        tidyr::nest(),
      by = {{ group_vars }}
    )

  if (gauc) {
    redd_results <- redd_results |>
      dplyr::mutate(gauc_list = purrr::map(data,
        .f = safely(function(x, ...) {
          mod_df <- x %>%
            dplyr::select(redds = {{ new_redd_nm }}) %>%
            dplyr::mutate(day = 1:n())

          if (add_zeros) {
            if (mod_df$redds[mod_df$day == min(mod_df$day)] != 0) {
              mod_df <- mod_df %>%
                dplyr::bind_rows(
                  dplyr::tibble(
                    redds = 0,
                    day = min(mod_df$day) - 1
                  )
                ) %>%
                dplyr::arrange(day) %>%
                dplyr::mutate(day = 1:n())
            }
            if (mod_df$redds[mod_df$day == max(mod_df$day)] != 0) {
              mod_df <- mod_df %>%
                dplyr::bind_rows(
                  dplyr::tibble(
                    redds = 0,
                    day = max(mod_df$day) + 1
                  )
                ) %>%
                dplyr::arrange(day)
            }
          }

          v_vec <- x %>%
            dplyr::rename(
              vis_redds = {{ vis_redd_nm }},
              net_err = {{ net_err_nm }},
              net_se = {{ net_se_nm }}
            ) %>%
            dplyr::filter(vis_redds > 0) %>%
            dplyr::summarize(
              dplyr::across(
                c(
                  net_err,
                  net_se
                ),
                ~ mean(.x, na.rm = T)
              )
            ) %>%
            dplyr::mutate(
              net_se = dplyr::if_else(is.na(net_err),
                0,
                net_se
              ),
              net_err = dplyr::if_else(is.na(net_err),
                0,
                net_err
              )
            )

          # # if you want to assume no observer error
          # v_vec = tibble(net_err = 1,
          #                net_se = 0)

          res_list <- fit_gauc(
            data = mod_df,
            v = v_vec$net_err,
            v_se = v_vec$net_se,
            ...
          )

          res_list <- c(
            as.list(res_list),
            list("mod_data" = mod_df)
          )

          return(res_list)
        })
      )) %>%
      dplyr::mutate(
        any_error = map(
          gauc_list,
          "error"
        ),
        dplyr::across(
          gauc_list,
          ~ purrr::map(
            .,
            "result"
          )
        )
      ) |>
      dplyr::mutate(
        converged = purrr::map_lgl(gauc_list,
          .f = function(x) {
            x$model$converged
          }
        )
      ) %>%
      dplyr::mutate(
        correct_curve = purrr::map_lgl(gauc_list,
          .f = function(x) {
            x$beta[1] < 0 &
              x$beta[2] > 0 &
              x$beta[3] < 0
          }
        )
      ) %>%
      dplyr::mutate(
        redd_est = purrr::map_dbl(gauc_list,
          .f = "E"
        ),
        redd_se = purrr::map_dbl(gauc_list,
          .f = "E_se"
        )
      ) %>%
      dplyr::mutate(GAUC = dplyr::if_else(!converged |
        n_non0_wks < min_non0_wks |
        tot_feat < min_redds |
        !correct_curve,
      F, T
      )) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        redd_est = dplyr::if_else(!GAUC,
          tot_feat / (err_est + 1),
          redd_est
        ),
        redd_se = dplyr::if_else(!GAUC,
          msm::deltamethod(~ x1 / x2,
            mean = c(tot_feat, err_est + 1),
            cov = diag(c(0, err_se)^2)
          ),
          redd_se
        )
      ) %>%
      dplyr::ungroup()
  } else {
    redd_results <- redd_results |>
      dplyr::rowwise() %>%
      dplyr::mutate(
        redd_est = tot_feat / (err_est + 1),
        redd_se = msm::deltamethod(~ x1 / (x2 + 1),
          mean = c(tot_feat, err_est),
          cov = diag(c(0, err_se)^2)
        ),
      ) %>%
      dplyr::ungroup()
  }

  redd_results <- redd_results %>%
    dplyr::mutate(
      dplyr::across(
        redd_est,
        round_half_up
      )
    )

  return(redd_results)
}
