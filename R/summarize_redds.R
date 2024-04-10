#' @title Summarize Redds
#'
#' @description Estimates true number of redds across a series of reaches, using a Gaussian area-under-the-curve model where possible and incorporating estimates of observer error if possible. These estimates are then summarized across a user-defined spatial scale.
#'
#' @author Kevin See
#'
#' @inheritParams estimate_redds
#' @inheritParams correlate_rchs
#' @inheritParams corrr::correlate
#' @param summ_vars vector of column names from {redd_df} to group summaries of redd results by
#' @param use_cor should correlations between reaches be used in calculating standard errors? Default is {FALSE}
#'
#' @import dplyr msm tidyr purrr corrr
#' @return dataframe
#' @export

summarize_redds <- function(redd_df = NULL,
                            species = c("Steelhead", "Spring Chinook"),
                            group_vars = c(river, reach, index, survey_type),
                            summ_vars = c("river", "location"),
                            new_redd_nm = "new_redds",
                            vis_redd_nm = "visible_redds",
                            net_err_nm = "net_error",
                            net_se_nm = "net_error_se",
                            min_non0_wks = 3,
                            min_redds = 2,
                            gauc = NULL,
                            add_zeros = F,
                            use_cor = F,
                            date_nm = "survey_date",
                            cor_redd_nm = "new_redds",
                            reach_nm = "reach",
                            use = "pairwise.complete.obs",
                            method = c(
                              "pearson",
                              "kendall",
                              "spearman"
                            ),
                            ...) {
  if (is.null(redd_df)) {
    stop("redd data must be supplied")
  }

  # estimate redds for each reach using net error estimate and GAUC
  rch_est <- estimate_redds(
    redd_df,
    species = species,
    group_vars = unique(c(group_vars, summ_vars)),
    new_redd_nm = new_redd_nm,
    vis_redd_nm = vis_redd_nm,
    net_err_nm = net_err_nm,
    net_se_nm = net_se_nm,
    min_non0_wks = min_non0_wks,
    min_redds = min_redds,
    gauc = gauc,
    add_zeros = add_zeros
  )

  # summarize results across summ_vars
  all_rchs <- redd_df %>%
    pull({{ reach_nm }}) %>%
    unique()

  summ_est <- rch_est %>%
    dplyr::filter(.data[[reach_nm]] %in% all_rchs) %>%
    dplyr::group_by(dplyr::across({{ summ_vars }})) %>%
    tidyr::nest() %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      n_rchs = purrr::map_dbl(data,
        .f = function(x) {
          x %>%
            rename(rch = {{ reach_nm }}) %>%
            summarize(n_rchs = n_distinct(rch)) %>%
            pull(n_rchs) %>%
            return()
        }
      ),
      strm_obs = purrr::map_dbl(data,
        .f = function(x) {
          sum(x$tot_feat, na.rm = T)
        }
      ),
      strm_est = purrr::map_dbl(data,
        .f = function(x) {
          sum(x$redd_est, na.rm = T)
        }
      ),
      strm_se = purrr::map_dbl(data,
        .f = function(x) {
          sqrt(sum(x$redd_se, na.rm = T)^2)
        }
      )
    )


  return_list <- list(
    rch_est = rch_est,
    summ_est = summ_est
  )

  if (use_cor) {
    # generate correlation matrix
    cor_df <- redd_df %>%
      dplyr::group_by(across({{ summ_vars }})) %>%
      tidyr::nest() %>%
      dplyr::summarize(
        cor_mat = purrr::map(data,
          .f = function(x) {
            correlate_rchs(x,
              date_nm = date_nm,
              cor_redd_nm = cor_redd_nm,
              reach_nm = reach_nm,
              use = use,
              method = method,
              ...
            )
          }
        ),
        .groups = "drop"
      )

    return_list$summ_est <- summ_est %>%
      rename(strm_se_naive = strm_se) %>%
      dplyr::left_join(cor_df,
        by = {{ summ_vars }}
      ) %>%
      # use correlations between reaches to get appropriate standard error
      dplyr::mutate(strm_se = purrr::map2_dbl(data,
        cor_mat,
        .f = function(x, y) {
          se <- try(
            deltamethod(as.formula(paste("~", paste0("x", 1:nrow(x), collapse = "+"))),
              mean = x$redd_est,
              cov = diag(
                x = x$redd_se,
                nrow = nrow(y)
              ) %*% y %*%
                diag(
                  x = x$redd_se,
                  nrow = nrow(y)
                )
            ),
            silent = T
          )

          if (inherits(se, "try-error") | is.na(se)) {
            se <- sqrt(sum(x$redd_se^2, na.rm = T))
          }


          return(se)
        }
      ))

    return_list <- c(
      return_list,
      list(cor_df = cor_df)
    )
  }

  return(return_list)
}
