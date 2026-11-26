testthat::test_that("GAUC preserves quasi-Poisson estimates", {
  counts <- data.frame(
    day = 1:11,
    redds = c(1, 2, 4, 8, 14, 19, 13, 8, 4, 2, 1)
  )
  new <- sroem:::fit_gauc(counts, SL = 2, v = -0.2, v_se = 0.1)
  testthat::expect_equal(
    unname(c(new$E, new$E_se, new$Fg, new$F_se, new$beta, new$Ncurve)),
    c(47.70584417370667, 6.550764510999211, 76.32935067793068,
      4.338448732449368, -1.9715919987171162, 1.5772022749557462,
      -0.1317343784571377, 3.795516446473269, 5.986297174009589,
      15.630259734527691),
    tolerance = 1e-8
  )
})

testthat::test_that("GAUC handles negative-binomial and unidentified curves", {
  counts <- data.frame(
    day = 1:11,
    redds = c(1, 0, 7, 3, 21, 12, 19, 3, 6, 0, 1)
  )
  result <- sroem:::fit_gauc(counts, model_fam = "negative.binomial")
  testthat::expect_s3_class(result$model, "glm")
  testthat::expect_true(is.finite(result$E))
  testthat::expect_warning(
    singular <- sroem:::fit_gauc(data.frame(
      day = c(1, 1, 2, 2),
      redds = c(1, 2, 3, 4)
    )),
    "does not define"
  )
  testthat::expect_true(is.na(singular$F_se))
})

testthat::test_that("disabled GAUC reports missing error and uses adjusted totals", {
  surveys <- data.frame(
    river = "R", reach = rep(c("A", "B"), each = 3),
    index = "I", survey_type = "ground",
    new_redds = c(1, 2, 3, 2, 3, 4), visible_redds = 1,
    net_error = c(NA, NA, NA, -0.5, -0.5, -0.5),
    net_error_se = c(NA, NA, NA, 0.1, 0.1, 0.1)
  )
  testthat::expect_message(
    result <- sroem:::estimate_redds(surveys, gauc = FALSE),
    "Assuming no net observer error for:.*reach=A"
  )
  testthat::expect_identical(result$gauc, c(FALSE, FALSE))
  testthat::expect_equal(result$redd_est, c(6, 18))
  testthat::expect_equal(result$err_est, c(0, -0.5))
  testthat::expect_equal(result$err_se, c(0, 0.1))
  testthat::expect_equal(result$redd_se[1], 0)
  testthat::expect_message(
    chinook <- sroem:::estimate_redds(surveys, species = "Spring Chinook"),
    "reach=A"
  )
  testthat::expect_identical(chinook$gauc, c(FALSE, FALSE))
})

testthat::test_that("failed GAUC fits fall back without losing successful fits", {
  surveys <- data.frame(
    reach = rep(c("A", "B"), each = 3),
    new_redds = c(1, 2, 3, 2, 3, 4), visible_redds = 1,
    net_error = -0.5, net_error_se = 0.1
  )
  testthat::local_mocked_bindings(
    fit_gauc = function(data, v, v_se, SL) {
      if (data$redds[1] == 1) {
        stop("simulated fit failure")
      }
      list(
        E = 90 / SL, E_se = 2, beta = c(-1, 1, -1),
        model = list(converged = TRUE)
      )
    },
    .package = "sroem"
  )
  testthat::expect_warning(
    result <- sroem:::estimate_redds(surveys, group_vars = "reach", SL = 2),
    "reach=A: simulated fit failure"
  )
  testthat::expect_identical(result$gauc, c(FALSE, TRUE))
  testthat::expect_identical(result$GAUC, result$gauc)
  testthat::expect_equal(result$redd_est, c(12, 45))
  testthat::expect_equal(result$redd_se[2], 2)
  testthat::expect_s3_class(result$any_error[[1]], "error")
})

testthat::test_that("real GAUC fits preserve existing estimates", {
  surveys <- data.frame(
    reach = "A",
    new_redds = c(0, 0, 1, 2, 4, 8, 15, 12, 8, 3, 1, 0),
    visible_redds = 1, net_error = -0.2, net_error_se = 0.1
  )
  for (use_gauc in c(FALSE, TRUE)) {
    result <- sroem:::estimate_redds(surveys, group_vars = "reach", gauc = use_gauc)
    testthat::expect_identical(result$gauc, use_gauc)
    testthat::expect_equal(result$redd_est, 68)
    testthat::expect_equal(result$redd_se, if (use_gauc) 9.484168939150065 else 8.4375,
                           tolerance = 1e-8)
  }
})

testthat::test_that("missing observer-error columns are treated as unavailable", {
  surveys <- data.frame(
    reach = "A", new_redds = c(0, 0, 1, 2, 4, 8, 15, 12, 8, 3, 1, 0),
    visible_redds = 1
  )
  testthat::expect_message(
    result <- sroem:::estimate_redds(surveys, group_vars = "reach"),
    "Assuming no net observer error for:.*reach=A"
  )
  testthat::expect_true(result$gauc)
  testthat::expect_equal(result$err_est, 0)
  testthat::expect_equal(result$err_se, 0)
  testthat::expect_true(is.finite(result$redd_est))
})

testthat::test_that("missing uncertainty does not erase available net error", {
  surveys <- data.frame(
    reach = "A", new_redds = c(1, 2, 3), visible_redds = 1, net_error = -0.2
  )
  testthat::expect_warning(
    result <- sroem:::estimate_redds(surveys, group_vars = "reach", gauc = FALSE),
    "Observer-error standard error unavailable for: reach=A"
  )
  testthat::expect_equal(result$err_est, -0.2)
  testthat::expect_true(is.na(result$redd_se))
})

testthat::test_that("real fitting failures use adjusted total counts", {
  surveys <- data.frame(
    reach = "A", new_redds = c(NA, 1, 3),
    visible_redds = 1, net_error = -0.2, net_error_se = 0.1
  )
  testthat::expect_warning(
    result <- sroem:::estimate_redds(surveys, group_vars = "reach", add_zeros = TRUE),
    "GAUC fitting failed; using adjusted totals for: reach=A"
  )
  testthat::expect_false(result$gauc)
  testthat::expect_equal(result$redd_est, 5)
  testthat::expect_s3_class(result$any_error[[1]], "error")
})

testthat::test_that("missing net error identifies reaches outside grouping keys", {
  surveys <- data.frame(
    river = "R", reach = c("A", "B"), new_redds = c(1, 2),
    visible_redds = 1, net_error = NA_real_, net_error_se = NA_real_
  )
  testthat::expect_message(
    result <- sroem:::estimate_redds(surveys, group_vars = "river", gauc = FALSE),
    "river=R, reach=A, B"
  )
  testthat::expect_false(result$gauc)
  testthat::expect_equal(result$redd_est, 3)
})

testthat::test_that("unavailable correlations become zero with named reach pairs", {
  surveys <- data.frame(
    river = "R", location = "L", reach = rep(c("A", "B", "C"), each = 4),
    survey_type = rep(c("Z", "A", "C"), each = 4),
    survey_date = rep(as.Date("2024-01-01") + 7 * 0:3, 3),
    new_redds = c(1, 2, 3, 4, 2, 4, 6, 8, 2, 2, 2, 2),
    visible_redds = 1, net_error = 0, net_error_se = 0.1
  )
  messages <- character()
  result <- withCallingHandlers(
    sroem:::summarize_redds(
      surveys,
      group_vars = c("survey_type", "reach"),
      summ_vars = c("river", "location"), gauc = FALSE, use_cor = TRUE
    ),
    message = function(m) {
      messages <<- c(messages, conditionMessage(m))
      invokeRestart("muffleMessage")
    },
    warning = function(w) {
      if (grepl("standard deviation is zero", conditionMessage(w))) {
        invokeRestart("muffleWarning")
      }
    }
  )
  mat <- result$cor_df$cor_mat[[1]]
  testthat::expect_equal(mat["A", "B"], 1)
  testthat::expect_equal(mat["A", "C"], 0)
  testthat::expect_equal(mat["B", "C"], 0)
  testthat::expect_equal(diag(mat), c(A = 1, B = 1, C = 1))
  testthat::expect_true(any(grepl("A and C; B and C", messages, fixed = TRUE)))
  testthat::expect_true(any(grepl("river=R, location=L", messages, fixed = TRUE)))
  testthat::expect_equal(result$summ_est$strm_se, sqrt((1 + 2)^2 + 0.8^2))
  testthat::expect_equal(result$summ_est$strm_se_naive, sqrt(1^2 + 2^2 + 0.8^2))
})

testthat::test_that("complete correlation failure uses independent reaches", {
  surveys <- data.frame(
    river = "R", reach = rep(c("A", "B"), each = 2),
    survey_date = as.Date("2024-01-01") + c(0, 7, 28, 35),
    new_redds = c(1, 2, 3, 4), visible_redds = 1,
    net_error = 0, net_error_se = 0.1
  )
  messages <- character()
  result <- withCallingHandlers(
    sroem:::summarize_redds(
      surveys,
      group_vars = "reach", summ_vars = "river",
      gauc = FALSE, use_cor = TRUE, use = "complete.obs"
    ),
    message = function(m) {
      messages <<- c(messages, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  testthat::expect_true(any(grepl("A and B", messages, fixed = TRUE)))
  testthat::expect_equal(result$cor_df$cor_mat[[1]]["A", "B"], 0)
  testthat::expect_equal(result$summ_est$strm_se, result$summ_est$strm_se_naive)
})

testthat::test_that("same-reach estimates without identifiable correlation use zero", {
  surveys <- data.frame(
    river = "R", reach = "A", survey_type = rep(c("ground", "boat"), each = 3),
    survey_date = rep(as.Date("2024-01-01") + 7 * 0:2, 2),
    new_redds = c(1, 2, 3, 2, 3, 4), visible_redds = 1,
    net_error = 0, net_error_se = 0.1
  )
  testthat::expect_message(
    result <- sroem:::summarize_redds(
      surveys,
      group_vars = c("reach", "survey_type"), summ_vars = "river",
      gauc = FALSE, use_cor = TRUE
    ),
    "multiple estimates of reach A"
  )
  testthat::expect_equal(result$summ_est$strm_se, sqrt(0.6^2 + 0.9^2))
})

testthat::test_that("a single reach needs no correlation fallback", {
  surveys <- data.frame(
    river = "R", reach = "A", survey_date = as.Date("2024-01-01") + 7 * 0:2,
    new_redds = 1:3, visible_redds = 1, net_error = 0, net_error_se = 0.1
  )
  result <- sroem:::summarize_redds(
    surveys,
    group_vars = "reach", summ_vars = c("river", "reach"),
    gauc = FALSE, use_cor = TRUE
  )
  testthat::expect_equal(result$summ_est$n_rchs, 1)
  testthat::expect_equal(result$summ_est$strm_se, result$summ_est$strm_se_naive)
  testthat::expect_equal(unname(result$cor_df$cor_mat[[1]]), matrix(1, 1, 1))
})

testthat::test_that("independent summaries preserve the original results", {
  surveys <- data.frame(
    river = "R", reach = rep(c("A", "B"), each = 3),
    new_redds = 1:6, visible_redds = 1, net_error = -0.2, net_error_se = 0.1
  )
  result <- sroem:::summarize_redds(
    surveys,
    group_vars = "reach", summ_vars = "river", gauc = FALSE
  )
  testthat::expect_equal(result$summ_est$n_rchs, 2)
  testthat::expect_equal(result$summ_est$strm_obs, 21)
  testthat::expect_equal(result$summ_est$strm_est, 27)
  testthat::expect_equal(result$summ_est$strm_se, sqrt((6 * .1 / .8^2)^2 +
                                                        (15 * .1 / .8^2)^2))
})

testthat::test_that("correlations distinguish years and support seasonal compatibility", {
  surveys <- data.frame(
    reach = rep(c("A", "B"), each = 4),
    survey_date = rep(as.Date(c(
      "2023-01-01", "2023-01-08",
      "2024-01-01", "2024-01-08"
    )), 2),
    new_redds = c(1, 2, 10, 20, 2, 1, 20, 10)
  )
  args <- list(
    redd_df = surveys, date_nm = "survey_date",
    cor_redd_nm = "new_redds", reach_nm = "reach"
  )
  pooled <- do.call(sroem:::correlate_rchs, args)
  legacy <- do.call(sroem:::correlate_rchs, c(args, list(year_mode = "legacy")))
  annual <- do.call(sroem:::correlate_rchs, c(args, list(year_mode = "per_year")))
  testthat::expect_equal(pooled["A", "B"], stats::cor(c(1, 2, 10, 20), c(2, 1, 20, 10)))
  testthat::expect_equal(legacy["A", "B"], -1)
  testthat::expect_named(annual, c("2023", "2024"))
  testthat::expect_equal(annual[["2023"]]["A", "B"], -1)
  testthat::expect_equal(annual[["2024"]]["A", "B"], -1)
})

testthat::test_that("undefined correlations report reach pairs and assume independence", {
  surveys <- data.frame(
    reach = rep(c("A", "B", "C"), each = 3),
    survey_date = rep(as.Date("2024-01-01") + 7 * 0:2, 3),
    new_redds = c(1, 2, 3, 2, 4, 6, 1, 1, 1)
  )
  testthat::expect_message(
    mat <- sroem:::correlate_rchs(
      surveys,
      date_nm = "survey_date", cor_redd_nm = "new_redds", reach_nm = "reach"
    ),
    "A and C; B and C.*Assuming independence"
  )
  testthat::expect_equal(mat["A", "B"], 1)
  testthat::expect_equal(mat["A", "C"], 0)
  testthat::expect_equal(mat["B", "C"], 0)
  testthat::expect_equal(diag(mat), c(A = 1, B = 1, C = 1))
  testthat::expect_error(
    sroem:::correlate_rchs(surveys,
      date_nm = "survey_date",
      reach_nm = "reach", make_na_zero = FALSE
    ),
    "must be TRUE"
  )
})

testthat::test_that("correlations respect missing-data methods and reach names", {
  values <- cbind(
    week = c(1, 2, 3, 5, 6), year = c(2, 4, NA, 10, 12),
    C = c(5, NA, 3, 1, 0)
  )
  surveys <- data.frame(
    reach = rep(colnames(values), each = nrow(values)),
    survey_date = rep(as.Date("2024-01-01") + 7 * 0:4, ncol(values)),
    new_redds = as.vector(values)
  )
  for (method in c("pearson", "spearman", "kendall")) {
    for (use in c("everything", "complete.obs", "na.or.complete", "pairwise.complete.obs")) {
      result <- suppressMessages(sroem:::correlate_rchs(
        surveys,
        date_nm = "survey_date", cor_redd_nm = "new_redds",
        reach_nm = "reach", use = use, method = method
      ))
      expected <- stats::cor(values, use = use, method = method)
      expected[is.na(expected)] <- 0
      diag(expected) <- 1
      testthat::expect_equal(result[colnames(values), colnames(values)], expected)
    }
  }
  testthat::expect_message(
    all_obs <- sroem:::correlate_rchs(
      surveys,
      date_nm = "survey_date", cor_redd_nm = "new_redds",
      reach_nm = "reach", use = "all.obs"
    ),
    "Assuming independence"
  )
  testthat::expect_equal(unname(all_obs), diag(3))
})

testthat::test_that("signed error correlations can drive summary uncertainty", {
  surveys <- data.frame(
    river = "R", reach = rep(c("A", "B"), each = 4),
    survey_date = rep(as.Date("2024-01-01") + 7 * 0:3, 2),
    new_redds = c(1, 2, 3, 4, 2, 4, 6, 8),
    signed_error = c(-2, -1, 1, 2, 2, 1, -1, -2),
    visible_redds = 1, net_error = 0, net_error_se = 0.1
  )
  args <- list(
    redd_df = surveys, date_nm = "survey_date", reach_nm = "reach",
    correlation_data = "errors", error_nm = "signed_error"
  )
  testthat::expect_equal(do.call(sroem:::correlate_rchs, args)["A", "B"], -1)
  result <- sroem:::summarize_redds(
    surveys,
    group_vars = "reach", summ_vars = "river", gauc = FALSE,
    use_cor = TRUE, correlation_data = "errors", error_nm = "signed_error"
  )
  testthat::expect_equal(result$cor_df$cor_mat[[1]]["A", "B"], -1)
  testthat::expect_equal(result$summ_est$strm_se, 1)
  testthat::expect_error(
    sroem:::summarize_redds(
      surveys,
      group_vars = "reach", summ_vars = "river", gauc = FALSE,
      use_cor = TRUE, correlation_data = "errors"
    ),
    "error_nm"
  )
})

testthat::test_that("per-year summary correlations require one year per group", {
  surveys <- data.frame(
    river = "R", reach = rep(c("A", "B"), each = 4),
    survey_date = rep(as.Date(c(
      "2023-01-01", "2023-01-08",
      "2024-01-01", "2024-01-08"
    )), 2),
    new_redds = c(1, 2, 10, 20, 2, 1, 20, 10),
    visible_redds = 1, net_error = 0, net_error_se = 0.1
  )
  surveys$year <- lubridate::year(surveys$survey_date)
  testthat::expect_error(
    sroem:::summarize_redds(
      surveys,
      group_vars = "reach", summ_vars = "river",
      gauc = FALSE, use_cor = TRUE, year_mode = "per_year"
    ),
    "one calendar year"
  )
  result <- sroem:::summarize_redds(
    surveys,
    group_vars = "reach", summ_vars = c("river", "year"),
    gauc = FALSE, use_cor = TRUE, year_mode = "per_year"
  )
  testthat::expect_equal(nrow(result$summ_est), 2)
  testthat::expect_equal(result$cor_df$cor_mat[[1]]["A", "B"], -1)
  testthat::expect_equal(result$cor_df$cor_mat[[2]]["A", "B"], -1)
})

testthat::test_that("beta posterior handles zero and complete observed error rates", {
  for (false in c(0, 100)) {
    result <- sroem:::boot_adj_prop(
      b_x = 100, b_y = 100, false_x = false, false_y = false,
      samp_x = 5000, samp_y = 5000, n_boot = 4000, seed = 12
    )
    expected_mean <- (false + 0.5) / 101
    testthat::expect_lt(abs(mean(result$err_x) - expected_mean), 0.0007)
    testthat::expect_lt(abs(mean(result$err_y) - expected_mean), 0.0007)
    testthat::expect_true(all(is.finite(result$prop_x_boot)))
    testthat::expect_equal(nrow(result), 4000)
  }
})

testthat::test_that("fixed zero can be selected independently by true sex", {
  x_fixed <- sroem:::boot_adj_prop(
    b_x = 20, b_y = 20, false_x = 0, false_y = 2,
    samp_x = 50, samp_y = 50, n_boot = 100, seed = 4, zero_error = "x"
  )
  testthat::expect_true(all(x_fixed$err_x == 0))
  testthat::expect_true(all(x_fixed$err_y > 0))
  both_fixed <- sroem:::boot_adj_prop(
    b_x = 20, b_y = 20, false_x = 0, false_y = 0,
    samp_x = 50, samp_y = 50, n_boot = 100, seed = 4,
    zero_error = c("x", "y")
  )
  testthat::expect_true(all(both_fixed$err_x == 0 & both_fixed$err_y == 0))
  testthat::expect_true(all(both_fixed$x_se == 0))
  testthat::expect_equal(both_fixed$prop_x_boot, both_fixed$samp_x / both_fixed$samp_n)
  testthat::expect_error(
    sroem:::boot_adj_prop(20, 20, 1, 0, 50, 50, zero_error = "x"),
    "false calls were observed"
  )
})

testthat::test_that("beta-prior uncertainty matches the delta method", {
  result <- sroem:::boot_adj_prop(
    b_x = 20, b_y = 30, false_x = 3, false_y = 4,
    samp_x = 60, samp_y = 40, n_boot = 50, seed = 7, prior = c(1, 1)
  )
  beta_se <- function(a, b) sqrt(a * b / ((a + b)^2 * (a + b + 1)))
  expected <- msm::deltamethod(
    ~ ((x1 / x2) - x3) / (1 - x3 - x4) * x2,
    mean = c(result$samp_x[1], result$samp_n[1], result$err_y[1], result$err_x[1]),
    cov = diag(c(0, 0, beta_se(5, 27), beta_se(4, 18))^2)
  )
  testthat::expect_equal(result$x_se[1], expected, tolerance = 1e-8)
  ratio_se <- msm::deltamethod(
    ~ x1 / (x2 - x1),
    mean = c(result$prop_x_boot[1] * result$samp_n[1], result$samp_n[1]),
    cov = diag(c(result$x_se[1], 0)^2)
  )
  testthat::expect_equal(result$x_y_se[1], ratio_se, tolerance = 1e-8)
  testthat::expect_s3_class(sroem::est_adj_prop(result, estimate = "prop_x"), "tbl_df")
  again <- sroem:::boot_adj_prop(
    20, 30, 3, 4, 60, 40,
    n_boot = 50, seed = 7, prior = c(1, 1)
  )
  testthat::expect_identical(result, again)
})

testthat::test_that("unsupported or impossible bootstrap inputs are explicit", {
  testthat::expect_error(
    sroem:::boot_adj_prop(20, 20, 21, 0, 50, 50),
    "cannot exceed"
  )
  testthat::expect_error(
    sroem:::boot_adj_prop(20, 20, 0, 0, 50, 50, prior = c(0, 1)),
    "positive finite"
  )
  testthat::expect_error(
    sroem:::boot_adj_prop(20, 20, 1, 1, 0, 10, n_boot = 10, max_batches = 2),
    "Only 0 valid bootstrap draws"
  )
  testthat::expect_warning(
    sroem:::boot_adj_prop(0, 20, 0, 0, 50, 50, n_boot = 10, seed = 4),
    "determined entirely by the prior"
  )
})

testthat::test_that("discharge requests are deduplicated and missing surveys remain", {
  testthat::skip_if_not_installed("writexl")
  file <- tempfile(fileext = ".xlsx")
  on.exit(unlink(file), add = TRUE)
  surveys <- data.frame(
    id = seq_len(13),
    river = c(rep("Wenatchee", 3), rep("Entiat", 2), rep("Other", 8)),
    reach = c(rep("W10", 3), rep("B", 2), rep("C", 4), "D", "E", "F", "H"),
    index = "I", survey_type = "ground", spawn_year = 2024,
    survey_date = as.Date(c(
      "2024-01-03", "2024-01-03", "2024-01-10", "2024-01-03", "2024-01-10",
      "2024-01-03", "2024-01-10", "2024-01-17", "2025-01-03",
      NA, "2024-01-03", "2024-01-03", "2024-01-03"
    )),
    new_redds = 1, visible_redds = 1, surveyors = "AA",
    surveyor1 = NA_character_, surveyor2 = NA_character_
  )
  reaches <- unique(surveys[c("river", "reach", "index")])
  reach_lengths <- transform(reaches, type = "Index", length_km = 1)
  thalweg <- transform(reaches[c("river", "reach")], mean_thalweg_cv = 30)
  gages <- data.frame(
    reach = c("W10", "W9", "B", "C", "D", "E", "F", "H"),
    site_code = c(
      "12345678", "87654321", "22222222", "46X111",
      NA, "99999999", "33333333", "87654321"
    )
  )
  writexl::write_xlsx(
    list(
      "Redd Surveys" = surveys, "Reach Length" = reach_lengths,
      "Thalweg CV" = thalweg, "Discharge Gages" = gages
    ),
    file
  )
  calls <- new.env(parent = emptyenv())
  calls$usgs <- character()
  calls$ecology <- character()
  testthat::local_mocked_bindings(
    .sroem_read_usgs_daily = function(site, date) {
      calls$usgs <- c(calls$usgs, paste(site, date))
      if (site == "99999999") stop("simulated network failure")
      if (site == "33333333" || (site == "87654321" && date == as.Date("2024-01-10"))) {
        return(NA_real_)
      }
      switch(site, "12345678" = 20, "87654321" = 50, "22222222" = 100)
    },
    .sroem_read_ecology_year = function(site, year) {
      calls$ecology <- c(calls$ecology, paste(site, year))
      values <- if (site == "46B060") c(30, NA, 40) else c(12, NA, 14)
      data.frame(
        date = as.Date(paste0(year, c("-01-03", "-01-10", "-01-17"))),
        value = values
      )
    },
    .package = "sroem"
  )
  messages <- warnings <- character()
  result <- withCallingHandlers(
    sroem:::query_redd_data(dirname(file), basename(file), query_year = 2024),
    message = function(m) {
      messages <<- c(messages, conditionMessage(m))
      invokeRestart("muffleMessage")
    },
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  result <- result[order(result$id), ]
  testthat::expect_equal(result$id, surveys$id)
  testthat::expect_equal(
    result$mean_discharge[c(1, 2, 4, 6, 8, 9, 13)],
    c(30, 30, 70, 12, 14, 12, 50)
  )
  testthat::expect_true(all(is.na(result$mean_discharge[c(3, 5, 7, 10, 11, 12)])))
  testthat::expect_false(anyDuplicated(calls$usgs) > 0)
  testthat::expect_equal(sort(calls$ecology), sort(c("46B060 2024", "46X111 2024", "46X111 2025")))
  testthat::expect_true(any(grepl("99999999.*simulated network failure", warnings)))
  report <- paste(messages, collapse = " ")
  for (entry in c(
    "reach=W10, date=2024-01-10", "reach=B, date=2024-01-10",
    "reach=C, date=2024-01-10", "reach=D, date=NA",
    "reach=E, date=2024-01-03", "reach=F, date=2024-01-03"
  )) {
    testthat::expect_true(grepl(entry, report, fixed = TRUE), info = entry)
  }

  writexl::write_xlsx(
    list(
      "Redd Surveys" = surveys, "Reach Length" = reach_lengths,
      "Thalweg CV" = thalweg, "Discharge Gages" = gages[gages$reach != "W9", ]
    ),
    file
  )
  warnings <- character()
  without_plain <- withCallingHandlers(
    sroem:::query_redd_data(dirname(file), basename(file), query_year = 2024),
    message = function(m) invokeRestart("muffleMessage"),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  testthat::expect_equal(nrow(without_plain), nrow(surveys))
  testthat::expect_true(all(is.na(without_plain$mean_discharge[without_plain$reach == "W10"])))
  testthat::expect_true(any(grepl("unique W9 Plain gage", warnings, fixed = TRUE)))
})

testthat::test_that("experience workbooks and entirely missing dates remain usable", {
  testthat::skip_if_not_installed("writexl")
  folder <- tempfile()
  dir.create(folder)
  file <- file.path(folder, "Wenatchee_fixture.xlsx")
  on.exit(
    {
      unlink(file)
      unlink(folder)
    },
    add = TRUE
  )
  survey <- data.frame(
    river = "Wenatchee", reach = "A", index = "I", survey_type = "ground",
    spawn_year = 2024, survey_date = as.Date(NA),
    new_redds = 2, visible_redds = 10, surveyors = "AA, BB (note)",
    surveyor1 = NA_character_, surveyor2 = NA_character_
  )
  experience <- data.frame(
    a = c(NA, "Wenatchee", "Wenatchee"), b = c(NA, "WDFW", "WDFW"),
    c = c(NA, "Name A", "Name B"), d = c(NA, "AA", "BB"),
    e = c(2024, 3, 5)
  )
  writexl::write_xlsx(
    list(
      "Redd Surveys" = survey,
      "Reach Length" = data.frame(
        river = "Wenatchee", reach = "A",
        type = "Index", index = "I", length_km = 2
      ),
      "Thalweg CV" = data.frame(river = "Wenatchee", reach = "A", mean_thalweg_cv = 30),
      "Discharge Gages" = data.frame(reach = "A", site_code = NA_character_),
      "Experience" = experience
    ),
    file
  )
  messages <- character()
  result <- withCallingHandlers(
    sroem:::query_redd_data(folder, basename(file), query_year = 2024),
    message = function(m) {
      messages <<- c(messages, conditionMessage(m))
      invokeRestart("muffleMessage")
    },
    warning = function(w) invokeRestart("muffleWarning")
  )
  testthat::expect_equal(nrow(result), 1)
  testthat::expect_equal(result$exp_sp_total, 4)
  testthat::expect_equal(result$naive_density_km, 5)
  testthat::expect_true(is.na(result$mean_discharge))
  testthat::expect_true(any(grepl("reach=A, date=NA", messages, fixed = TRUE)))
})

testthat::test_that("Methow and Entiat experience layouts are supported offline", {
  testthat::skip_if_not_installed("writexl")
  folder <- tempfile()
  dir.create(folder)
  files <- file.path(folder, c("Methow_fixture.xlsx", "Entiat_fixture.xlsx"))
  on.exit(
    {
      unlink(files)
      unlink(folder)
    },
    add = TRUE
  )
  calls <- new.env(parent = emptyenv())
  calls$n <- 0
  unexpected_request <- function(...) {
    calls$n <- calls$n + 1
    stop("Unexpected network request")
  }
  testthat::local_mocked_bindings(
    .sroem_read_usgs_daily = unexpected_request,
    .sroem_read_ecology_year = unexpected_request,
    .package = "sroem"
  )
  for (i in seq_along(files)) {
    basin <- c("Methow", "Entiat")[i]
    survey <- data.frame(
      river = basin, reach = "A", index = "I", survey_type = "ground",
      spawn_year = 2024, survey_date = as.Date(NA),
      new_redds = 2, visible_redds = 10, surveyors = "AA, BB (note)",
      surveyor1 = NA_character_, surveyor2 = NA_character_
    )
    experience <- data.frame(
      basin = c(NA, basin, basin), name = c(NA, "Name A", "Name B"),
      initials = c(NA, "AA", "BB"), year = c(2024, 3, 5)
    )
    if (basin == "Methow") {
      experience <- cbind(experience[1],
        agency = c(NA, "WDFW", "WDFW"),
        experience[-1]
      )
    }
    writexl::write_xlsx(
      list(
        "Redd Surveys" = survey,
        "Reach Length" = data.frame(
          river = basin, reach = "A", type = "Index", index = "I", length_km = 2
        ),
        "Thalweg CV" = data.frame(river = basin, reach = "A", mean_thalweg_cv = 30),
        "Discharge Gages" = data.frame(reach = "A", site_code = NA_character_),
        "Experience" = experience
      ),
      files[i]
    )
    result <- suppressWarnings(suppressMessages(
      sroem:::query_redd_data(folder, basename(files[i]), query_year = 2024)
    ))
    testthat::expect_equal(result$exp_sp_total, 4)
    testthat::expect_equal(result$naive_density_km, 5)
    testthat::expect_true(is.na(result$mean_discharge))
  }
  testthat::expect_equal(calls$n, 0)
})

testthat::test_that("successful ordinary surveys preserve original output", {
  testthat::skip_if_not_installed("writexl")
  folder <- tempfile()
  dir.create(folder)
  file <- file.path(folder, "Wenatchee_fixture.xlsx")
  on.exit(
    {
      unlink(file)
      unlink(folder)
    },
    add = TRUE
  )
  survey <- data.frame(
    river = "Wenatchee", reach = "A", index = "I", survey_type = "ground",
    spawn_year = 2024, survey_date = as.Date(c("2024-01-03", "2024-01-10")),
    new_redds = c(2, 4), visible_redds = c(10, 12),
    surveyors = c("AA, BB (note)", "AA"), surveyor1 = NA_character_,
    surveyor2 = NA_character_
  )
  experience <- data.frame(
    a = c(NA, "Wenatchee", "Wenatchee"), b = c(NA, "WDFW", "WDFW"),
    c = c(NA, "Name A", "Name B"), d = c(NA, "AA", "BB"),
    e = c(2024, 3, 5)
  )
  writexl::write_xlsx(
    list(
      "Redd Surveys" = survey,
      "Reach Length" = data.frame(
        river = "Wenatchee", reach = "A", type = "Index", index = "I", length_km = 2
      ),
      "Thalweg CV" = data.frame(river = "Wenatchee", reach = "A", mean_thalweg_cv = 30),
      "Discharge Gages" = data.frame(reach = "A", site_code = "12345678"),
      "Experience" = experience
    ), file
  )
  testthat::local_mocked_bindings(
    read_waterdata_daily = function(...) data.frame(value = 50),
    .package = "dataRetrieval"
  )
  result <- suppressWarnings(suppressMessages(
    sroem:::query_redd_data(folder, basename(file), query_year = 2024)
  ))
  testthat::expect_equal(result$new_redds, c(2, 4))
  testthat::expect_equal(result$mean_discharge, c(50, 50))
  testthat::expect_equal(result$exp_sp_total, c(4, 3))
  testthat::expect_equal(result$naive_density_km, c(5, 6))
})

testthat::test_that("discharge readers validate and parse service responses offline", {
  usgs_response <- data.frame(value = c(3, 3))
  testthat::local_mocked_bindings(
    read_waterdata_daily = function(...) usgs_response,
    .package = "dataRetrieval"
  )
  testthat::expect_equal(
    sroem:::.sroem_read_usgs_daily("12345678", as.Date("2024-01-03")), 3
  )
  usgs_response <- data.frame(value = c(3, 4))
  testthat::expect_error(
    sroem:::.sroem_read_usgs_daily("12345678", as.Date("2024-01-03")), "ambiguous"
  )
  ecology_response <- data.frame(
    X1 = c("01/03/2024", "01/10/2024"), X15 = c(10, 20), X16 = c(11, NA)
  )
  testthat::local_mocked_bindings(
    read_delim = function(...) ecology_response,
    .package = "readr"
  )
  parsed <- sroem:::.sroem_read_ecology_year("46X111", 2024)
  testthat::expect_equal(parsed$date, as.Date(c("2024-01-03", "2024-01-10")))
  testthat::expect_equal(parsed$value, c(11, 20))
  ecology_response$X1[2] <- ecology_response$X1[1]
  testthat::expect_error(sroem:::.sroem_read_ecology_year("46X111", 2024), "duplicate")
})

testthat::test_that("covariate comparison preserves values and avoids duplicates", {
  model <- get("one_obs_net_mod", envir = asNamespace("sroem"))
  centers <- get("one_obs_covar_center", envir = asNamespace("sroem"))
  terms <- attr(model$terms, "term.labels")
  inputs <- model$data[1:6, terms, drop = FALSE]
  for (term in terms) {
    center <- centers[centers$metric == term, ]
    inputs[[term]] <- inputs[[term]] * center$sd + center$mean
  }
  for (scaled in c(FALSE, TRUE)) {
    new <- sroem:::compare_covars(inputs,
      num_obs = "one",
      z_score = scaled, incl_neterr = FALSE
    )
    testthat::expect_false(any(new$covariate == "net_error"))
    term <- terms[vapply(model$data[terms], is.numeric, logical(1))][1]
    center <- centers[centers$metric == term, ]
    expected <- if (scaled) model$data[[term]] else model$data[[term]] * center$sd + center$mean
    testthat::expect_equal(
      new$value[new$source == "Model Data" & new$covariate == term],
      expected, tolerance = 1e-8
    )
    with_error <- sroem:::compare_covars(inputs,
      num_obs = "one",
      z_score = scaled, incl_neterr = TRUE
    )
    testthat::expect_equal(
      sum(with_error$covariate == "net_error" &
        with_error$source == "Model Data"),
      nrow(model$data)
    )
    testthat::expect_equal(
      sum(with_error$covariate == "net_error" &
        with_error$source == "Predictive Data"),
      nrow(inputs)
    )
  }
})

testthat::test_that("covariate comparison works for two observers and Chinook", {
  cases <- list(
    list(
      model = "two_obs_net_mod", center = "two_obs_covar_center",
      species = "Steelhead"
    ),
    list(
      model = "chnk_net_mod", center = "chnk_covar_center",
      species = "Spring Chinook"
    )
  )
  for (case in cases) {
    model <- get(case$model, envir = asNamespace("sroem"))
    centers <- get(case$center, envir = asNamespace("sroem"))
    terms <- attr(model$terms, "term.labels")
    inputs <- model$data[1:6, terms, drop = FALSE]
    for (term in intersect(terms, centers$metric)) {
      center <- centers[centers$metric == term, ]
      inputs[[term]] <- inputs[[term]] * center$sd + center$mean
    }
    for (scaled in c(FALSE, TRUE)) {
      new <- sroem:::compare_covars(inputs,
        species = case$species,
        z_score = scaled, incl_neterr = FALSE
      )
      testthat::expect_false(any(new$covariate == "net_error"))
      term <- terms[vapply(model$data[terms], is.numeric, logical(1))][1]
      center <- centers[centers$metric == term, ]
      expected <- if (scaled) model$data[[term]] else model$data[[term]] * center$sd + center$mean
      testthat::expect_equal(
        new$value[new$source == "Model Data" & new$covariate == term],
        expected, tolerance = 1e-8
      )
    }
  }
})

testthat::test_that("prediction agrees across models and retains missing rows", {
  cases <- list(
    list(
      model = "one_obs_net_mod", center = "one_obs_covar_center",
      species = "Steelhead", observers = "one"
    ),
    list(
      model = "two_obs_net_mod", center = "two_obs_covar_center",
      species = "Steelhead", observers = "two"
    ),
    list(
      model = "chnk_net_mod", center = "chnk_covar_center",
      species = "Spring Chinook", observers = "two"
    )
  )
  for (case in cases) {
    model <- get(case$model, envir = asNamespace("sroem"))
    centers <- get(case$center, envir = asNamespace("sroem"))
    terms <- attr(model$terms, "term.labels")
    inputs <- model$data[1:6, terms, drop = FALSE]
    for (term in intersect(terms, centers$metric)) {
      center <- centers[centers$metric == term, ]
      inputs[[term]] <- inputs[[term]] * center$sd + center$mean
    }
    inputs <- rbind(inputs, inputs[1, , drop = FALSE])
    new <- sroem:::predict_neterr(inputs,
      species = case$species,
      num_obs = case$observers, err_floor = FALSE, err_ceiling = FALSE
    )
    scaled <- rbind(model$data[1:6, terms, drop = FALSE],
                    model$data[1, terms, drop = FALSE])
    expected <- stats::predict(model, newdata = scaled, type = "link", se.fit = TRUE)
    testthat::expect_equal(unname(new$net_error), unname(expected$fit), tolerance = 1e-8)
    testthat::expect_equal(unname(new$net_error_se), unname(expected$se.fit), tolerance = 1e-8)

    inputs[[terms[length(terms)]]][2] <- NA
    new <- sroem:::predict_neterr(inputs,
      species = case$species,
      num_obs = case$observers
    )
    testthat::expect_equal(nrow(new), nrow(inputs))
    testthat::expect_true(is.na(new$net_error[2]))
    testthat::expect_true(is.finite(new$net_error[1]))
  }
})

testthat::test_that("prediction derives covariates and applies explicit limits", {
  inputs <- data.frame(
    exp_sp_total = c(10, 20),
    mean_discharge = c(500, 600),
    mean_thalweg_cv = c(30, 40),
    visible_redds = c(10, 20),
    reach_length_km = c(5, 5)
  )
  new <- sroem:::predict_neterr(inputs, err_min = -0.3, err_max = 0.1)
  testthat::expect_equal(new$naive_density_km, c(2, 4))
  testthat::expect_true(all(new$net_error >= -0.3 & new$net_error <= 0.1))
  testthat::expect_error(sroem:::predict_neterr(data.frame()), "Missing covariates")
})

testthat::test_that("DABOM loads only the requested saved object", {
  root <- tempfile()
  dir.create(root)
  dir.create(file.path(root, "RockIsland"))
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  dam_escp_df <- data.frame(value = 42)
  result_type <- "tag_summ"
  save(dam_escp_df, result_type,
    file = file.path(root, "RockIsland", "test_2020.rda")
  )
  testthat::expect_equal(
    sroem:::query_dabom_results(root, "RockIsland", "test_", 2020, "dam_summ"),
    dam_escp_df
  )
  testthat::expect_error(
    sroem:::query_dabom_results(root, "RockIsland", "test_", 2020, "tag_summ"),
    "does not contain 'tag_summ'"
  )
  testthat::expect_error(
    sroem:::query_dabom_results(root, "RockIsland", "test_", 2019, "dam_summ"),
    "file not found"
  )
})

testthat::test_that("unified basin preparation returns and saves complete results", {
  years <- 2023:2024
  tags <- tidyr::expand_grid(
    spawn_year = years, tag_code = c("M1", "M2", "F1", "F2")
  ) |>
    dplyr::mutate(
      path = "LMR LWE", final_node = "LMR",
      origin = dplyr::if_else(tag_code %in% c("M1", "F1"), "W", "H"),
      sex = dplyr::if_else(tag_code %in% c("M1", "M2"), "M", "F"),
      sex = dplyr::if_else(spawn_year == 2023 & tag_code == "M1", "F", sex),
      ad_clip = FALSE, cwt = FALSE
    )
  brood <- tags |>
    dplyr::transmute(
      spawn_year,
      recaptured_pit = tag_code,
      sex_final = dplyr::if_else(tag_code %in% c("M1", "M2"), "M", "F")
    )
  testthat::local_mocked_bindings(
    read_excel = function(...) brood,
    .package = "readxl"
  )

  original_boot <- sroem::boot_adj_prop
  basin <- "Methow"
  have_redd <- TRUE
  mock_redd <- function(..., query_year) {
    if (!have_redd) return(NULL)
    data.frame(
      spawn_year = query_year, reach = if (basin == "Methow") "MH1" else "W8",
      new_redds = 1
    )
  }
  mock_predict <- function(data, ...) {
    dplyr::mutate(data, net_error = 0)
  }
  mock_dabom <- function(..., query_year, result_type) {
    if (result_type == "tag_summ") {
      return(dplyr::filter(tags, spawn_year == query_year))
    }
    if (result_type == "escape_summ") {
      return(tidyr::expand_grid(
        spawn_year = query_year, origin = c("W", "H"),
        location = c("LMR", "LMR_bb", "LWE", "LWE_bb", "TWR")
      ) |>
        dplyr::mutate(median = 50, sd = 4, lower_ci = 40, upper_ci = 60))
    }
    tidyr::expand_grid(
      origin = c("W", "H"),
      param = c("LMR", "LMR_bb", "LWE", "LWE_bb"),
      chain = 1L, iter = 1:20
    ) |>
      dplyr::mutate(abund = 40 + iter)
  }
  calls <- list()
  mock_boot <- function(...) {
    args <- list(...)
    calls[[length(calls) + 1L]] <<- args
    do.call(original_boot, args)
  }
  testthat::local_mocked_bindings(
    query_redd_data = mock_redd, query_dabom_results = mock_dabom,
    predict_neterr = mock_predict, boot_adj_prop = mock_boot,
    .package = "sroem"
  )

  folder <- tempfile()
  dir.create(folder)
  on.exit(unlink(folder, recursive = TRUE), add = TRUE)
  globals <- c(
    "redd_df", "basin_tags", "sex_err", "fpr_df",
    "trib_spawners", "escp_est", "rem_df"
  )
  old_global <- mget(globals[globals %in% ls(.GlobalEnv, all.names = TRUE)],
    envir = .GlobalEnv
  )
  on.exit(
    {
      rm(
        list = globals[globals %in% ls(.GlobalEnv, all.names = TRUE)],
        envir = .GlobalEnv
      )
      list2env(old_global, envir = .GlobalEnv)
    },
    add = TRUE
  )
  assign("fpr_df", "unchanged", envir = .GlobalEnv)
  result <- suppressMessages(sroem:::prep_uc_sthd_data(
    basin = basin, query_year = years, phos_data = "tags",
    removal_file_path = folder, save_rda = TRUE, save_file_path = folder,
    sex_error_prior = c(1, 2),
    sex_zero_error = list("2023" = "y", "2024" = c("x", "y"))
  ))
  testthat::expect_identical(names(result), c(
    "redd_df", "basin_tags", "sex_err", "fpr_df",
    "trib_spawners", "escp_est", "rem_df"
  ))
  testthat::expect_identical(get("fpr_df", envir = .GlobalEnv), "unchanged")
  testthat::expect_equal(result$redd_df$location, rep("Spring Creek", 2))
  testthat::expect_null(result$rem_df)
  err_2023 <- dplyr::filter(result$sex_err, spawn_year == 2023)
  testthat::expect_equal(err_2023$n_tags[err_2023$sex == "M"], 2)
  testthat::expect_equal(err_2023$n_false[err_2023$sex == "M"], 1)
  testthat::expect_equal(err_2023$n_tags[err_2023$sex == "F"], 2)
  testthat::expect_equal(err_2023$n_false[err_2023$sex == "F"], 0)
  testthat::expect_equal(calls[[1]]$prior, c(1, 2))
  testthat::expect_equal(calls[[1]]$b_x, 2)
  testthat::expect_equal(calls[[1]]$false_x, 1)
  testthat::expect_equal(calls[[1]]$b_y, 2)
  testthat::expect_equal(calls[[1]]$false_y, 0)
  testthat::expect_identical(calls[[1]]$zero_error, "y")
  testthat::expect_identical(calls[[2]]$zero_error, c("x", "y"))
  for (year in years) {
    saved <- new.env(parent = emptyenv())
    load(file.path(folder, paste0("Methow_", year, ".rda")), envir = saved)
    testthat::expect_identical(sort(ls(saved)), sort(names(result)))
    testthat::expect_equal(saved$sex_err$spawn_year, rep(year, 2))
    testthat::expect_null(saved$rem_df)
  }

  basin <- "Wenatchee"
  result <- suppressMessages(sroem:::prep_uc_sthd_data(
    basin = basin, query_year = years, adjust_fpr = FALSE, assign_global = TRUE,
    removal_file_path = folder, save_rda = TRUE, save_by_year = FALSE,
    save_file_path = folder
  ))
  testthat::expect_equal(result$redd_df$location, rep("Above Tumwater", 2))
  testthat::expect_equal(result$sex_err$spawn_year, years)
  testthat::expect_equal(get("fpr_df", envir = .GlobalEnv), result$fpr_df)
  saved <- new.env(parent = emptyenv())
  load(file.path(folder, "Wenatchee_2023-2024.rda"), envir = saved)
  testthat::expect_equal(saved$sex_err$spawn_year, years)

  suppressMessages(sroem:::prep_uc_sthd_data(
    basin = basin, query_year = years, phos_data = "tags", adjust_fpr = FALSE,
    removal_file_path = folder, save_rda = TRUE, save_file_path = folder
  ))
  for (year in years) {
    saved <- new.env(parent = emptyenv())
    load(file.path(folder, paste0("Wenatchee_", year, ".rda")), envir = saved)
    testthat::expect_equal(saved$sex_err$spawn_year, year)
  }

  testthat::expect_error(
    suppressMessages(sroem:::prep_uc_sthd_data(
      basin = basin, query_year = 2023, phos_data = "tags",
      removal_file_path = folder, sex_zero_error = list("2023" = "x")
    )),
    "false calls were observed"
  )
  original_brood <- brood
  brood <- dplyr::bind_rows(
    brood,
    dplyr::filter(brood, spawn_year == 2023, recaptured_pit == "M2") |>
      dplyr::mutate(sex_final = "F")
  )
  testthat::expect_error(
    suppressMessages(sroem:::prep_uc_sthd_data(
      basin = basin, query_year = 2023, phos_data = "tags",
      removal_file_path = folder
    )),
    "Conflicting sex calls"
  )
  brood <- original_brood
  brood <- brood[0, ]
  have_redd <- FALSE
  testthat::expect_warning(
    empty <- suppressMessages(sroem:::prep_uc_sthd_data(
      basin = basin, query_year = 2023, phos_data = "tags",
      removal_file_path = folder, save_rda = TRUE, save_file_path = folder
    )),
    "determined entirely by the prior"
  )
  testthat::expect_null(empty$redd_df)
  testthat::expect_equal(nrow(empty$sex_err), 0)
  testthat::expect_true(is.finite(empty$fpr_df$fpr))
  saved <- new.env(parent = emptyenv())
  load(file.path(folder, "Wenatchee_2023.rda"), envir = saved)
  testthat::expect_null(saved$redd_df)
})

testthat::test_that("basin preparation rejects unavailable or unsafe configurations", {
  testthat::expect_error(sroem:::prep_uc_sthd_data("Entiat"), "not yet available")
  testthat::expect_error(sroem:::prep_uc_sthd_data("invalid"), "arg")
  testthat::expect_error(
    sroem:::prep_uc_sthd_data("Methow",
      query_year = 2023:2024, save_rda = TRUE, save_file_name = "data.rda"
    ),
    "multiple per-year"
  )
  testthat::expect_error(
    sroem:::prep_uc_sthd_data("Methow",
      query_year = 2023, sex_zero_error = list("2024" = "x")
    ),
    "named by requested years"
  )
})
