.sroem_read_usgs_daily <- function(site, date) {
  result <- dataRetrieval::read_waterdata_daily(
    monitoring_location_id = paste0("USGS-", site),
    parameter_code = "00060",
    statistic_id = "00003",
    time = c(date, date)
  )
  if (inherits(result, "sf")) {
    result <- sf::st_drop_geometry(result)
  }
  if (!"value" %in% names(result) || !is.numeric(result$value)) {
    stop("USGS response does not contain a numeric value column.", call. = FALSE)
  }
  values <- unique(result$value[!is.na(result$value)])
  if (length(values) > 1L || any(!is.finite(values))) {
    stop("USGS response contains ambiguous or non-finite discharge values.",
      call. = FALSE
    )
  }
  if (length(values) == 0L) NA_real_ else values[[1]]
}

.sroem_read_ecology_year <- function(site, year) {
  url <- paste0(
    "https://apps.ecology.wa.gov/ContinuousFlowAndWQ/StationData/Prod/",
    site, "/", site, "_", year, "_DSG_DV.txt"
  )
  result <- readr::read_delim(
    url,
    skip = 12, delim = " ", show_col_types = FALSE, col_names = FALSE
  )
  if (!all(c("X1", "X15", "X16") %in% names(result))) {
    stop("Ecology response does not contain the expected columns.", call. = FALSE)
  }
  result <- result |>
    dplyr::transmute(
      date = lubridate::mdy(.data$X1),
      value = as.numeric(dplyr::coalesce(.data$X16, .data$X15))
    )
  if (anyNA(result$date) || anyDuplicated(result$date) ||
    any(is.infinite(result$value))) {
    stop("Ecology response contains invalid or duplicate dates or values.",
      call. = FALSE
    )
  }
  result
}

#' Query redd surveys and discharge data
#'
#' Reads the specified survey workbook, adds covariates, and queries mean daily
#' discharge. USGS requests are made once per site and date; Washington
#' Department of Ecology files are read once per site and calendar year.
#'
#' @param redd_file_path Directory containing the survey workbook.
#' @param redd_file_name Workbook containing sheets `"Reach Length"`,
#'   `"Thalweg CV"`, `"Discharge Gages"`, and `"Redd Surveys"`.
#' @param experience_path,experience_file_name Directory and workbook containing
#'   the `"Experience"` sheet. By default these are the survey workbook.
#' @param query_year Spawn years to include.
#'
#' @details W10 discharge is Plain minus Chiwawa; Entiat B discharge is the
#'   Entiat gage minus Roaring Creek. Missing discharge, including a missing
#'   component of either adjustment, stays missing without interpolation.
#'   Request errors generate warnings, and a message lists affected reaches
#'   and survey dates. Surveys are not removed because discharge is unavailable.
#'
#' @return A tibble of survey rows and covariates, including `mean_discharge`,
#'   or `NULL` with a warning or message when the file or requested years are
#'   unavailable.
#' @export
query_redd_data <- function(
  redd_file_path = "T:/DFW-Team FP Upper Columbia Escapement - General/UC_Sthd/inputs/Redd Data",
  redd_file_name = "Wenatchee_Redd_Surveys.xlsx",
  experience_path = NULL,
  experience_file_name = NULL,
  query_year = lubridate::year(lubridate::today()) - 1
) {
  data_file <- file.path(redd_file_path, redd_file_name)
  if (!file.exists(data_file)) {
    warning("Redd data file not found: ", data_file, call. = FALSE)
    return(NULL)
  }
  for (pkg in c("readxl", "forcats", "dataRetrieval", "readr", "sf")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("The package ", pkg, " is required for this function.", call. = FALSE)
    }
  }
  if (is.null(experience_path)) experience_path <- redd_file_path
  if (is.null(experience_file_name)) experience_file_name <- redd_file_name

  sheets <- c("Reach Length", "Thalweg CV", "Discharge Gages", "Redd Surveys")
  missing_sheets <- setdiff(sheets, readxl::excel_sheets(data_file))
  if (length(missing_sheets) > 0) {
    stop("Missing workbook sheets: ", paste(missing_sheets, collapse = ", "),
      call. = FALSE
    )
  }
  data_list <- stats::setNames(lapply(sheets, function(sheet) {
    data <- readxl::read_excel(data_file, sheet = sheet) |>
      janitor::clean_names()
    if ("river" %in% names(data)) {
      data <- data |>
        dplyr::mutate(river = dplyr::case_match(
          .data$river, "Methow" ~ "Lower Methow",
          .default = .data$river
        ))
    }
    data
  }), sheets)

  redd_surv <- data_list[["Redd Surveys"]] |>
    dplyr::mutate(
      dplyr::across(c(survey_type, river), stringr::str_to_title),
      dplyr::across(c(new_redds, visible_redds), as.numeric),
      surveyor1 = dplyr::case_when(
        is.na(surveyor1) & !is.na(surveyors) ~ stringr::str_split_i(surveyors, ",", 1),
        .default = surveyor1
      ),
      surveyor2 = dplyr::case_when(
        is.na(surveyor2) & !is.na(surveyors) &
          stringr::str_detect(surveyors, ",") ~ stringr::str_split_i(surveyors, ",", 2),
        .default = surveyor2
      ),
      dplyr::across(
        c(surveyor1, surveyor2),
        ~ stringr::str_trim(stringr::str_split_i(.x, "\\(", 1))
      )
    )

  experience_file <- file.path(experience_path, experience_file_name)
  if (stringr::str_detect(redd_file_name, "Wenatchee|Methow")) {
    exp_df <- readxl::read_excel(experience_file, sheet = "Experience", skip = 1) |>
      dplyr::rename(
        basin = `...1`, agency = `...2`, surveyor_name = `...3`,
        surveyor_initials = `...4`
      ) |>
      tidyr::pivot_longer(
        -c(basin:surveyor_initials),
        names_to = "spawn_year",
        values_to = "experience"
      ) |>
      dplyr::mutate(spawn_year = as.numeric(spawn_year))
  } else if (stringr::str_detect(redd_file_name, "Entiat")) {
    exp_df <- readxl::read_excel(experience_file, sheet = "Experience", skip = 1) |>
      dplyr::rename(
        basin = `...1`, surveyor_name = `...2`, surveyor_initials = `...3`
      ) |>
      tidyr::pivot_longer(
        -c(basin:surveyor_initials),
        names_to = "spawn_year",
        values_to = "experience"
      ) |>
      dplyr::mutate(spawn_year = as.numeric(spawn_year))
  } else {
    exp_df <- tibble::tibble(
      spawn_year = numeric(), surveyor_initials = character(), experience = numeric()
    )
  }
  redd_surv <- redd_surv |>
    dplyr::left_join(
      dplyr::select(exp_df, spawn_year, surveyor1 = surveyor_initials, exp1 = experience),
      by = c("spawn_year", "surveyor1")
    ) |>
    dplyr::left_join(
      dplyr::select(exp_df, spawn_year, surveyor2 = surveyor_initials, exp2 = experience),
      by = c("spawn_year", "surveyor2")
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(exp_sp_total = mean(c(exp1, exp2), na.rm = TRUE)) |>
    dplyr::ungroup()

  gages <- data_list[["Discharge Gages"]] |>
    dplyr::transmute(reach, usgs_site_code = trimws(as.character(site_code)))
  redd_df <- redd_surv |>
    dplyr::left_join(
      data_list[["Reach Length"]] |>
        dplyr::group_by(river, reach, type, index) |>
        dplyr::summarise(length_km = sum(length_km), .groups = "drop"),
      by = c("river", "reach", "index")
    ) |>
    dplyr::left_join(
      dplyr::select(data_list[["Thalweg CV"]], river, reach, mean_thalweg_cv),
      by = c("river", "reach")
    ) |>
    dplyr::left_join(gages, by = "reach") |>
    dplyr::mutate(
      dplyr::across(c(reach, river), as.factor),
      reach = forcats::fct_relevel(reach, "W10", "C1", "N1", "P1", after = Inf),
      reach = forcats::fct_relevel(reach, "MH1", "T1", "WN1", after = Inf)
    ) |>
    dplyr::filter(spawn_year %in% query_year) |>
    dplyr::mutate(
      naive_density_km = visible_redds / length_km,
      exp_sp_total_log = log(exp_sp_total)
    )
  if (nrow(redd_df) == 0L) {
    message("No redd data found for ", paste(query_year, collapse = ", "), ".")
    return(NULL)
  }

  message("Querying discharge data.")
  dates <- if (all(is.na(redd_df$survey_date))) {
    rep(as.Date(NA), nrow(redd_df))
  } else {
    as.Date(redd_df$survey_date)
  }
  sites <- as.character(redd_df$usgs_site_code)
  sites[!is.na(sites) & !nzchar(trimws(sites))] <- NA_character_
  w10 <- !is.na(redd_df$reach) & as.character(redd_df$reach) == "W10"
  entiat_b <- !is.na(redd_df$river) & !is.na(redd_df$reach) &
    as.character(redd_df$river) == "Entiat" & as.character(redd_df$reach) == "B"
  plain_code <- NA_character_
  if (any(w10)) {
    plain_codes <- unique(gages$usgs_site_code[!is.na(gages$reach) &
      as.character(gages$reach) == "W9" & !is.na(gages$usgs_site_code) &
      nzchar(trimws(gages$usgs_site_code))])
    if (length(plain_codes) == 1L) {
      plain_code <- plain_codes[[1]]
    } else {
      warning("Cannot identify a unique W9 Plain gage for the W10 adjustment.",
        call. = FALSE
      )
    }
  }

  requests <- dplyr::bind_rows(
    tibble::tibble(site = sites, date = dates),
    tibble::tibble(site = rep(plain_code, sum(w10)), date = dates[w10]),
    tibble::tibble(site = rep("46B060", sum(entiat_b)), date = dates[entiat_b])
  ) |>
    dplyr::filter(!is.na(site), !is.na(date)) |>
    dplyr::distinct()
  requests$value <- NA_real_
  usgs <- which(nchar(requests$site) == 8L)
  for (i in usgs) {
    requests$value[i] <- tryCatch(
      .sroem_read_usgs_daily(requests$site[i], requests$date[i]),
      error = function(e) {
        warning("USGS discharge request failed for ", requests$site[i], " on ",
          requests$date[i], ": ", conditionMessage(e),
          call. = FALSE
        )
        NA_real_
      }
    )
  }

  ecology <- requests |>
    dplyr::filter(nchar(site) != 8L) |>
    dplyr::mutate(year = lubridate::year(date)) |>
    dplyr::distinct(site, year)
  for (i in seq_len(nrow(ecology))) {
    site <- ecology$site[i]
    year <- ecology$year[i]
    annual <- tryCatch(
      .sroem_read_ecology_year(site, year),
      error = function(e) {
        warning("Ecology discharge request failed for ", site, " in ", year,
          ": ", conditionMessage(e),
          call. = FALSE
        )
        NULL
      }
    )
    if (!is.null(annual)) {
      indices <- which(requests$site == site & lubridate::year(requests$date) == year)
      requests$value[indices] <- annual$value[match(requests$date[indices], annual$date)]
    }
  }

  lookup <- function(site, date) {
    keys <- tibble::tibble(site = site, date = date)
    dplyr::left_join(keys, requests, by = c("site", "date"))$value
  }
  discharge <- lookup(sites, dates)
  if (any(w10)) {
    discharge[w10] <- lookup(rep(plain_code, sum(w10)), dates[w10]) - discharge[w10]
  }
  if (any(entiat_b)) {
    discharge[entiat_b] <- discharge[entiat_b] -
      lookup(rep("46B060", sum(entiat_b)), dates[entiat_b])
  }
  redd_df$mean_discharge <- discharge
  missing_discharge <- redd_df |>
    dplyr::filter(is.na(mean_discharge)) |>
    dplyr::distinct(river, reach, survey_date)
  if (nrow(missing_discharge) > 0L) {
    missing_dates <- if (all(is.na(missing_discharge$survey_date))) {
      rep(NA_character_, nrow(missing_discharge))
    } else {
      as.character(as.Date(missing_discharge$survey_date))
    }
    details <- paste0(
      "river=", as.character(missing_discharge$river),
      ", reach=", as.character(missing_discharge$reach),
      ", date=", missing_dates
    )
    message("Missing discharge for: ", paste(details, collapse = "; "), ".")
  }

  redd_df |>
    dplyr::arrange(spawn_year, river, reach, index, survey_date)
}
