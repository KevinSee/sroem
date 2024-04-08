#' @title Query Redd Data
#'
#' @description Prepares redd survey data including relevant covariates for an observer error model by querying specific files on a WDFW Sharepoint site, and filters for selected year(s).
#'
#' @author Kevin See
#'
#' @param redd_file_path file path to redd data file
#' @param redd_file_name name of Excel file containing redd data in a very particular format, including sheets labeled "Reach Length", "Thalweg CV", "Discharge Gages" and "Redd Surveys".
#' @param experience_path file path to experience file
#' @param experience_file_name name of Excel file containing experience data in a very particular format, with a sheet labeled "Experience"
#' @param query_year which year or years should be included in this query?
#'
#' @import rlang purrr dplyr janitor lubridate readxl forcats stringr dataRetrieval
#' @return tibble
#' @export

query_redd_data <- function(
  redd_file_path = "T:/DFW-Team FP Upper Columbia Escapement - General/UC_Sthd/inputs/Redd Data",
  redd_file_name = "Wenatchee_Redd_Surveys.xlsx",
  experience_path = NULL,
  experience_file_name = NULL,
  query_year = lubridate::year(lubridate::today()) - 1) {

  data_file = paste(redd_file_path,
                    redd_file_name,
                    sep = "/")

  if(!file.exists(data_file)) {
    stop("File not found.")
  }

  if(is.null(experience_path)) {
    experience_path <- redd_file_path
  }

  if(is.null(experience_file_name)) {
    experience_file_name <- redd_file_name
  }

  # compile all data
  data_list <- readxl::excel_sheets(data_file) |>
    as.list() |>
    rlang::set_names() |>
    purrr::map(.f = purrr::quietly(function(x) {
      readxl::read_excel(data_file,
                         sheet = x) |>
        janitor::clean_names()
    })) |>
    purrr::map("result")


  redd_surv_df <-
    data_list$`Redd Surveys` |>
    dplyr::mutate(
      across(
        c(survey_type,
          river),
        stringr::str_to_title
      )
    ) |>
    dplyr::mutate(
      dplyr::across(
        c(new_redds,
          visible_redds),
        as.numeric))


  # slightly different format for experience tables between Wenatchee, Methow and Entiat
  if(stringr::str_detect(redd_file_name, "Wenatchee")) {

    # get experience data
    exp_df <- suppressMessages(readxl::read_excel(paste(experience_path,
                                                        experience_file_name,
                                                        sep = "/"),
                                                  sheet = "Experience",
                                                  skip = 1)) |>
      dplyr::rename(basin = `...1`,
                    agency = `...2`,
                    surveyor_name = `...3`,
                    surveyor_initials = `...4`) |>
      tidyr::pivot_longer(-c(basin:surveyor_initials),
                          names_to = "spawn_year",
                          values_to = "experience") |>
      dplyr::mutate(
        dplyr::across(
          spawn_year,
          as.numeric
        )
      )
  } else if(stringr::str_detect(redd_file_name, "Methow")) {
    # get experience data
    exp_df <- suppressMessages(readxl::read_excel(paste(experience_path,
                                                        experience_file_name,
                                                        sep = "/"),
                                                  sheet = "Experience",
                                                  skip = 1)) |>
      dplyr::rename(basin = `...1`,
                    # agency = `...2`,
                    surveyor_name = `...2`,
                    surveyor_initials = `...3`,
                    notes = `...14`) |>
      dplyr::select(-notes) |>
      tidyr::pivot_longer(-c(basin:surveyor_initials),
                          names_to = "spawn_year",
                          values_to = "experience") |>
      dplyr::mutate(
        dplyr::across(
          spawn_year,
          as.numeric
        )
      )
  } else if(stringr::str_detect(redd_file_name, "Entiat")) {
    # get experience data
    exp_df <- suppressMessages(readxl::read_excel(paste(experience_path,
                                                        experience_file_name,
                                                        sep = "/"),
                                                  sheet = "Experience",
                                                  skip = 1)) |>
      dplyr::rename(basin = `...1`,
                    # agency = `...2`,
                    surveyor_name = `...2`,
                    surveyor_initials = `...3`) |>
      tidyr::pivot_longer(-c(basin:surveyor_initials),
                          names_to = "spawn_year",
                          values_to = "experience") |>
      dplyr::mutate(
        dplyr::across(
          spawn_year,
          as.numeric
        )
      )
  } else {
    exp_df <- tibble(basin = NA_character_,
                     surveyor_name = NA_character_,
                     surveyor_initials = NA_character_,
                     spawn_year = NA_real_,
                     experience = NA_real_)
  }

    redd_surv_df <- redd_surv_df |>
      dplyr::mutate(
        dplyr::across(
          c(surveyor1,
            surveyor2),
          ~ stringr::str_remove(.,
                                "\\ \\([:alpha:]+\\)"))) |>
      dplyr::left_join(exp_df |>
                         dplyr::select(spawn_year,
                                       surveyor1 = surveyor_initials,
                                       exp1 = experience),
                       by = c("spawn_year", "surveyor1")) |>
      dplyr::left_join(exp_df |>
                         dplyr::select(spawn_year,
                                       surveyor2 = surveyor_initials,
                                       exp2 = experience),
                       by = c("spawn_year", "surveyor2")) |>
      dplyr::rowwise() |>
      dplyr::mutate(exp_sp_total = mean(c(exp1, exp2), na.rm = T)) |>
      dplyr::ungroup()

  redd_df <- redd_surv_df |>
    dplyr::left_join(data_list$`Reach Length` |>
                       dplyr::group_by(river,
                                     reach,
                                     type, index) |>
                       dplyr::summarize(
                         dplyr::across(length_km,
                                       ~ sum(.)),
                         .groups = "drop"),
                     by = c("river", "reach", "index")) |>
    dplyr::left_join(data_list$`Thalweg CV` |>
                       dplyr::select(river,
                                     reach,
                                     mean_thalweg_cv),
                     by = c("river", "reach")) |>
    dplyr::left_join(data_list$`Discharge Gages` |>
                       dplyr::select(reach,
                                     usgs_site_code = site_code),
                     by = "reach") |>
    # left_join(data_list$Discharge,
    #           by = c("spawn_year", "river", "reach", "index", "survey_type", "survey_date")) |>
    dplyr::mutate(
      dplyr::across(
        c(reach,
          river),
        as.factor),
      dplyr::across(
        reach,
        ~ forcats::fct_relevel(.,
                               "W10",
                               "C1", "N1", "P1",
                               after = Inf)),
      dplyr::across(
        reach,
        ~ forcats::fct_relevel(.,
                               "MH1", "T1", "WN1",
                               after = Inf))) |>
    dplyr::filter(spawn_year %in% query_year) |>
    # calculate redd density and log of experience
    dplyr::mutate(naive_density_km = visible_redds / length_km,
                  exp_sp_total_log = log(exp_sp_total))

  if(nrow(redd_df) == 0) {
    message(paste("No redd data found for",
                  paste(query_year, collapse = ", "),
                  ".\n"))
    return(NULL)
  }

  message("\t Querying USGS for discharge data\n")

  # query USGS for mean daily discharge data
  discharge_df <- redd_df |>
    dplyr::filter(!is.na(usgs_site_code)) |>
    # dplyr::select(river,
    #               reach,
    #               survey_date,
    #               usgs_site_code) |>
    # slice(1:15) |>
    dplyr::mutate(disch_data = purrr::map2(usgs_site_code,
                                           survey_date,
                                           function(x, y) {
                                             if(nchar(x) == 8) {
                                               z <-
                                                 try(dataRetrieval::readNWISdv(x,
                                                                               parameterCd = "00060", # discharge
                                                                               startDate = as.character(lubridate::ymd(y)),
                                                                               endDate = as.character(lubridate::ymd(y)),
                                                                               statCd = "00003") |>  # mean
                                                       dplyr::rename(mean_discharge = X_00060_00003) |>
                                                       # dplyr::select(-c(agency_cd:Date, X_00060_00003_cd))
                                                       dplyr::select(mean_discharge)
                                                 )
                                             } else {
                                               doe_file_nm <- paste0("https://apps.ecology.wa.gov/ContinuousFlowAndWQ/StationData/Prod/",
                                                                     x,
                                                                     "/",
                                                                     x,
                                                                     "_",
                                                                     year(y),
                                                                     "_DSG_DV.txt")

                                               z <-
                                                 try(read_delim(doe_file_nm,
                                                                skip = 12,
                                                                delim = " ",
                                                                show_col_types = F,
                                                                col_names = F) |>
                                                       dplyr::rename(date = X1,
                                                                     mean_discharge = X16) |>
                                                       dplyr::mutate(
                                                         dplyr::across(mean_discharge,
                                                                       ~ if_else(is.na(.),
                                                                                 X15,
                                                                                 .))
                                                       ) |>
                                                       dplyr::mutate(
                                                         dplyr::across(date,
                                                                       lubridate::mdy),
                                                         dplyr::across(mean_discharge,
                                                                       as.numeric)) |>
                                                       dplyr::filter(date == y) |>
                                                       dplyr::select(mean_discharge))
                                             }

                                             if(inherits(z, "try-error")) {
                                               z <- NULL
                                             }
                                             return(z)
                                           },
                                           .progress = T)) |>
    unnest(disch_data)

  # adjust discharge for W10: Plain - Chiwawa discharge
  if("W10" %in% unique(redd_df$reach)) {
    plain_code <- redd_df |>
      dplyr::filter(reach == "W9") |>
      dplyr::pull(usgs_site_code) |>
      unique()

    w10_discharge <- discharge_df |>
      dplyr::filter(reach == "W10") |>
      dplyr::rowwise() |>
      dplyr::mutate(
        dataRetrieval::readNWISdv(plain_code,
                                  parameterCd = "00060", # discharge
                                  startDate = as.character(lubridate::ymd(survey_date)),
                                  endDate = as.character(lubridate::ymd(survey_date)),
                                  statCd = "00003" # mean
        )) |>
      dplyr::ungroup() |>
      dplyr::rename(plain_discharge = X_00060_00003) |>
      dplyr::select(-c(agency_cd:Date, X_00060_00003_cd)) |>
      dplyr::mutate(mean_discharge = plain_discharge - mean_discharge) |>
      dplyr::select(-plain_discharge)

    # put it all back together
    discharge_df |>
      dplyr::filter(reach != "W10") |>
      dplyr::bind_rows(w10_discharge) -> discharge_df

  }

  # adjust discharge for Entiat reach B: Entiat River Near Entiat - DOE 46B060 Roaring Cr. Nr mouth
  if("Entiat_B" %in% unique(unite(redd_df, rch, river, reach)$rch)) {
    roar_crk_code <- "46B060"

    roar_crk_disch <-
      discharge_df |>
      dplyr::filter(river == "Entiat",
                    reach == "B") |>
      select(spawn_year) |>
      unique() |>
      dplyr::mutate(rc_disc = purrr::map(spawn_year,
                                         .f = function(yr) {
                                           doe_file_nm <-  paste0("https://apps.ecology.wa.gov/ContinuousFlowAndWQ/StationData/Prod/",
                                                                  roar_crk_code,
                                                                  "/",
                                                                  roar_crk_code,
                                                                  "_",
                                                                  yr,
                                                                  "_DSG_DV.txt")
                                           z <-
                                             try(read_delim(doe_file_nm,
                                                            skip = 12,
                                                            delim = " ",
                                                            show_col_types = F,
                                                            col_names = F) |>
                                                   dplyr::rename(date = X1,
                                                                 mean_discharge = X16) |>
                                                   dplyr::mutate(
                                                     dplyr::across(mean_discharge,
                                                                   ~ if_else(is.na(.),
                                                                             X15,
                                                                             .))
                                                   ) |>
                                                   dplyr::mutate(
                                                     dplyr::across(date,
                                                                   lubridate::mdy),
                                                     dplyr::across(mean_discharge,
                                                                   as.numeric)) |>
                                                   dplyr::select(date,
                                                                 rc_discharge = mean_discharge))

                                           if(inherits(z, "try-error")) {
                                             z <- NULL
                                           }
                                           return(z)
                                         })) |>
      unnest(rc_disc)

    # any missing values?
    miss_disch <-
      roar_crk_disch |>
      filter(date %in% c(discharge_df |>
               dplyr::filter(river == "Entiat",
                             reach == "B") |>
               pull(survey_date)),
             is.na(rc_discharge)) |>
      add_column(prev_disch = NA_real_,
                 next_disch = NA_real_)

    if(nrow(miss_disch) > 0) {
      for(i in 1:nrow(miss_disch)) {
        miss_disch$prev_disch[i] <-
          roar_crk_disch |>
          filter(date < miss_disch$date[i]) |>
          filter(!is.na(rc_discharge)) |>
          filter(date == max(date)) |>
          pull(rc_discharge)

        miss_disch$next_disch[i] <-
          roar_crk_disch |>
          filter(date > miss_disch$date[i]) |>
          filter(!is.na(rc_discharge)) |>
          filter(date == min(date)) |>
          pull(rc_discharge)
      }
      miss_disch <-
        miss_disch |>
        rowwise() |>
        mutate(rc_discharge = mean(prev_disch,
                                   next_disch)) |>
        ungroup()

      roar_crk_disch |>
        anti_join(miss_disch,
                  by = join_by(date)) |>
        bind_rows(miss_disch |>
                    select(all_of(names(roar_crk_disch)))) |>
        arrange(date) -> roar_crk_disch
    }

    # calculate appropriate discharge for Entiat B reach
    entB_discharge <-
      discharge_df |>
      dplyr::filter(river == "Entiat",
                    reach == "B") |>
      dplyr::left_join(roar_crk_disch,
                by = dplyr::join_by(spawn_year,
                             survey_date == date)) |>
      dplyr::mutate(
        dplyr::across(mean_discharge,
                      ~ . - rc_discharge)) |>
      dplyr::select(-rc_discharge)

    # put it all back together
    discharge_df |>
      dplyr::filter(!(river == "Entiat" &
                        reach == "B")) |>
      dplyr::bind_rows(entB_discharge) -> discharge_df

  }



  redd_data <- discharge_df |>
    dplyr::bind_rows(redd_df |>
                       dplyr::filter(is.na(usgs_site_code))) |>
    dplyr::arrange(spawn_year,
                   river,
                   reach,
                   index,
                   survey_date)

  # identical(nrow(redd_df),
  #           nrow(redd_data))


  return(redd_data)

}
