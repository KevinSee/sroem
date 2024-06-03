#' @title Prepare All Data from Wenatchee for a Report
#'
#' @description This function pulls together redd survey data and estimates net error for each survey, PIT tags detected in the Wenatchee, error rates of sex calls at Priest, a corrected fish/redd estimate based on sex ratios adjusted for the error rate of sex calls at Priest, estimates of escapement to various tributaries in the Wenatchee as well as run escapement to the Wenatchee as a whole and the number of known removals (e.g. broodstock collection, harvest). These are all either saved as an .RData object, or loaded into the global environment.
#'
#' @author Kevin See
#'
#' @inheritParams query_redd_data
#' @inheritParams query_dabom_results
#' @param brood_file_path file path to broodstock collection file
#' @param brood_file_name name of Excel file containing broodstock collection data in a very particular format
#' @param removal_file_path file path to removal data file
#' @param removal_file_name name of Excel file containing removal data in a very particular format
#' @param n_observers how many observers / boats were used on each survey?
#' @param phos_data should the data used to estimate pHOS come from PIT `tags` or `escapement` estimates? Default is `escapement`.
#' @param save_rda should the data that's returned by saved as an .RData object (`TRUE`)? Default value of `FALSE` loads all the returned objects into the Global Environment.
#' @param save_file_path if `save_rda` is `TRUE`, where should the .RData object be saved?
#' @param save_file_name if `save_rda` is `TRUE`, what should the file name be? Should end in ".rda".
#'
#'
#' @import rlang purrr dplyr readxl readr janitor tidyr lubridate coda
#' @importFrom DescTools BinomCI
#' @importFrom msm deltamethod
#' @return either saves an .Rdata object with output for a given year, or loads those results into global environment.
#' @export

prep_wen_sthd_data <- function(
    redd_file_path = "T:/DFW-Team FP Upper Columbia Escapement - General/UC_Sthd/inputs/Redd Data",
    redd_file_name = "Wenatchee_Redd_Surveys.xlsx",
    experience_path = redd_file_path,
    experience_file_name = redd_file_name,
    dabom_file_path = "O:Documents/Git/MyProjects/DabomPriestRapidsSthd/analysis/data/derived_data/estimates",
    dabom_file_name = "UC_Sthd_DABOM_",
    brood_file_path = "T:/DFW-Team FP Upper Columbia Escapement - General/UC_Sthd/inputs/Bio Data/Sex and Origin PRD-Brood Comparison Data",
    brood_file_name = "STHD UC Brood Collections_2011 to current.xlsx",
    removal_file_path = "T:/DFW-Team FP Upper Columbia Escapement - General/UC_Sthd/inputs/Fish Removals",
    removal_file_name = "Master_STHD_Removals_2.18.23.MH.xlsx",
    n_observers = "two",
    query_year = lubridate::year(lubridate::today()) - 1,
    phos_data = c("escapement",
                  "tags"),
    save_rda = F,
    save_by_year = T,
    save_file_path = here::here("analysis/data/derived_data"),
    save_file_name = NULL
) {

  message("\t Gathering redd data.\n")

  phos_data = match.arg(phos_data)

  # load data for selected years
  redd_df_all <- query_redd_data(redd_file_path,
                                 redd_file_name,
                                 experience_path,
                                 experience_file_name,
                                 query_year = query_year)

  if(!is.null(redd_df_all)) {
    # divide reaches into various location categories
    redd_df_all <-
      redd_df_all |>
      dplyr::mutate(location = dplyr::case_when(reach %in% paste0("W", 8:10) ~ "Above Tumwater",
                                                reach %in% paste0("W", 1:7) ~ "Below Tumwater",
                                                .default = "Tributaries"))

    # drop surveys that didn't actually happen (NAs for new redds)
    redd_df_all <-
      redd_df_all |>
      dplyr::filter(!is.na(new_redds))

    # predict net error
    redd_df_all <- redd_df_all |>
      sroem::predict_neterr(species = "Steelhead",
                            num_obs = n_observers)
  }

  #-----------------------------------------------------------------
  # load data on error calls for sex at Priest Rapids when fish were tagged

  message("\t Pulling PIT tag data.\n\n")

  dabom_df <- dplyr::tibble(spawn_year = query_year,
                            dam_nm = dplyr::if_else(spawn_year %in% c(2011:2015, 2018),
                                                    "PriestRapids",
                                                    "RockIsland"))

  # get info on tags detected somewhere in the Wenatchee
  all_tags <- dabom_df |>
    dplyr::mutate(tag_summ = purrr::map2(spawn_year,
                                         dam_nm,
                                         .f = function(yr, dam_nm) {
                                           sroem::query_dabom_results(dabom_file_path = dabom_file_path,
                                                                      dabom_dam_nm = dam_nm,
                                                                      dabom_file_name = dabom_file_name,
                                                                      query_year = yr,
                                                                      result_type = "tag_summ") |>
                                             dplyr::select(-dplyr::any_of("spawn_year"))
                                         })) |>
    dplyr::select(-dam_nm) |>
    tidyr::unnest(tag_summ)

  if("spawn_node" %in% names(all_tags)) {
    all_tags <-
      all_tags |>
      rename(final_node = spawn_node)
  }

  wen_tags_all <-
    all_tags |>
    dplyr::filter(stringr::str_detect(path, "LWE")) |>
    dplyr::mutate(location = dplyr::if_else(final_node %in% c('TUM', 'UWE'),
                                            'Above Tumwater',
                                            dplyr::if_else(stringr::str_detect(final_node, "^LWE"),
                                                           "Below Tumwater",
                                                           dplyr::if_else(stringr::str_detect(path, "CHL"),
                                                                          "Chiwawa",
                                                                          dplyr::if_else(stringr::str_detect(path, "NAL"),
                                                                                         "Nason",
                                                                                         dplyr::if_else(stringr::str_detect(path, "PES"),
                                                                                                        "Peshastin",
                                                                                                        "Other Tributaries"))))),
                  dplyr::across(location,
                                ~ factor(.,
                                         levels = c("Below Tumwater",
                                                    'Above Tumwater',
                                                    "Peshastin",
                                                    "Nason",
                                                    "Chiwawa",
                                                    'Other Tributaries')))) |>
    dplyr::select(spawn_year,
                  tag_code,
                  location,
                  origin,
                  sex)

  #-------------------------------------------------------
  # generate fish / redd and pHOS for different areas
  fpr_all = wen_tags_all |>
    dplyr::mutate(across(c(sex),
                         ~ dplyr::recode(.,
                                         "Male" = "M",
                                         "Female" = "F"))) |>
    dplyr::group_by(spawn_year,
                    location) |>
    dplyr::summarize(n_male = dplyr::n_distinct(tag_code[sex == "M"]),
                     n_female = dplyr::n_distinct(tag_code[sex == "F"]),
                     n_sexed = n_male + n_female,
                     n_wild = dplyr::n_distinct(tag_code[origin == "W"]),
                     n_hatch = dplyr::n_distinct(tag_code[origin == "H"]),
                     n_origin = n_wild + n_hatch,
                     .groups = "drop") |>
    dplyr::mutate(prop_m = n_male / n_sexed,
                  prop_se = sqrt((prop_m * (1 - prop_m)) / (n_sexed)),
                  fpr = (prop_m) / (1 - prop_m) + 1) |>
    dplyr::rowwise() |>
    dplyr::mutate(fpr_se = msm::deltamethod(~ x1 / (1 - x1) + 1,
                                            mean = prop_m,
                                            cov = prop_se^2)) |>
    dplyr::ungroup() |>
    dplyr::mutate(phos = n_hatch / n_origin,
                  phos_se = sqrt((phos * (1 - phos)) / (n_origin)))

  message("\t Adjusting fish/redd.\n")

  # adjust fish / redd for errors in Priest sex calls
  # the excel file contains rounded numbers, so re-calculate
  # various statistics for use in analyses
  # estimate error rate for each sex
  sex_err_rate <-
    all_tags |>
    dplyr::select(spawn_year,
                  tag_code,
                  sex_field = sex) |>
    dplyr::inner_join(readxl::read_excel(paste(brood_file_path,
                                               brood_file_name,
                                               sep = "/"),
                                         sheet = "Brood Collected_PIT Tagged Only") |>
                        janitor::clean_names() |>
                        dplyr::rename(tag_code = recaptured_pit) |>
                        dplyr::select(spawn_year,
                                      tag_code,
                                      sex_final) |>
                        dplyr::distinct(),
                      by = c("spawn_year",
                             "tag_code")) |>
    dplyr::filter(!is.na(sex_final),
                  !is.na(sex_field)) |>
    dplyr::mutate(across(c(sex_field,
                           sex_final),
                         ~ dplyr::recode(.,
                                         "Male" = "M",
                                         "Female" = "F"))) |>
    dplyr::mutate(agree = dplyr::if_else(sex_field == sex_final,
                                         T, F)) |>
    dplyr::group_by(spawn_year,
                    sex = sex_field) |>
    dplyr::summarize(n_tags = dplyr::n_distinct(tag_code),
                     n_true = sum(agree),
                     n_false = sum(!agree),
                     .groups = "drop") |>
    dplyr::mutate(binom_ci = purrr::map2(n_false,
                                         n_tags,
                                         .f = function(x, y) {
                                           DescTools::BinomCI(x, y) |>
                                             dplyr::as_tibble()
                                         })) |>
    tidyr::unnest(binom_ci) |>
    janitor::clean_names() |>
    dplyr::rename(perc_false = est,
                  lowerci = lwr_ci,
                  upperci = upr_ci) |>
    dplyr::mutate(perc_se = sqrt((perc_false * (1 - perc_false)) / n_tags)) |>
    dplyr::relocate(perc_se,
                    .after = "perc_false")


  adj_fpr <- fpr_all |>
    dplyr::select(spawn_year,
                  location,
                  n_male,
                  n_female) |>
    tidyr::pivot_longer(cols = c(n_male,
                                 n_female),
                        names_to = "sex",
                        values_to = "n_fish") |>
    dplyr::mutate(
      dplyr::across(sex,
                    ~ stringr::str_remove(.,
                                          "^n_")),
      dplyr::across(sex,
                    stringr::str_to_title)) |>
    dplyr::mutate(
      dplyr::across(sex,
                    ~ dplyr::recode(.,
                                    "Male" = "M",
                                    "Female" = "F"))) |>
    dplyr::left_join(sex_err_rate |>
                       dplyr::select(spawn_year,
                                     sex,
                                     dplyr::starts_with("perc_")),
                     by = c("spawn_year", "sex")) |>
    tidyr::pivot_wider(names_from = sex,
                       values_from = c(n_fish,
                                       perc_false,
                                       perc_se)) |>
    dplyr::mutate(true_male = n_fish_M - (n_fish_M * perc_false_M) + (n_fish_F * perc_false_F),
                  true_female = n_fish_F - (n_fish_F * perc_false_F) + (n_fish_M * perc_false_M),
                  dplyr::across(starts_with("true"),
                                janitor::round_half_up)) |>
    dplyr::rowwise() |>
    dplyr::mutate(true_m_se = msm::deltamethod(~ x1 - (x1 * x2) + (x3 * x4),
                                               mean = c(n_fish_M,
                                                        perc_false_M,
                                                        n_fish_F,
                                                        perc_false_F),
                                               cov = diag(c(0,
                                                            perc_se_M,
                                                            0,
                                                            perc_se_F)^2)),
                  true_f_se = msm::deltamethod(~ x1 - (x1 * x2) + (x3 * x4),
                                               mean = c(n_fish_F,
                                                        perc_false_F,
                                                        n_fish_M,
                                                        perc_false_M),
                                               cov = diag(c(0,
                                                            perc_se_F,
                                                            0,
                                                            perc_se_M)^2))) |>
    dplyr::mutate(n_sexed = true_male + true_female,
                  prop_m = true_male / (true_male + true_female),
                  prop_se = msm::deltamethod(~ x1 / (x1 + x2),
                                             mean = c(true_male,
                                                      true_female),
                                             cov = diag(c(true_m_se,
                                                          true_f_se)^2)),
                  fpr = (prop_m) / (1 - prop_m) + 1,
                  fpr_se = msm::deltamethod(~ x1 / (1 - x1) + 1,
                                            mean = prop_m,
                                            cov = prop_se^2)) |>
    dplyr::ungroup() |>
    dplyr::rename(n_male = true_male,
                  n_female = true_female) |>
    dplyr::left_join(fpr_all |>
                       dplyr::select(spawn_year,
                                     location,
                                     n_wild,
                                     n_hatch,
                                     n_origin,
                                     starts_with("phos")),
                     by = c("spawn_year", "location")) |>
    dplyr::select(dplyr::any_of(names(fpr_all)))

  # # look at changes to fish/redd
  # fpr_all |>
  #   select(spawn_year,
  #          location,
  #          old_fpr = fpr) |>
  #   left_join(adj_fpr |>
  #               select(spawn_year,
  #                      location,
  #                      adj_fpr = fpr))

  # if any fpr values are Inf, use the older ones
  if(sum(adj_fpr$fpr == Inf) > 0) {
    adj_fpr <- adj_fpr |>
      dplyr::left_join(fpr_all |>
                         dplyr::select(location,
                                       old_fpr = fpr,
                                       old_se = fpr_se)) |>
      dplyr::mutate(fpr = dplyr::if_else(is.na(fpr) | fpr == Inf,
                                         old_fpr,
                                         fpr),
                    fpr_se = dplyr::if_else(is.na(fpr_se) | fpr_se == Inf,
                                            old_se,
                                            fpr_se)) |>
      dplyr::select(-dplyr::starts_with("old"))
  }

  fpr_all <- adj_fpr

  rm(adj_fpr)

  #-----------------------------------------------------------------
  # read in data about known removals of fish prior to spawning
  if(file.exists(paste(removal_file_path,
                       removal_file_name,
                       sep = "/"))) {
    if(stringr::str_detect(removal_file_name, "csv$")) {
      removal_df <- readr::read_csv(paste(removal_file_path,
                                          removal_file_name,
                                          sep = "/")) |>
        janitor::clean_names() |>
        dplyr::filter(subbasin == "Wenatchee",
                      spawn_year %in% query_year)
    }
    if(stringr::str_detect(removal_file_name, "xls$") |
       stringr::str_detect(removal_file_name, "xlsx$")) {

      removal_df <- readxl::read_excel(paste(removal_file_path,
                                             removal_file_name,
                                             sep = "/"),
                                       skip = 3,
                                       col_names = c("run cycle",
                                                     "spawn year",
                                                     "population",
                                                     "removal location",
                                                     "agency",
                                                     "adult trapping surplus H",
                                                     "brood collections H",
                                                     "harvest H",
                                                     "adult trapping surplus W",
                                                     "brood collections W",
                                                     "harvest W",
                                                     "all removals H",
                                                     "all removals W",
                                                     "all removals T",
                                                     "notes")) |>
        janitor::clean_names() |>
        dplyr::mutate(
          across(
            c(ends_with("_h"),
              ends_with("_w"),
              ends_with("_t")),
            as.numeric
          )) |>
        dplyr::filter(!is.na(spawn_year)) |>
        dplyr::select(spawn_year:harvest_w) |>
        tidyr::pivot_longer(cols = c(ends_with("_h"),
                                     ends_with("_w")),
                            names_to = "source",
                            values_to = "removed") |>
        dplyr::mutate(origin = stringr::str_sub(source, -1)) |>
        dplyr::relocate(origin,
                        .before = "removed") |>
        dplyr::filter(origin %in% c("h", "w")) |>
        dplyr::mutate(
          dplyr::across(
            source,
            ~ stringr::str_remove(.,
                                  "_h$")),
          dplyr::across(
            source,
            ~ stringr::str_remove(.,
                                  "_w$")),
          dplyr::across(
            source,
            ~ stringr::str_to_title(stringr::str_replace_all(., "_", " "))
          ),
          dplyr::across(
            origin,
            ~ dplyr::recode(.,
                            "h" = "Hatchery",
                            "w" = "Natural")
          )
        ) |>
        dplyr::filter(spawn_year %in% query_year,
                      population == "Wenatchee")
    }

  } else {
    message("Removal data not found.\n")
    removal_df <- NULL
  }


  #-----------------------------------------------------------------
  # pull in some estimates from DABOM

  message("\t Gathering PIT escapement estimates.\n")

  all_escp <- dabom_df |>
    dplyr::mutate(escp = purrr::map2(spawn_year,
                                     dam_nm,
                                     .f = function(yr, dam_nm) {
                                       sroem::query_dabom_results(dabom_file_path = dabom_file_path,
                                                                  dabom_dam_nm = dam_nm,
                                                                  dabom_file_name = dabom_file_name,
                                                                  query_year = yr,
                                                                  result_type = "escape_summ")
                                     })) |>
    dplyr::select(-c(spawn_year,
                     dam_nm)) |>
    tidyr::unnest(escp) |>
    dplyr::filter(location %in% c('ICL',
                                  'PES',
                                  'MCL',
                                  'CHM',
                                  'CHW',
                                  'CHL',
                                  'NAL',
                                  'LWN',
                                  'WTL',
                                  'LWE',
                                  'LWE_bb',
                                  'TUM_bb',
                                  'UWE_bb')) |>
    dplyr::select(spawn_year,
                  origin,
                  location,
                  estimate = median,
                  se = sd,
                  lci = lower_ci,
                  uci = upper_ci)

  # pull out estimates of tributary spawners from DABOM
  trib_spawners_all = all_escp |>
    dplyr::filter(location %in% c('ICL',
                                  'PES',
                                  'MCL',
                                  'CHM',
                                  'CHW',
                                  'CHL',
                                  'NAL',
                                  'LWN',
                                  'WTL')) |>
    dplyr::select(spawn_year,
                  origin,
                  location,
                  spawners = estimate,
                  spawners_se = se,
                  lci,
                  uci) |>
    dplyr::mutate(
      dplyr::across(
        origin,
        ~ dplyr::recode(.,
                        "W" = "Natural",
                        "H" = "Hatchery")),
      dplyr::across(
        location,
        ~ dplyr::recode(.,
                        'CHL' = 'Chiwawa',
                        'CHM' = 'Chumstick',
                        'CHW' = 'Chiwaukum',
                        'ICL' = 'Icicle',
                        'LWN' = 'Little Wenatchee',
                        'MCL' = 'Mission',
                        'NAL' = 'Nason',
                        'PES' = 'Peshastin',
                        'WTL' = 'White River'))) |>
    dplyr::arrange(location, origin)

  # pull out mainstem escapement estimates
  # escp_wen_all = all_escp |>
  #   dplyr::filter(location %in% c('LWE',
  #                                 'LWE_bb',
  #                                 'TUM_bb',
  #                                 'UWE_bb')) |>
  #   dplyr::mutate(
  #     dplyr::across(
  #       location,
  #       ~ dplyr::recode(.,
  #                       'LWE' = 'Wen_all',
  #                       'LWE_bb' = 'Below Tumwater',
  #                       'TUM_bb' = "Above Tumwater",
  #                       'UWE_bb' = 'Above Tumwater'))) |>
  #   dplyr::mutate(
  #     dplyr::across(
  #       origin,
  #       ~ dplyr::recode(.,
  #                       "W" = "Natural",
  #                       "H" = "Hatchery"))) |>
  #   dplyr::group_by(spawn_year,
  #                   location,
  #                   origin) |>
  #   dplyr::summarise(
  #     dplyr::across(estimate,
  #                   sum),
  #     dplyr::across(se,
  #                   ~ sqrt(sum(.^2))),
  #     .groups = "drop")


  escp_wen_all <-
    dabom_df |>
    dplyr::mutate(post = purrr::map2(spawn_year,
                                     dam_nm,
                                     .f = function(yr, dam_nm) {
                                       sroem::query_dabom_results(dabom_file_path = dabom_file_path,
                                                                  dabom_dam_nm = dam_nm,
                                                                  dabom_file_name = dabom_file_name,
                                                                  query_year = yr,
                                                                  result_type = "escape_post")
                                     })) |>
    dplyr::select(-dam_nm) |>
    tidyr::unnest(post) |>
    dplyr::rename(location = param) |>
    dplyr::filter(location %in% c('LWE',
                                  'LWE_bb',
                                  'TUM_bb',
                                  'UWE_bb')) |>
    dplyr::mutate(
      dplyr::across(
        location,
        ~ dplyr::recode(.,
                        'LWE' = 'Wen_all',
                        'LWE_bb' = 'Below Tumwater',
                        'TUM_bb' = "Above Tumwater",
                        'UWE_bb' = 'Above Tumwater'))) |>
    dplyr::mutate(
      dplyr::across(
        origin,
        ~ dplyr::recode(.,
                        "W" = "Natural",
                        "H" = "Hatchery"))) |>
    dplyr::group_by(spawn_year,
                    location,
                    origin,
                    chain,
                    iter) |>
    dplyr::summarise(
      dplyr::across(abund,
                    sum),
      .groups = "drop") |>
    dplyr::group_by(spawn_year,
                    location,
                    origin) |>
    dplyr::summarize(estimate = median(abund),
                     se = sd(abund),
                     lci = coda::HPDinterval(coda::as.mcmc(abund))[,1],
                     uci = coda::HPDinterval(coda::as.mcmc(abund))[,2],
                     .groups = "drop")



  #-----------------------------------------------------------------
  # use escapement estimates rather than tags to estimate pHOS
  if(phos_data == "escapement") {
    escp_phos <-
      escp_wen_all |>
      dplyr::bind_rows(trib_spawners_all |>
                         dplyr::rename(estimate = spawners,
                                       se = spawners_se)) |>
      dplyr::select(spawn_year,
                    location,
                    origin,
                    estimate,
                    se) |>
      tidyr::pivot_wider(names_from = origin,
                         values_from = c(estimate, se)) |>
      dplyr::rowwise() |>
      dplyr::mutate(phos = estimate_Hatchery / (estimate_Hatchery + estimate_Natural),
                    phos_se = msm::deltamethod(~ x1 / (x1 + x2),
                                               mean = c(estimate_Hatchery,
                                                        estimate_Natural),
                                               cov = diag(c(se_Hatchery,
                                                            se_Natural)^2))) |>
      dplyr::ungroup()

    fpr_all <-
      fpr_all |>
      dplyr::left_join(escp_phos |>
                         dplyr::select(spawn_year,
                                       location,
                                       phos2 = phos,
                                       phos_se2 = phos_se),
                       by = dplyr::join_by(spawn_year, location)) |>
      dplyr::mutate(
        dplyr::across(
          phos,
          ~ dplyr::if_else(!is.na(phos2),
                           phos2,
                           .)),
        dplyr::across(
          phos_se,
          ~ dplyr::if_else(!is.na(phos_se2),
                           phos_se2,
                           .))
      ) |>
      dplyr::select(-c(phos2,
                       phos_se2))

  }

  #-----------------------------------------------------------------
  # save
  if(save_rda & save_by_year) {
    for(yr in query_year) {
      message(paste("Saving data from spawn year",
                    yr,
                    ".\n\n"))

      if(!is.null(redd_df_all)) {
        redd_df <- redd_df_all |>
          dplyr::filter(spawn_year == yr)
      } else {
        redd_df <- NULL
      }

      wen_tags <- wen_tags_all |>
        dplyr::filter(spawn_year == yr)

      sex_err <- sex_err_rate |>
        dplyr::filter(spawn_year == yr)

      fpr_df <- fpr_all |>
        dplyr::filter(spawn_year == yr)

      trib_spawners <- trib_spawners_all |>
        dplyr::filter(spawn_year == yr)

      escp_wen <- escp_wen_all |>
        dplyr::filter(spawn_year == yr)

      rem_df <- removal_df |>
        dplyr::filter(spawn_year == yr)

      if(is.null(save_file_name)) {
        file_nm = paste0('wen_', yr, '.rda')
      } else {
        file_nm = save_file_name
      }

      save(redd_df,
           wen_tags,
           sex_err,
           fpr_df,
           trib_spawners,
           escp_wen,
           rem_df,
           file = paste(save_file_path,
                        file_nm,
                        sep = "/"))
      rm(file_nm)

    }
  }
  else {
    if(!is.null(redd_df_all)) {
      redd_df <- redd_df_all
    } else {
      redd_df <- NULL
    }

    wen_tags <- wen_tags_all

    sex_err <- sex_err_rate

    fpr_df <- fpr_all

    trib_spawners <- trib_spawners_all

    escp_wen <- escp_wen_all

    rem_df <- removal_df


    if(save_rda & !save_by_year) {
      if(is.null(save_file_name)) {
        if(length(query_year) > 1) {
          save_file_name <- paste0('wen_',
                                   paste(min(query_year),
                                         max(query_year),
                                         sep = "-"),
                                   '.rda')
        } else {
          save_file_name <- paste0('wen_',
                                   query_year,
                                   '.rda')
        }
      }
      save(redd_df,
           wen_tags,
           sex_err,
           fpr_df,
           trib_spawners,
           escp_wen,
           rem_df,
           file = paste(save_file_path,
                        save_file_name,
                        sep = "/"))
    } else {

      tmp_file <- tempfile(fileext = ".rda")

      save(redd_df,
           wen_tags,
           sex_err,
           fpr_df,
           trib_spawners,
           escp_wen,
           rem_df,
           file = tmp_file)

      load(tmp_file,
           envir = .GlobalEnv)

      file.remove(tmp_file)
      rm(tmp_file)
    }
  }

}
