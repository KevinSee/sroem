#' @title Prepare All Data from an Upper Columbia Basin
#'
#' @description Combines redd surveys, observer-error predictions, PIT-tag
#'   information, sex-call error, fish per redd, escapement, and known removals.
#'   This unified workflow supersedes the legacy basin-specific preparation
#'   functions. Entiat is not yet implemented and produces an error.
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
#' @param adjust_fpr should sex data from broodstock collections be used to assess error rate of sex calls at Priest, and then adjust fish / redd values based on those error rates. Default value is `TRUE`.
#' @param basin Basin to prepare: `"Wenatchee"` or `"Methow"`. `"Entiat"`
#'   is reserved for a future implementation.
#' @param sex_error_prior Positive beta prior shape parameters passed to
#'   [boot_adj_prop()] for sex-call error, defaulting to Jeffreys' prior.
#' @param sex_zero_error Named list indexed by spawn year. Each entry is
#'   `"x"`, `"y"`, or `c("x", "y")`, fixing the true male (x) and/or
#'   true female (y) error rate at zero without uncertainty. Use only when
#'   external evidence supports an exact zero, not just for small samples.
#' @param assign_global If `TRUE`, also assign the returned objects to the
#'   global environment, replacing objects with the same names. Default `FALSE`.
#' @param save_rda If `TRUE`, also save the output objects in R data files.
#' @param save_by_year If saving, save a separate file for each year rather
#'   than a single combined file. Defaults to `TRUE`.
#' @param save_file_path if `save_rda` is `TRUE`, where should the .RData object be saved?
#' @param save_file_name Optional file name. A custom name may not be used
#'   with multiple years and `save_by_year = TRUE`, to prevent overwrites.
#'
#' @details Sex-error sample sizes and false calls are grouped by known true
#'   sex. `sex_err` reports the observed proportions and binomial confidence
#'   intervals; the fish-per-redd adjustment instead uses the beta posterior
#'   or the requested fixed-zero rates. Years without known-sex observations
#'   rely on the prior unless a rate is explicitly fixed at zero.
#'
#'
#' @import purrr dplyr readxl readr janitor tidyr lubridate coda
#' @importFrom rlang .data
#' @importFrom DescTools BinomCI
#' @importFrom msm deltamethod
#' @export
#' @return A named list with `redd_df`, `basin_tags`, `sex_err`, `fpr_df`,
#'   `trib_spawners`, `escp_est`, and `rem_df`. Redd or removal data may be
#'   `NULL` when unavailable. The list includes all requested years whether or
#'   not files are saved or objects are assigned globally.

prep_uc_sthd_data <- function(
  basin = c(
    "Wenatchee",
    "Methow",
    "Entiat"
  ),
  redd_file_path = "T:/DFW-Team FP Upper Columbia Escapement - General/UC_Sthd/inputs/Redd Data",
  redd_file_name = NULL,
  experience_path = NULL,
  experience_file_name = NULL,
  dabom_file_path = file.path(
    "C:",
    "Users",
    "seek1477",
    "Documents",
    "Git",
    "MyProjects",
    "DabomPriestRapidsSthd",
    "analysis",
    "data",
    "derived_data",
    "estimates"
  ),
  dabom_file_name = "UC_Sthd_DABOM_",
  brood_file_path = "T:/DFW-Team FP Upper Columbia Escapement - General/UC_Sthd/inputs/Bio Data/Sex and Origin PRD-Brood Comparison Data",
  brood_file_name = "STHD UC Brood Collections_2011 to current.xlsx",
  removal_file_path = "T:/DFW-Team FP Upper Columbia Escapement - General/UC_Sthd/inputs/Fish Removals",
  removal_file_name = "Master_STHD_Removals.xlsx",
  n_observers = "two",
  query_year = lubridate::year(lubridate::today()) - 1,
  phos_data = c(
    "escapement",
    "tags"
  ),
  adjust_fpr = TRUE,
  save_rda = FALSE,
  save_by_year = TRUE,
  save_file_path = here::here("analysis/data/derived_data"),
  save_file_name = NULL,
  sex_error_prior = c(0.5, 0.5),
  sex_zero_error = list(),
  assign_global = FALSE
) {
  basin <- match.arg(basin)
  phos_data <- match.arg(phos_data)

  if (basin == "Entiat") {
    stop("Entiat preparation is not yet available: paths, locations, and hatchery-mark categories are still being compiled.", call. = FALSE)
  }
  if (!is.numeric(query_year) || !length(query_year) ||
    anyNA(query_year) || any(!is.finite(query_year)) ||
    any(query_year != floor(query_year)) || anyDuplicated(query_year)) {
    stop("query_year must contain distinct, finite whole years.", call. = FALSE)
  }
  for (flag in c("adjust_fpr", "save_rda", "save_by_year", "assign_global")) {
    value <- get(flag)
    if (!is.logical(value) || length(value) != 1L || is.na(value)) {
      stop(flag, " must be TRUE or FALSE.", call. = FALSE)
    }
  }
  if (!is.numeric(sex_error_prior) || length(sex_error_prior) != 2L ||
    any(!is.finite(sex_error_prior)) || any(sex_error_prior <= 0)) {
    stop("sex_error_prior must contain two positive finite values.", call. = FALSE)
  }
  if (!is.list(sex_zero_error) ||
    (length(sex_zero_error) > 0L &&
      (is.null(names(sex_zero_error)) || anyNA(names(sex_zero_error)) ||
        any(!nzchar(names(sex_zero_error))) || anyDuplicated(names(sex_zero_error)) ||
        any(!names(sex_zero_error) %in% as.character(query_year)))) ||
    any(!vapply(sex_zero_error, function(x) {
      is.character(x) && !anyNA(x) && all(x %in% c("x", "y"))
    }, logical(1)))) {
    stop("sex_zero_error must be a list named by requested years, with values containing 'x' and/or 'y'.", call. = FALSE)
  }
  if (save_rda && save_by_year && length(query_year) > 1L &&
    !is.null(save_file_name)) {
    stop("A custom save_file_name cannot be used with multiple per-year files.", call. = FALSE)
  }

  if (is.null(redd_file_name)) {
    redd_file_name <- dplyr::case_when(
      basin == "Wenatchee" ~ "Wenatchee_Redd_Surveys.xlsx",
      basin == "Methow" ~ "Methow_Redd_Surveys.xlsx"
    )
  }

  if (is.null(experience_path)) {
    experience_path <- redd_file_path
  }

  if (is.null(experience_file_name)) {
    experience_file_name <- redd_file_name
  }


  message("\t Gathering redd data.\n")

  # load data for selected years
  redd_df_all <- query_redd_data(redd_file_path,
    redd_file_name,
    experience_path,
    experience_file_name,
    query_year = query_year
  )

  if (!is.null(redd_df_all)) {
    # divide reaches into various location categories
    if (basin == "Wenatchee") {
      redd_df_all <-
        redd_df_all |>
        dplyr::mutate(location = dplyr::case_when(reach %in% paste0("W", 8:10) ~ "Above Tumwater",
          reach %in% paste0("W", 1:7) ~ "Below Tumwater",
          .default = "Tributaries"
        ))
    } else if (basin == "Methow") {
      redd_df_all <-
        redd_df_all |>
        dplyr::mutate(location = dplyr::case_when(reach == "T1" ~ "Twisp",
          reach == "MH1" ~ "Spring Creek",
          reach == "WN1" ~ "Spring Creek",
          .default = "Lower Methow"
        ))
    }

    # drop surveys that didn't actually happen (NAs for new redds)
    redd_df_all <-
      redd_df_all |>
      dplyr::filter(!is.na(new_redds))

    # predict net error
    redd_df_all <-
      redd_df_all |>
      predict_neterr(
        species = "Steelhead",
        num_obs = n_observers
      )
  }

  #-----------------------------------------------------------------
  # load data on error calls for sex at Priest Rapids when fish were tagged

  message("\t Pulling PIT tag data.\n\n")

  dabom_df <- dplyr::tibble(
    spawn_year = query_year,
    dam_nm = dplyr::case_when(spawn_year %in% c(2011:2015, 2018) ~ "PriestRapids",
      spawn_year >= 2026 ~ "PriestRapids",
      .default = "RockIsland"
    )
  )

  # get info on tags detected somewhere in the basin
  all_tags <- dabom_df |>
    dplyr::mutate(tag_summ = purrr::map2(spawn_year,
      dam_nm,
      .f = function(yr, dam_nm) {
        query_dabom_results(
          dabom_file_path = dabom_file_path,
          dabom_dam_nm = dam_nm,
          dabom_file_name = dabom_file_name,
          query_year = yr,
          result_type = "tag_summ"
        ) |>
          dplyr::select(-dplyr::any_of("spawn_year"))
      }
    )) |>
    dplyr::select(-dam_nm) |>
    tidyr::unnest(tag_summ)

  if ("spawn_node" %in% names(all_tags)) {
    all_tags <-
      all_tags |>
      rename(final_node = spawn_node)
  }

  if (!"cwt" %in% names(all_tags) |
    !"ad_clip" %in% names(all_tags)) {
    all_tags <-
      all_tags |>
      dplyr::mutate(
        cwt = dplyr::if_else(stringr::str_detect(conditional_comments, "CP") |
          stringr::str_detect(conditional_comments, "CW"),
        TRUE, FALSE
        ),
        ad_clip = dplyr::case_when(stringr::str_detect(conditional_comments, "AD") ~ TRUE,
          stringr::str_detect(conditional_comments, "AI") ~ FALSE,
          .default = NA
        )
      )
  }

  if (!"sex" %in% names(all_tags)) {
    all_tags <-
      all_tags |>
      dplyr::mutate(sex = dplyr::case_when(stringr::str_detect(conditional_comments, "MA") ~ "M",
        stringr::str_detect(conditional_comments, "FE") ~ "F",
        .default = NA_character_
      ))
  }

  if (basin == "Methow") {
    basin_tags_all <-
      all_tags |>
      dplyr::filter(stringr::str_detect(path, "LMR") |
        stringr::str_detect(path, "MTB")) |>
      dplyr::mutate(
        location = dplyr::case_when(
          stringr::str_detect(final_node, "^MRC") ~ "Lower Methow",
          stringr::str_detect(final_node, "^LMR") ~ "Lower Methow",
          stringr::str_detect(final_node, "^MTB") ~ "Lower Methow",
          stringr::str_detect(path, " LBC") ~ "Libby",
          stringr::str_detect(path, " GLC") ~ "Gold",
          stringr::str_detect(path, " BVC") ~ "Beaver",
          stringr::str_detect(path, " TWR") ~ "Twisp",
          stringr::str_detect(path, " MSH") ~ "Methow Fish Hatchery",
          stringr::str_detect(path, " SCP") ~ "Spring Creek",
          stringr::str_detect(path, " CRW") ~ "Chewuch",
          stringr::str_detect(path, " MRW") ~ "Upper Methow",
          .default = NA_character_
        ),
        dplyr::across(
          location,
          ~ factor(.,
            levels = c(
              "Lower Methow",
              "Upper Methow",
              "Chewuch",
              "Twisp",
              "Methow Fish Hatchery",
              "Spring Creek",
              "Beaver",
              "Gold",
              "Libby"
            )
          )
        )
      ) |>
      dplyr::select(
        spawn_year,
        tag_code,
        location,
        origin,
        sex,
        ad_clip,
        cwt
      ) |>
      # differentiate different tags in hatchery fish
      # this rule set came from Charlie Snow for the Methow
      dplyr::mutate(mark_grp = dplyr::case_when(origin == "W" ~ "W",
        ad_clip & !cwt ~ "HOR-SN",
        cwt ~ "HOR-C",
        origin == "H" & (!cwt | !ad_clip) ~ "HOR-C",
        .default = NA_character_
      )) |>
      dplyr::mutate(
        dplyr::across(
          mark_grp,
          ~ factor(.,
            levels = c(
              "W",
              "HOR-SN",
              "HOR-C"
            )
          )
        )
      )
  }

  if (basin == "Wenatchee") {
    basin_tags_all <-
      all_tags |>
      dplyr::filter(stringr::str_detect(path, "LWE")) |>
      dplyr::mutate(
        location = dplyr::case_when(final_node %in% c("TUM", "UWE") ~ "Above Tumwater",
          stringr::str_detect(final_node, "^LWE") ~ "Below Tumwater",
          stringr::str_detect(path, "CHL") ~ "Chiwawa",
          stringr::str_detect(path, "NAL") ~ "Nason",
          stringr::str_detect(path, "PES") ~ "Peshastin",
          .default = "Other Tributaries"
        ),
        dplyr::across(
          location,
          ~ factor(.,
            levels = c(
              "Below Tumwater",
              "Above Tumwater",
              "Peshastin",
              "Nason",
              "Chiwawa",
              "Other Tributaries"
            )
          )
        )
      ) |>
      dplyr::select(
        spawn_year,
        tag_code,
        location,
        origin,
        sex,
        ad_clip,
        cwt
      ) |>
      # differentiate different tags in hatchery fish
      # this rule set came from Katy Shelby for the Wenatchee
      dplyr::mutate(mark_grp = dplyr::case_when(origin == "W" ~ "W",
        ad_clip ~ "HOR-SN",
        origin == "H" & !ad_clip ~ "HOR-C",
        .default = NA_character_
      )) |>
      dplyr::mutate(
        dplyr::across(
          mark_grp,
          ~ factor(.,
            levels = c(
              "W",
              "HOR-SN",
              "HOR-C"
            )
          )
        )
      )
  }

  #-------------------------------------------------------
  # generate fish / redd and pHOS for different areas
  fpr_all <- basin_tags_all |>
    dplyr::mutate(
      dplyr::across(
        c(sex),
        ~ dplyr::recode(.,
          "Male" = "M",
          "Female" = "F"
        )
      )
    ) |>
    dplyr::group_by(
      spawn_year,
      location
    ) |>
    dplyr::summarize(
      n_male = dplyr::n_distinct(tag_code[sex == "M"]),
      n_female = dplyr::n_distinct(tag_code[sex == "F"]),
      n_sexed = n_male + n_female,
      n_wild = dplyr::n_distinct(tag_code[origin == "W"]),
      n_hatch = dplyr::n_distinct(tag_code[origin == "H"]),
      n_origin = n_wild + n_hatch,
      n_hor_sn = dplyr::n_distinct(tag_code[mark_grp == "HOR-SN"]),
      n_hor_c = dplyr::n_distinct(tag_code[mark_grp == "HOR-C"]),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      prop_m = n_male / n_sexed,
      prop_se = sqrt((prop_m * (1 - prop_m)) / (n_sexed)),
      fpr = (prop_m) / (1 - prop_m) + 1
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(fpr_se = msm::deltamethod(~ x1 / (1 - x1) + 1,
      mean = prop_m,
      cov = prop_se^2
    )) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      phos = n_hatch / n_origin,
      phos_se = sqrt((phos * (1 - phos)) / (n_origin))
    )

  if (adjust_fpr) {
    message("\t Adjusting fish/redd.\n")

    # adjust fish / redd for errors in Priest sex calls
    # the excel file contains rounded numbers, so re-calculate
    # various statistics for use in analyses
    # estimate error rate for each sex
    sex_comparison <-
      all_tags |>
      dplyr::mutate(
        dplyr::across(
          c(sex),
          ~ dplyr::recode(.,
            "Male" = "M",
            "Female" = "F"
          )
        )
      ) |>
      dplyr::select(spawn_year,
        tag_code,
        sex_field = sex
      ) |>
      dplyr::inner_join(
        readxl::read_excel(
          paste(brood_file_path,
            brood_file_name,
            sep = "/"
          ),
          sheet = "Brood Collected_PIT Tagged Only"
        ) |>
          janitor::clean_names() |>
          dplyr::rename(tag_code = recaptured_pit) |>
          dplyr::select(
            spawn_year,
            tag_code,
            sex_final
          ) |>
          dplyr::distinct(),
        by = dplyr::join_by(spawn_year, tag_code)
      ) |>
      # update one tag, based on genetics and notes
      dplyr::mutate(
        dplyr::across(
          sex_final,
          ~ dplyr::case_when(
            spawn_year == 2025 &
              tag_code == "3DD.003E0FF10A" ~ "M",
            .default = .
          )
        )
      ) |>
      dplyr::filter(
        !is.na(sex_final),
        !is.na(sex_field)
      ) |>
      dplyr::mutate(sex_final = dplyr::recode(sex_final, "Male" = "M", "Female" = "F")) |>
      dplyr::distinct(spawn_year, tag_code, sex_field, sex_final)

    if (any(!sex_comparison$sex_field %in% c("M", "F")) ||
      any(!sex_comparison$sex_final %in% c("M", "F"))) {
      stop("Known and field sex calls must be M or F.", call. = FALSE)
    }
    if (anyDuplicated(sex_comparison[c("spawn_year", "tag_code")])) {
      stop("Conflicting sex calls found for the same tag and spawn year.", call. = FALSE)
    }

    sex_err_rate <-
      sex_comparison |>
      dplyr::mutate(agree = dplyr::if_else(sex_field == sex_final,
        TRUE, FALSE
      )) |>
      dplyr::group_by(spawn_year,
        sex = sex_final
      ) |>
      dplyr::summarize(
        n_tags = dplyr::n_distinct(tag_code),
        n_true = sum(agree),
        n_false = sum(!agree),
        .groups = "drop"
      ) |>
      dplyr::mutate(binom_ci = purrr::map2(n_false,
        n_tags,
        .f = function(x, y) {
          DescTools::BinomCI(x, y) |>
            dplyr::as_tibble()
        }
      )) |>
      tidyr::unnest(binom_ci, ptype = list(
        binom_ci = tibble::tibble(est = double(), lwr.ci = double(), upr.ci = double())
      )) |>
      janitor::clean_names() |>
      dplyr::rename(
        perc_false = est,
        lowerci = lwr_ci,
        upperci = upr_ci
      ) |>
      dplyr::mutate(perc_se = sqrt((perc_false * (1 - perc_false)) / n_tags)) |>
      dplyr::relocate(perc_se,
        .after = "perc_false"
      )

    adj_fpr <-
      fpr_all |>
      filter_out(n_male == 0 |
        n_female == 0) |>
      dplyr::select(spawn_year:n_sexed) |>
      dplyr::left_join(
        sex_err_rate |>
          dplyr::select(spawn_year,
            sex,
            brood_tags = n_tags,
            brood_false = n_false
          ) |>
          dplyr::mutate(sex = factor(sex, levels = c("M", "F"))) |>
          tidyr::pivot_wider(
            names_from = "sex",
            values_from = starts_with("brood"),
            names_expand = TRUE,
            values_fill = 0
          ),
        by = dplyr::join_by(spawn_year)
      ) |>
      dplyr::mutate(dplyr::across(
        dplyr::starts_with("brood_"),
        ~ tidyr::replace_na(., 0)
      )) |>
      dplyr::mutate(
        boot_sim = purrr::pmap(
          .l = list(
            spawn_year = spawn_year,
            b_x = brood_tags_M,
            b_y = brood_tags_F,
            false_x = brood_false_M,
            false_y = brood_false_F,
            samp_x = n_male,
            samp_y = n_female
          ),
          .f = function(spawn_year, ...) {
            zero_error <- sex_zero_error[[as.character(spawn_year)]]
            if (is.null(zero_error)) zero_error <- character()
            boot_adj_prop(...,
              seed = 4,
              prior = sex_error_prior,
              zero_error = zero_error
            )
          },
          .progress = TRUE
        ),
        x_y_est = purrr::map(boot_sim,
          .f = est_adj_prop,
          .progress = TRUE
        ),
        prop_est = purrr::map(boot_sim,
          .f = est_adj_prop,
          estimate = "prop_x",
          .progress = TRUE
        ),
        adj_male = purrr::map_dbl(boot_sim,
          .f = function(x) {
            median(x$x_boot)
          }
        )
      ) |>
      tidyr::unnest(c(
        x_y_est,
        prop_est
      )) |>
      dplyr::mutate(
        dplyr::across(
          n_female,
          ~ case_when(n_male != adj_male ~ n_sexed - adj_male,
            .default = .
          )
        ),
        dplyr::across(
          n_male,
          ~ case_when(n_male != adj_male ~ adj_male,
            .default = .
          )
        ),
        fpr = x_y_est + 1
      ) |>
      dplyr::rename(
        fpr_se = x_y_se,
        prop_m = prop_x,
        prop_se = prop_x_se
      ) |>
      dplyr::select(dplyr::any_of(names(fpr_all)))

    adj_fpr <-
      adj_fpr |>
      dplyr::bind_rows(fpr_all |>
        dplyr::filter(n_male == 0 |
          n_female == 0) |>
        dplyr::select(dplyr::any_of(names(adj_fpr))))

    # add other information back
    adj_fpr <-
      fpr_all |>
      dplyr::select(
        !any_of(names(adj_fpr)),
        spawn_year,
        location
      ) |>
      dplyr::left_join(adj_fpr,
        by = dplyr::join_by(
          spawn_year,
          location
        )
      ) |>
      dplyr::select(dplyr::all_of(names(fpr_all)))

    # if any fpr values are Inf, use the older ones
    if (sum(adj_fpr$fpr == Inf, na.rm = TRUE) > 0) {
      adj_fpr <- adj_fpr |>
        dplyr::left_join(fpr_all |>
          dplyr::select(spawn_year, location,
            old_fpr = fpr,
            old_se = fpr_se
          ), by = c("spawn_year", "location")) |>
        dplyr::mutate(
          fpr = dplyr::if_else(is.na(fpr) | fpr == Inf,
            old_fpr,
            fpr
          ),
          fpr_se = dplyr::if_else(is.na(fpr_se) | fpr_se == Inf,
            old_se,
            fpr_se
          )
        ) |>
        dplyr::select(-dplyr::starts_with("old"))
    }

    # if any values are 1 (i.e. no males) use a weighted average
    if (sum(adj_fpr$fpr == 1, na.rm = TRUE) > 0) {
      avg_fpr <-
        adj_fpr |>
        filter(n_male > 0) |>
        summarize(across(
          c(
            fpr,
            fpr_se
          ),
          ~ weighted.mean(.,
            w = n_sexed
          )
        ))

      adj_fpr <-
        adj_fpr |>
        mutate(
          across(
            fpr,
            ~ case_when(n_male == 0 | fpr == 1 ~ avg_fpr$fpr,
              .default = .
            )
          ),
          across(
            fpr_se,
            ~ case_when(n_male == 0 | fpr == 1 ~ avg_fpr$fpr_se,
              .default = .
            )
          )
        )
    }


    fpr_all <- adj_fpr

    rm(adj_fpr)
  } else {
    sex_err_rate <-
      tibble(spawn_year = query_year)
  }

  #-----------------------------------------------------------------
  # read in data about known removals of fish prior to spawning
  removal_path <- file.path(removal_file_path, removal_file_name)
  if (file.exists(removal_path)) {
    removal_ext <- tolower(tools::file_ext(removal_file_name))
    if (removal_ext == "csv") {
      removal_df <- readr::read_csv(removal_path) |>
        janitor::clean_names() |>
        dplyr::filter(
          subbasin == basin,
          spawn_year %in% query_year
        )
    } else if (removal_ext %in% c("xls", "xlsx")) {
      removal_df <- readxl::read_excel(
        removal_path,
        skip = 3,
        col_names = c(
          "run cycle",
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
          "notes"
        )
      ) |>
        janitor::clean_names() |>
        dplyr::mutate(
          across(
            c(
              ends_with("_h"),
              ends_with("_w"),
              ends_with("_t")
            ),
            as.numeric
          )
        ) |>
        dplyr::filter(!is.na(spawn_year)) |>
        dplyr::select(spawn_year:harvest_w) |>
        tidyr::pivot_longer(
          cols = c(
            ends_with("_h"),
            ends_with("_w")
          ),
          names_to = "source",
          values_to = "removed"
        ) |>
        dplyr::mutate(origin = stringr::str_sub(source, -1)) |>
        dplyr::relocate(origin,
          .before = "removed"
        ) |>
        dplyr::filter(origin %in% c("h", "w")) |>
        dplyr::mutate(
          dplyr::across(
            source,
            ~ stringr::str_remove(
              .,
              "_h$"
            )
          ),
          dplyr::across(
            source,
            ~ stringr::str_remove(
              .,
              "_w$"
            )
          ),
          dplyr::across(
            source,
            ~ stringr::str_to_title(stringr::str_replace_all(., "_", " "))
          ),
          dplyr::across(
            origin,
            ~ dplyr::recode(.,
              "h" = "Hatchery",
              "w" = "Natural"
            )
          )
        ) |>
        dplyr::filter(
          spawn_year %in% query_year,
          population == basin
        )
    } else {
      stop("Unsupported removal file type: ", removal_ext, call. = FALSE)
    }
  } else {
    message("Removal data not found.\n")
    removal_df <- NULL
  }

  #-----------------------------------------------------------------
  # pull in some estimates from DABOM

  message("\t Gathering PIT escapement estimates.\n")

  if (basin == "Methow") {
    if (sum(stringr::str_detect(all_tags$final_node, "MTB")) > 0) {
      all_locs <- c(
        "MTB",
        "MTB_bb",
        "LMR_bb",
        "MRC_bb",
        "GLC",
        "LBC",
        "MSH",
        "MRW",
        "TWR",
        "CRW",
        "SCP",
        "BVC"
      )
    } else {
      all_locs <- c(
        "LMR",
        "LMR_bb",
        "MRC_bb",
        "GLC",
        "LBC",
        "MSH",
        "MRW",
        "TWR",
        "CRW",
        "SCP",
        "BVC"
      )
    }
  } else if (basin == "Wenatchee") {
    all_locs <- c(
      "ICL",
      "PES",
      "MCL",
      "CHM",
      "CHW",
      "CHL",
      "NAL",
      "LWN",
      "WTL",
      "LWE",
      "LWE_bb",
      "TUM_bb",
      "UWE_bb"
    )
  }
  # non-tributary locations
  main_locs <-
    all_locs[all_locs %in%
      union(
        all_locs[stringr::str_detect(all_locs, "_bb$")],
        all_locs[stringr::str_detect(all_locs, "_bb$")] |>
          stringr::str_remove("_bb$")
      )]

  # tributary locations
  trib_locs <-
    all_locs[!all_locs %in% main_locs]

  all_escp <- dabom_df |>
    dplyr::mutate(escp = purrr::map2(spawn_year,
      dam_nm,
      .f = function(yr, dam_nm) {
        query_dabom_results(
          dabom_file_path = dabom_file_path,
          dabom_dam_nm = dam_nm,
          dabom_file_name = dabom_file_name,
          query_year = yr,
          result_type = "escape_summ"
        )
      }
    )) |>
    dplyr::select(-c(
      spawn_year,
      dam_nm
    )) |>
    tidyr::unnest(escp) |>
    dplyr::filter(location %in% all_locs) |>
    dplyr::select(spawn_year,
      origin,
      location,
      estimate = median,
      se = sd,
      lci = lower_ci,
      uci = upper_ci
    )

  # pull out estimates of tributary spawners from DABOM
  trib_spawners_all <- all_escp |>
    dplyr::filter(location %in% trib_locs) |>
    dplyr::select(spawn_year,
      origin,
      location,
      spawners = estimate,
      spawners_se = se,
      lci,
      uci
    ) |>
    dplyr::mutate(
      dplyr::across(
        origin,
        ~ dplyr::replace_values(
          .,
          "W" ~ "Natural",
          "H" ~ "Hatchery"
        )
      ),
      dplyr::across(
        location,
        ~ dplyr::replace_values(
          .,
          "CHL" ~ "Chiwawa",
          "CHM" ~ "Chumstick",
          "CHW" ~ "Chiwaukum",
          "ICL" ~ "Icicle",
          "LWN" ~ "Little Wenatchee",
          "MCL" ~ "Mission",
          "NAL" ~ "Nason",
          "PES" ~ "Peshastin",
          "WTL" ~ "White River",
          "GLC" ~ "Gold",
          "LBC" ~ "Libby",
          "MSH" ~ "Methow Fish Hatchery",
          "MRW" ~ "Upper Methow",
          "TWR" ~ "Twisp",
          "CRW" ~ "Chewuch",
          "SCP" ~ "Spring Creek",
          "BVC" ~ "Beaver"
        )
      )
    ) |>
    dplyr::arrange(location, origin)

  # pull out mainstem escapement estimates
  escp_est_all <-
    dabom_df |>
    dplyr::mutate(post = purrr::map2(spawn_year,
      dam_nm,
      .f = function(yr, dam_nm) {
        query_dabom_results(
          dabom_file_path = dabom_file_path,
          dabom_dam_nm = dam_nm,
          dabom_file_name = dabom_file_name,
          query_year = yr,
          result_type = "escape_post"
        )
      }
    )) |>
    dplyr::select(-dam_nm) |>
    tidyr::unnest(post) |>
    dplyr::rename(location = param) |>
    dplyr::filter(location %in% main_locs) |>
    dplyr::mutate(
      dplyr::across(
        location,
        ~ dplyr::replace_values(
          .,
          "LWE" ~ "Wen_all",
          "LWE_bb" ~ "Below Tumwater",
          "TUM_bb" ~ "Above Tumwater",
          "UWE_bb" ~ "Above Tumwater",
          "LMR" ~ "Methow_all",
          "MTB" ~ "Methow_all",
          "MTB_bb" ~ "Lower Methow",
          "LMR_bb" ~ "Lower Methow",
          "MRC_bb" ~ "Lower Methow"
        )
      )
    ) |>
    dplyr::mutate(
      dplyr::across(
        origin,
        ~ dplyr::replace_values(
          .,
          "W" ~ "Natural",
          "H" ~ "Hatchery"
        )
      )
    ) |>
    dplyr::group_by(
      spawn_year,
      location,
      origin,
      chain,
      iter
    ) |>
    dplyr::summarise(
      dplyr::across(
        abund,
        sum
      ),
      .groups = "drop"
    ) |>
    dplyr::group_by(
      spawn_year,
      location,
      origin
    ) |>
    dplyr::summarize(
      estimate = median(abund),
      se = sd(abund),
      lci = coda::HPDinterval(coda::as.mcmc(abund))[, 1],
      uci = coda::HPDinterval(coda::as.mcmc(abund))[, 2],
      .groups = "drop"
    )

  #-----------------------------------------------------------------
  # use escapement estimates rather than tags to estimate pHOS
  if (phos_data == "escapement") {
    escp_phos <-
      escp_est_all |>
      dplyr::bind_rows(trib_spawners_all |>
        dplyr::rename(
          estimate = spawners,
          se = spawners_se
        )) |>
      dplyr::select(
        spawn_year,
        location,
        origin,
        estimate,
        se
      ) |>
      tidyr::pivot_wider(
        names_from = origin,
        values_from = c(estimate, se)
      ) |>
      dplyr::rowwise() |>
      dplyr::mutate(
        phos = estimate_Hatchery / (estimate_Hatchery + estimate_Natural),
        phos_se = msm::deltamethod(~ x1 / (x1 + x2),
          mean = c(
            estimate_Hatchery,
            estimate_Natural
          ),
          cov = diag(c(
            se_Hatchery,
            se_Natural
          )^2)
        )
      ) |>
      dplyr::ungroup()

    fpr_all <-
      fpr_all |>
      dplyr::left_join(
        escp_phos |>
          dplyr::select(spawn_year,
            location,
            phos2 = phos,
            phos_se2 = phos_se
          ),
        by = dplyr::join_by(spawn_year, location)
      ) |>
      dplyr::mutate(
        dplyr::across(
          phos,
          ~ dplyr::if_else(!is.na(phos2),
            phos2,
            .
          )
        ),
        dplyr::across(
          phos_se,
          ~ dplyr::if_else(!is.na(phos_se2),
            phos_se2,
            .
          )
        )
      ) |>
      dplyr::select(-c(
        phos2,
        phos_se2
      ))
  }


  result <- list(
    redd_df = redd_df_all,
    basin_tags = basin_tags_all,
    sex_err = sex_err_rate,
    fpr_df = fpr_all,
    trib_spawners = trib_spawners_all,
    escp_est = escp_est_all,
    rem_df = removal_df
  )

  if (save_rda) {
    if (save_by_year) {
      for (yr in query_year) {
        message("Saving data from spawn year ", yr, ".")
        yearly <- lapply(result, function(x) {
          if (is.null(x)) {
            return(NULL)
          }
          dplyr::filter(x, spawn_year == yr)
        })
        file_nm <- if (is.null(save_file_name)) {
          paste0(basin, "_", yr, ".rda")
        } else {
          save_file_name
        }
        save(
          list = names(yearly), file = file.path(save_file_path, file_nm),
          envir = list2env(yearly, parent = emptyenv())
        )
      }
    } else {
      if (is.null(save_file_name)) {
        years <- if (length(query_year) > 1L) {
          paste(range(query_year), collapse = "-")
        } else {
          as.character(query_year)
        }
        save_file_name <- paste0(basin, "_", years, ".rda")
      }
      save(
        list = names(result), file = file.path(save_file_path, save_file_name),
        envir = list2env(result, parent = emptyenv())
      )
    }
  }
  if (assign_global) {
    list2env(result, envir = .GlobalEnv)
  }
  result
}
