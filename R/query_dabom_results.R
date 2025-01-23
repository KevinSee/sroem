#' @title Query DABOM results
#'
#' @description Loads .Rdata objects that have been saved for a specific year's DABOM model run, and returns specified type of results.
#'
#' @author Kevin See
#'
#' @param dabom_file_path file path to DABOM results. Should contain at least two folders names `RockIsland` and `PriestRapids`.
#' @param dabom_dam_nm name of dam where dam counts were used to generate escapement estimates. Choices are `RockIsland` (default) or `PriestRapids`
#' @param dabom_file_name partial name of .Rdata object saved after running DABOM model for Upper Columbia.
#' @param query_year which spawn year should results be queried fof?
#' @param result_type what type of results should be queried for? Options include summary of escapement estimates (`escape_summ`), tag summary (`tag_summ`), posterior draws of escapement (`escape_post`), summary of dam counts and adjusted counts (`dam_summ`), total escapement by origin used in the results (`total_escape`), summary of detection probabilities (`detect_summ`) or parent-child table (`parent_child`). Default is `escape_summ`.
#'
#' @import lubridate
#' @return tibble
#' @export

query_dabom_results <- function(
    dabom_file_path = "O:Documents/Git/MyProjects/DabomPriestRapidsSthd/analysis/data/derived_data/estimates",
    dabom_dam_nm = c(
      "RockIsland",
      "PriestRapids"
    ),
    dabom_file_name = "UC_Sthd_DABOM_",
    query_year = lubridate::year(lubridate::today()) - 1,
    result_type = c(
      "escape_summ",
      "tag_summ",
      "escape_post",
      "dam_summ",
      "total_escape",
      "detect_summ",
      "parent_child"
    )) {
  dabom_dam_nm <- match.arg(dabom_dam_nm)

  data_file <- paste0(
    dabom_file_path,
    "/",
    dabom_dam_nm,
    "/",
    dabom_file_name,
    query_year,
    ".rda"
  )

  if (!file.exists(data_file)) {
    stop("File not found.")
  }

  result_type <- match.arg(result_type)

  load(data_file)

  if (result_type == "escape_summ") {
    return(escape_summ)
  } else if (result_type == "tag_summ") {
    return(tag_summ)
  } else if (result_type == "escape_post") {
    return(escape_post)
  } else if (result_type == "dam_summ") {
    return(dam_escp_df)
  } else if (result_type == "total_escape") {
    return(org_escape)
  } else if (result_type == "detect_summ") {
    return(detect_summ)
  } else if (result_type == "parent_child") {
    return(parent_child)
  }
}
