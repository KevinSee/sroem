#' Read a result from a saved DABOM run
#'
#' Loads a single named result from a DABOM `.rda` file into a private
#' environment. Other objects in the caller's environment are not modified.
#'
#' @param dabom_file_path Directory containing the dam-specific folders.
#' @param dabom_dam_nm `"RockIsland"` or `"PriestRapids"`.
#' @param dabom_file_name Prefix of the `.rda` file, before the year.
#' @param query_year Spawn year used in the file name.
#' @param result_type One of `"escape_summ"`, `"tag_summ"`, `"escape_post"`,
#'   `"dam_summ"`, `"total_escape"`, `"detect_summ"`, or `"parent_child"`.
#'
#' @return The selected object, with its original class and contents.
#' @export
query_dabom_results <- function(
  dabom_file_path = file.path(
    "C:", "Users", "seek1477", "Documents", "Git", "MyProjects",
    "DabomPriestRapidsSthd", "analysis", "data", "derived_data", "estimates"
  ),
  dabom_dam_nm = c("RockIsland", "PriestRapids"),
  dabom_file_name = "UC_Sthd_DABOM_",
  query_year = lubridate::year(lubridate::today()) - 1,
  result_type = c(
    "escape_summ", "tag_summ", "escape_post", "dam_summ",
    "total_escape", "detect_summ", "parent_child"
  )
) {
  dabom_dam_nm <- match.arg(dabom_dam_nm)
  result_type <- match.arg(result_type)
  data_file <- file.path(
    dabom_file_path, dabom_dam_nm, paste0(dabom_file_name, query_year, ".rda")
  )
  if (length(data_file) != 1L || !file.exists(data_file)) {
    stop("DABOM file not found: ", paste(data_file, collapse = ", "),
      call. = FALSE
    )
  }

  object_name <- switch(result_type,
    dam_summ = "dam_escp_df",
    total_escape = "org_escape",
    result_type
  )
  results <- new.env(parent = emptyenv())
  loaded <- load(data_file, envir = results)
  if (!object_name %in% loaded) {
    stop("DABOM file does not contain '", object_name, "': ", data_file,
      call. = FALSE
    )
  }
  get(object_name, envir = results, inherits = FALSE)
}
