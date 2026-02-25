#' Apply labels to a dataframe using JSON metadata
#'
#' This function reads label definitions from JSON files in the metadata directory
#' and applies them to matching columns in the provided dataframe, converting values
#' to human-readable factor labels.
#'
#' @param data A dataframe to which labels will be applied
#' @param language Character string specifying the language for labels ("EN" for English by default)
#'
#' @return A dataframe with labeled factor columns where possible
#'
#' @examples
#' \dontrun{
#' # Apply English labels
#' labeled_data <- label_maker(my_data, "EN")
#'
#' # Apply French labels
#' labeled_data_fr <- label_maker(my_data, "FR")
#' }
#'
#' @importFrom jsonlite fromJSON
#' @importFrom forcats fct_recode
#' @importFrom stats setNames
#' @importFrom dplyr mutate %>%
#' @importFrom rlang sym
#'
#' @export
label_maker <- function(data, language = "EN") {
  # Get all JSON files from the metadata directory
  metadata_path <- get_config("lfs_metadata_path")
  json_files <- list.files(path = metadata_path, pattern = "\\.json$", full.names = TRUE)

  cat(format(Sys.time(), "%H:%M:%S"), "Applying labels (", language, ") \n")

  # Initialize an empty list for labels
  labels <- list()

  # Load each file individually
  for (i in seq_along(json_files)) {
    file <- json_files[i]

    # Read and parse the JSON file
    tryCatch(
      {
        # Load the JSON data
        file_labels <- jsonlite::fromJSON(file)

        # Add each top-level element to our labels
        label_names <- names(file_labels)

        # Add to the main labels list
        for (name in label_names) {
          labels[[name]] <- file_labels[[name]]
        }
      },
      error = function(e) {
        cat("  Error loading file:", e$message, "\n")
      }
    )
  }

  suppressWarnings({
    # Identify columns present in both the dataframe and the labels list
    relevant_columns <- intersect(names(data), names(labels))

    # Loop through each relevant column
    for (col_name in relevant_columns) {
      if (!is.null(labels[[col_name]][[language]])) {
        # Construct the recode map in "Label" = "Value" pattern
        recode_map <- labels[[col_name]][[language]]
        recode_map <- stats::setNames(names(recode_map), recode_map)
        data <- data %>%
          dplyr::mutate(!!rlang::sym(col_name) := forcats::fct_recode(!!rlang::sym(col_name), !!!recode_map))
      }
    }

    return(data)
  })
}
