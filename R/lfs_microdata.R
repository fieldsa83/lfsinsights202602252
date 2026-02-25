#' Load LFS microdata
#'
#' @description
#' Retrieves Labour Force Survey (LFS) microdata for the specified date range without
#' any aggregation or summary calculations. This function provides direct access to
#' individual records for custom analysis.
#'
#' @param start_date Start date of the range (YYYY-MM-DD).
#' @param end_date End date of the range (YYYY-MM-DD).
#' @param filter_condition Optional filter condition expression (e.g., "AGE >= 15").
#' @param filter_months Optional months to filter (numeric vector or comma-separated string).
#' @param filter_years Optional years to filter (character vector or comma-separated string).
#' @param add_custom_dvs Whether to add custom derived variables (default: TRUE).
#' @param weight_var Weighting variable to use (default: "FINALWT").
#' @param prerelease Whether to use pre-release data (default: FALSE).
#' @param bootstraps Whether to include bootstrap weights (default: FALSE).
#' @param tabs_plus Whether to include TABS Plus data (default: FALSE).
#' @param supplement Deprecated. Please use \code{supplement_lmi} instead.
#' @param supplement_lmi Whether to include LMI (monthly) supplement data (default: FALSE).
#' @param supplement_lmsi Whether to include LMSI (quarterly) supplement data (default: FALSE).
#' @param supplement_dlmi Whether to include DLMI (annual disability) supplement data (default: FALSE).
#' @param north Whether to include north data (default: FALSE).
#' @param add_labels Whether to apply labels (default: FALSE).
#' @param language Language for labels, "EN" or "FR" (default: "EN").
#' @param ... Additional parameters passed to the processing functions.
#'
#' @return A data frame containing LFS microdata records.
#'
#' @details
#' This function retrieves individual LFS survey records without performing any
#' aggregation or summary calculations. It's useful for custom analysis or when
#' you need to work with raw microdata.
#'
#' Unlike lfs_table(), this function does not require or use the estimates parameter
#' and does not perform any summary calculations. It simply loads and filters the
#' data according to the specified parameters.
#'
#' @examples
#' \dontrun{
#' # Basic usage: Get all records for one month
#' microdata <- lfs_microdata(
#'   start_date = "2025-01-01",
#'   end_date = "2025-01-01"
#' )
#'
#' # Filtered example: Only employed people age 25-54
#' employed_core <- lfs_microdata(
#'   start_date = "2025-01-01",
#'   end_date = "2025-03-01",
#'   filter_condition = "AGE >= 25 & AGE <= 54 & EMPLOYED == 1",
#'   add_labels = TRUE
#' )
#' }
#'
#' @export
lfs_microdata <- function(
    # Required parameters
    start_date,
    end_date,
    # Filter parameters
    filter_condition = NULL,
    filter_months = NULL,
    filter_years = NULL,
    add_custom_dvs = TRUE,
    # Data source options
    weight_var = "FINALWT",
    prerelease = FALSE,
    bootstraps = FALSE,
    tabs_plus = FALSE,
    supplement = FALSE,        # Deprecated parameter
    supplement_lmi = FALSE,    # New parameter
    supplement_lmsi = FALSE,   # New parameter
    supplement_dlmi = FALSE,   # New parameter
    north = FALSE,
    add_labels = FALSE,
    language = "EN",
    # Additional parameters
    ...) {

  # Backwards compatibility: Map 'supplement' to 'supplement_lmi'
  if (supplement) {
    supplement_lmi <- TRUE
    message("Note: The 'supplement' argument is deprecated. Now using 'supplement_lmi' instead.")
  }

  # Create configuration list
  config <- list(
    start_date = start_date,
    end_date = end_date,
    filter_condition = filter_condition,
    filter_months = filter_months,
    filter_years = filter_years,
    add_custom_dvs = add_custom_dvs,
    weight_var = weight_var,
    prerelease = prerelease,
    bootstraps = bootstraps,
    tabs_plus = tabs_plus,
    supplement_lmi = supplement_lmi,
    supplement_lmsi = supplement_lmsi,
    supplement_dlmi = supplement_dlmi,
    north = north,
    add_labels = add_labels,
    language = language,
    microdata_only = TRUE # Key parameter to indicate microdata mode
  )

  # Add any additional parameters
  config <- c(config, list(...))

  # Process the data
  result <- process_lfs_data(config)

  return(result)
}



#' Process microdata without estimates
#'
#' @param lfs_config_list Configuration
#' @return Data frame with microdata
#' @keywords internal
process_microdata <- function(lfs_config_list, required_months) {

   # Initialize custom DVs if applicable
  if (lfs_config_list$add_custom_dvs) {
    load_custom_dvs()
  }

  cat(format(Sys.time(), "%H:%M:%S"), "*** Processing microdata ***\n")

  all_data <- list()
  for (current_date in required_months) {
    current_date <- as.Date(current_date)
    tryCatch(
      {
        # Just load data without summarizing
        data <- load_month_data(current_date, lfs_config_list)
        all_data[[length(all_data) + 1]] <- data

        # Clean up
        rm(data)
        gc()
      },
      error = function(e) {
        warning("Failed to load microdata for ", format(current_date, "%Y-%m"), ": ", e$message)
      }
    )
  }

  if (length(all_data) == 0) {
    stop("No microdata was loaded")
  }

  # Combine results
  combined <- dplyr::bind_rows(all_data)

  # Apply labels if requested
  if (lfs_config_list$add_labels) {
    combined <- combined %>% label_maker(lfs_config_list$language)
  }

  # Ensure key columns are first
  combined <- combined %>%
    dplyr::select(DATE, HHLDID, LINE, dplyr::everything())

  cat(format(Sys.time(), "%H:%M:%S"), "*** Microdata processing complete ***\n")

  # End timer
  timer_result <- tictoc::toc(quiet = TRUE)
  cat(format(Sys.time(), "%H:%M:%S"), paste0("Completed in ", format(timer_result$toc - timer_result$tic, nsmall = 1), " secs \n"))

  return(combined)
}
