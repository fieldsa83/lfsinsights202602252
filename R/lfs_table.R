#' Create LFS summary tables with flexible estimate types
#'
#' @param start_date Start date of the range (YYYY-MM-DD).
#' @param end_date End date of the range (YYYY-MM-DD).
#' @param analysis_vars Variables to analyze (comma-separated string or character vector). Default is "PROV".
#' @param filter_condition Global filter condition applied during data loading (e.g., "AGE >= 15"). This filter is applied before any estimate-specific filters.
#' @param filter_months Optional months to filter (numeric vector or comma-separated string).
#' @param filter_years Optional years to filter (character vector or comma-separated string).
#' @param moving_avg Number of periods for moving average (default: 1).
#' @param estimates List of estimate configurations. This can be either a nested list (for multiple estimates), a single list, or omitted (defaults to a simple sum). Each estimate sub-list can contain:
#'   \itemize{
#'     \item \code{est_name}: Character string for the output column name.
#'     \item \code{est_type}: The type of calculation: "sum", "ratio", or "ratio_distribution".
#'     \item \code{est_filter}: Optional filter applied specifically to this estimate.
#'     \item \code{ratio_numerator} & \code{ratio_denominator}: Required for "ratio" types.
#'     \item \code{var_name}: Required if \code{est_type = "ratio_distribution"}. The variable to calculate the distribution across.
#'     \item \code{ratio_type}: "percent" or "average" (default is "percent").
#'     \item \code{ratio_decimals}: Integer for rounding ratios (default is 1).
#'   }
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
#' @param weight_rounding Numeric factor for rounding estimate levels, including sum, numerator, and denominator (default: 100).
#' @param include_marginals Logical. Whether to include marginal totals (default: TRUE).
#' @param add_suppression_flag Logical. Whether to add basic suppression flags to the output (default: FALSE).
#' @param calculate_change Logical. Whether to calculate period-over-period changes (default: FALSE).
#' @param lag_period Integer. Number of periods to lag for the change calculation (default: 1).
#' @param calculate_difference Logical. If TRUE, calculates the difference between two categories defined in \code{comparison_variable} (default: FALSE).
#' @param comparison_variable Character. The name of the column containing the categories to compare (e.g., "GENDER"). Required if \code{calculate_difference = TRUE}.
#' @param comparison_categories Character vector of length 2. The specific categories within the \code{comparison_variable} to calculate the difference between (e.g., \code{c("Male", "Female")}).
#' @param ... Additional parameters passed to the processing functions.
#'
#' @return A data frame containing LFS tabular results for all estimates.
#' @export
lfs_table <- function(
    # Required parameters
    start_date,
    end_date,
    estimates = NULL,
    # Common parameters with defaults
    analysis_vars = "PROV",
    filter_condition = "AGE >= 15 & !is.na(LFSSTAT)",
    weight_var = "FINALWT",
    moving_avg = 1,
    filter_months = NULL,
    filter_years = NULL,
    add_custom_dvs = TRUE,
    # Data source options
    prerelease = FALSE,
    bootstraps = FALSE,
    tabs_plus = FALSE,
	supplement = FALSE,      # Deprecated parameter
    supplement_lmi = FALSE,
    supplement_lmsi = FALSE,
    supplement_dlmi = FALSE,
    north = FALSE,
    add_labels = TRUE,
    language = "EN",
    # Calculation options
    weight_rounding = 100,
    include_marginals = TRUE,
    add_suppression_flag = FALSE,
    calculate_change = FALSE,
    lag_period = 1,
    calculate_difference = FALSE,
    comparison_variable = NULL,
    comparison_categories = NULL,
    # Additional parameters
    ...) {
  # Default to simple sum if estimates is NULL
  if (is.null(estimates)) {
    estimates <- list(est_type = "sum")
  }
  
  # Backwards compatibility: Map 'supplement' to 'supplement_lmi'
  if (supplement) {
    supplement_lmi <- TRUE
    message("Note: The 'supplement' argument is deprecated. Now using 'supplement_lmi' instead.")
  }
  
  # Check if estimates is a simple list (not nested) and convert it
  # We check if the first element is not a list, or if there's an est_type directly in estimates
  if (!is.list(estimates[[1]]) || !is.null(estimates$est_type)) {
    # Convert simple list to nested list
    estimates <- list(estimates)
  }

  # Create configuration list
  config <- list(
    start_date = start_date,
    end_date = end_date,
    estimates = estimates,
    analysis_vars = analysis_vars,
    filter_condition = filter_condition,
    weight_var = weight_var,
    moving_avg = moving_avg,
    filter_months = filter_months,
    filter_years = filter_years,
    add_custom_dvs = add_custom_dvs,
    prerelease = prerelease,
    bootstraps = bootstraps,
    tabs_plus = tabs_plus,
    supplement_lmi = supplement_lmi,
    supplement_lmsi = supplement_lmsi,
    supplement_dlmi = supplement_dlmi,
    north = north,
    add_labels = add_labels,
    language = language,
    weight_rounding = weight_rounding,
    include_marginals = include_marginals,
    add_suppression_flag = add_suppression_flag,
    calculate_change = calculate_change,
    calculate_difference = calculate_difference,
    comparison_variable = comparison_variable,
    comparison_categories = comparison_categories,
    lag_period = lag_period,
    microdata_only = FALSE
  )

  # Add any additional parameters
  config <- c(config, list(...))

  # Process the data
  result <- process_lfs_data(config)

  return(result)
}
