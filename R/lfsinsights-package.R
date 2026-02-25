#' Labour Force Survey Insights
#'
#' @description
#' A package to process and analyze Labour Force Survey (LFS) data with support
#' for multiple estimate types, custom derived variables, and extensive post-processing
#' options.
#'
#' @details
#' The lfsinsights package provides a comprehensive set of tools for working with
#' Labour Force Survey data:
#'
#' \itemize{
#'   \item Load and process microdata from LFS
#'   \item Create summary tables with multiple estimate types in a single pass
#'   \item Calculate ratios like unemployment rates and other labor market indicators
#'   \item Apply moving averages, calculate changes, and perform other post-processing
#'   \item Add custom derived variables through a user-editable configuration
#'   \item Apply labels to variables for better readability
#' }
#'
#' The package is designed to be memory-efficient, processing one month of data
#' at a time to avoid excessive memory usage.
#'
#' @section Main Functions:
#' \itemize{
#'   \item \code{\link{lfs_table}}: Create summary tables with multiple estimates
#'   \item \code{\link{lfs_microdata}}: Load microdata without aggregation
#'   \item \code{\link{label_maker}}: Apply labels to variables
#'   \item \code{\link{open_custom_dvs}}: Edit custom derived variables
#' }
#'
#' @section Helper Functions:
#' \itemize{
#'   \item \code{\link{calculate_ratio}}: Calculate ratios between two columns
#'   \item \code{\link{calculate_change}}: Calculate period-over-period changes
#'   \item \code{\link{calculate_moving_avg}}: Apply moving averages to data
#'   \item \code{\link{complete_combinations}}: Ensure all combinations are present
#'   \item \code{\link{add_suppression_flag}}: Add suppression flags based on thresholds
#' }
#'
#' @keywords internal
"_PACKAGE"
