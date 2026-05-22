#' Determine required months for analysis
#'
#' This function calculates the list of required months to load for a given date range and moving average period.
#' It can be used directly or is called internally by `lfs_table`.
#'
#' @param start_date Start date of the range (YYYY-MM-DD).
#' @param end_date End date of the range (YYYY-MM-DD).
#' @param moving_avg Number of periods for moving average (default: 1).
#' @param filter_months Optional months to filter (numeric vector or comma-separated string).
#' @param filter_years Optional years to filter (character vector or comma-separated string).
#' @param ... Additional arguments (used for internal calls from lfs_table).
#'
#' @return A vector of dates in the format YYYY-MM-01.
#' @importFrom lubridate years
#' @export
determine_required_months <- function(start_date, end_date, moving_avg = 1, filter_months = NULL, filter_years = NULL, ...) {
  # A helper function to validate and standardize dates, including a check for day changes.
  check_dates_helper <- function(start_date_str, end_date_str) {
    # Convert to Date objects first to standardize
    start_date_obj <- try(as.Date(start_date_str), silent = TRUE)
    end_date_obj <- try(as.Date(end_date_str), silent = TRUE)

    # Check if dates are valid
    if (inherits(start_date_obj, "try-error") || is.na(start_date_obj)) {
      cli::cli_abort("Invalid start_date format. Please provide a date in YYYY-MM-DD format.")
    }
    if (inherits(end_date_obj, "try-error") || is.na(end_date_obj)) {
      cli::cli_abort("Invalid end_date format. Please provide a date in YYYY-MM-DD format.")
    }

    # Check if start_date is before or equal to end_date
    if (start_date_obj > end_date_obj) {
      cli::cli_abort(c(
        "start_date must be before or equal to end_date",
        "i" = paste("start_date:", format(start_date_obj)),
        "i" = paste("end_date:  ", format(end_date_obj))
      ))
    }

    # Standardize to first day of month
    start_date_standardized <- as.Date(paste0(format(start_date_obj, "%Y-%m"), "-01"))
    end_date_standardized <- as.Date(paste0(format(end_date_obj, "%Y-%m"), "-01"))

    # Check if the day was changed during standardization and print a message
    if (format(start_date_obj, "%d") != "01") {
      .lfs_info(sprintf(
        "start_date day was changed from %s to 01 (first day of month)",
        format(start_date_obj, "%d")
      ))
    }
    if (format(end_date_obj, "%d") != "01") {
      .lfs_info(sprintf(
        "end_date day was changed from %s to 01 (first day of month)",
        format(end_date_obj, "%d")
      ))
    }

    return(list(start_date = start_date_standardized, end_date = end_date_standardized))
  }

  # Get all arguments passed to the function
  args <- list(...)
  lfs_config_list <- args$lfs_config_list

  # If the function is called internally by lfs_table, extract parameters from the config list
  if (!is.null(lfs_config_list) && is.list(lfs_config_list)) {
    date_info <- check_dates_helper(lfs_config_list$start_date, lfs_config_list$end_date)
    start_date <- date_info$start_date
    end_date <- date_info$end_date

    moving_avg <- lfs_config_list$moving_avg %||% 1
    filter_months <- lfs_config_list$filter_months %||% NULL
    filter_years <- lfs_config_list$filter_years %||% NULL
  } else {
    # Otherwise, validate dates from direct user input
    date_info <- check_dates_helper(start_date, end_date)
    start_date <- date_info$start_date
    end_date <- date_info$end_date
  }

  # Ensure moving_avg is at least 1
  moving_avg <- max(1, as.integer(moving_avg))

  # Convert filter_months to numeric if it exists
  filter_months <- if (!is.null(filter_months)) {
    if (is.character(filter_months)) {
      # Use regex pattern ",\\s*" which means "comma followed by zero or more spaces"
      as.integer(strsplit(filter_months, ",\\s*")[[1]])
    } else {
      as.integer(filter_months)
    }
  } else {
    NULL
  }

  # Convert filter_years to character if it exists
  filter_years <- if (!is.null(filter_years)) {
    if (is.character(filter_years)) {
      strsplit(filter_years, ",\\s*")[[1]]
    } else {
      as.character(filter_years)
    }
  } else {
    NULL
  }

  # Get initial sequence including months needed for moving average
  required_months <- seq(as.Date(start_date) - months(moving_avg - 1), as.Date(end_date), by = "month")

  # Apply year filter if specified
  if (!is.null(filter_years)) {
    required_months <- required_months[format(required_months, "%Y") %in% filter_years]
  }

  # Apply month filter if specified
  if (!is.null(filter_months)) {
    # Get the target months (those that match the filter)
    target_months <- required_months[as.integer(format(required_months, "%m")) %in% filter_months]

    # For each target month, get the previous months needed for moving average
    additional_months <- unique(unlist(lapply(target_months, function(d) {
      seq(d - months(moving_avg - 1), d, by = "month")
    })))

    # Combine target and additional months
    required_months <- sort(unique(c(target_months, additional_months)))
  }

  # Ensure we don't go beyond our intended range
  min_date <- min(
    as.Date(start_date) - months(moving_avg - 1),
    as.Date(start_date) - years(1) - months(moving_avg - 1)
  )
  required_months <- required_months[required_months >= min_date &
    required_months <= as.Date(end_date)]

  # Emit the list of required months as a timestamped info line with the
  # dates on a following continuation line (dim). The caller (process_lfs_data)
  # does not re-print this, so this message is authoritative whether the
  # function is called standalone or internally.
.lfs_sub(paste(
  sprintf("Determining required months (%d in total):", length(required_months)),
  cli::col_silver(paste(format(required_months, "%Y-%m"), collapse = ", "))
))

  return(required_months)
}
