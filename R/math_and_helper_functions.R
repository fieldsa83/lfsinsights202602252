#' Apply rounding to numeric columns while preserving original values
#'
#' This version stores the unrounded values in new columns with the suffix "_unrounded"
#' before applying rounding. These extra columns can later be removed in clean_final_results().
#'
#' @param data A data frame containing columns to round
#' @param cols_to_round Character vector of column names to round
#' @param rounding_factor Numeric value to round to (e.g., 100 rounds to nearest hundred)
#' @return A data frame with rounded values and additional "_unrounded" columns
#' @export
apply_rounding <- function(data, cols_to_round, rounding_factor) {
  # If no rounding factor is provided or it's zero, return data unchanged
  if (is.null(rounding_factor) || rounding_factor == 0) {
    return(data)
  }

  cat(format(Sys.time(), "%H:%M:%S"), "Applying rounding (to levels) \n")
  
  # Loop through each specified column to round
  for (col_name in cols_to_round) {
    # 1. Store the original (unrounded) values in a new column
    unrounded_col <- paste0(col_name, "_unrounded")
    data[[unrounded_col]] <- data[[col_name]]
    
    # 2. Apply rounding to the original column
    data[[col_name]] <- round(data[[col_name]] / rounding_factor) * rounding_factor
  }
  
  # Return the modified data frame
  return(data)
}


#' Calculate ratios between numerator and denominator columns
#'
#' Calculates ratios (e.g., Unemployment Rate).
#' IMPORTANT: This function creates an "_unrounded" version of the ratio FIRST
#' for variance calculations, then rounds the main ratio column to the specified
#' decimals so that subsequent Change calculations are based on the published (rounded) values.
#'
#' @param data Data frame containing numerator and denominator columns
#' @param num_col Name of numerator column
#' @param den_col Name of denominator column
#' @param ratio_col Name for the output ratio column
#' @param is_percent Whether to multiply by 100 (for percentage)
#' @param decimals Number of decimal places to round to (default 1)
#' @param weight_var Weight variable prefix (used for finding bootstrap columns)
#' @return Data frame with calculated ratio
#' @export
calculate_ratio <- function(data, is_percent = TRUE, decimals = 1, weight_var = "FINALWT") {

  cat(format(Sys.time(), "%H:%M:%S"), "Calculating ratios \n")
  
  # Identify numerator and denominator columns for the main weight variable
  main_num_col <- paste0(weight_var, "_num")
  main_den_col <- paste0(weight_var, "_den")
  main_num_col_unrounded <- paste0(weight_var, "_num_unrounded")
  main_den_col_unrounded <- paste0(weight_var, "_den_unrounded")
  ratio_col_name <- paste0(weight_var, "_ratio")
  
  # 1. Helper Function: Calculate raw ratio
  calc_raw_ratio <- function(num, den) {
    val <- ifelse(den > 0, num / den, NA_real_)
    if (is_percent) val * 100 else val
  }
  
  # 2. Process Main Ratio
  if (main_num_col %in% names(data) && main_den_col %in% names(data)) {
    # A. Calculate regular and unrounded ratios, based on rounded and unrounded num/den, respectively
    raw_val <- calc_raw_ratio(data[[main_num_col]], data[[main_den_col]])
    raw_val_unrounded <- calc_raw_ratio(data[[main_num_col_unrounded]], data[[main_den_col_unrounded]])
    
    # B. Save High Precision for Variance (CVs)
    # The variance function explicitly looks for this "_unrounded" suffix
    data[[paste0(ratio_col_name, "_unrounded")]] <- raw_val_unrounded
    
    # C. Round the Main Column immediately
    # This ensures subsequent "Calculate Change" sees the discrete/rounded number
    data[[ratio_col_name]] <- round(raw_val, decimals)
  }
  
  # 3. Process Bootstrap Ratios (if present)
  # We generally leave these unrounded or round them? 
  # Usually, we leave bootstraps unrounded for better variance precision,
  # but if your variance function calculates SD on the bootstraps, unrounded is better.
  bw_cols <- grep(paste0("^BW_", weight_var, "_\\d+_num$"), names(data), value = TRUE)
  
  if (length(bw_cols) > 0) {
    # Pre-calculate common strings to speed up loop
    bw_prefixes <- sub("_num$", "", bw_cols)
    bw_den_cols <- paste0(bw_prefixes, "_den")
    bw_ratio_cols <- paste0(bw_prefixes, "_ratio")
    
    # Vectorized calculation might be hard due to dynamic names, keeping loop but optimized
    for (i in seq_along(bw_cols)) {
      if (bw_den_cols[i] %in% names(data)) {
        # Bootstrap ratios are kept UNROUNDED for maximum precision in variance calc
        data[[bw_ratio_cols[i]]] <- calc_raw_ratio(data[[bw_cols[i]]], data[[bw_den_cols[i]]])
      }
    }
  }
  
  return(data)
}


#' Calculate period-over-period changes for specified columns
#'
#' @param data Data frame containing time series data
#' @param analysis_vars  Character vector of grouping variables
#' @param lag_period Number of periods to lag
#' @param calculate_percent Whether to calculate percent change
#' @return Data frame with added change columns
#' @export
calculate_change <- function(data, analysis_vars, weight_var, est_type, lag_period, is_percent, calculate_percent = TRUE) {
  cat(format(Sys.time(), "%H:%M:%S"), "Calculating changes \n")

  group_vars <- analysis_vars

  # Check for estimate columns and add them to grouping
  if ("estimate_name" %in% names(data)) {
    group_vars <- c(group_vars, "estimate_name")
  }
  if ("est_type" %in% names(data)) {
    group_vars <- c(group_vars, "est_type")
  }

  # Identify columns to calculate change for and suppression columns to keep
  to_calc_change_cols <- names(data)[!names(data) %in% c("DATE", "Moving_avg", group_vars)]
  lag_cols <- c("DATE", to_calc_change_cols)
  to_calc_change_cols <- to_calc_change_cols[!grepl("_suppress$", to_calc_change_cols)]
  to_calc_change_cols <- to_calc_change_cols[!grepl("_num$", to_calc_change_cols)] # drop num and den, just keep change in ratio or sum
  to_calc_change_cols <- to_calc_change_cols[!grepl("_den$", to_calc_change_cols)] # drop num and den, just keep change in ratio or sum

  pct_change_col <- paste0(weight_var, "_", est_type)

  # Calculate change and keep lag period estimates
  data <- data %>%
    group_by(across(all_of(analysis_vars))) %>%
    dplyr::mutate(
      across(all_of(lag_cols),
        ~ dplyr::lag(.x, lag_period),
        .names = "{.col}_lagperiod"      
      ),
        across(all_of(to_calc_change_cols),
        ~ .x - dplyr::lag(.x, lag_period),
        .names = "{.col}_change"
      )
    ) %>%
    # Only add percent change if not already a percent
    {
      if (est_type != "ratio" || !is_percent) {
        dplyr::mutate(., !!paste0(pct_change_col, "_pct_change") :=
          round((.data[[pct_change_col]] - dplyr::lag(.data[[pct_change_col]], lag_period)) /
            dplyr::lag(.data[[pct_change_col]], lag_period) * 100, 1))
      } else {
        .
      }
    } %>%
    ungroup() 

  # Remove rows where change couldn't be calculated
  data <- data %>%
    dplyr::filter(if_any(ends_with("_change"), ~ !is.na(.))) 

  # Prepare to reorder some columns again -- we want the estimate columns to be in the following order: change (including percent change, if applicable), then current month estimates, then previous month estimates
  change_cols <- grep("_change$", names(data), value = TRUE)
  lagperiod_cols <- grep("_lagperiod$", names(data), value = TRUE)

  # Reorder these columns so that change_cols are first and lagperiod_cols are at the end -- don't worry about the other columns for now, they will be rearranged again in final cleanup
  data <- data %>%
    dplyr::relocate(any_of(change_cols), .before = 1) %>%
    dplyr::relocate(any_of(group_vars), .before = 1) %>%
    dplyr::relocate(any_of(lagperiod_cols), .after = last_col())

  return(data)
}


#' Calculate the difference between comparison categories for the comparison variable
#'
#' @param data Data frame containing time series data
#' @param analysis_vars Character vector of grouping variables (e.g., "GENDER")
#' @param weight_var Character scalar used to form the ratio column names (e.g., "supp_weight")
#' @param est_type Type of estimate, ratio or ratio_distribution
#' @param estimate List with various parameters
#' @param comparison_variable Name of the variable to compare (e.g., "GENDER")
#' @param comparison_categories string of categories to compare (e.g., "1,2")
#'
#' @return A data frame:
#'   - If `comparison_variable` is NULL: original rows with `<measure>_change` and optionally `<measure>_pct_change`.
#'   - If `comparison_variable` is set: one row per DATE + other groups (excluding `comparison_variable`),
#'     with `<weight_var>_ratio_pp_diff` and `BW_..._ratio_pp_diff` for all bootstrap replicates.
#' @export
calculate_difference <- function(
  data,
  analysis_vars,
  weight_var,
  est_type,
  estimate,
  comparison_variable = NULL,
  comparison_categories = NULL
) {
  stopifnot(est_type %in% c("ratio", "ratio_distribution"))
  if (is.null(estimate$ratio_numerator) || !nzchar(estimate$ratio_numerator)) {
    stop("For proportions, `estimate$ratio_numerator` must be supplied.")
  }
  if (!is.null(comparison_variable)) {
    if (!(comparison_variable %in% analysis_vars)) {
      stop("`comparison_variable` must be one of `analysis_vars`.")
    }
  }
    cat(format(Sys.time(), "%H:%M:%S"), "Calculating differences \n")

  #Converts string input for comparison categories to numeric vector
  comparison_categories <- comparison_categories %>%
    str_replace_all("[[:space:]]+", "") %>% 
    str_replace_all("\"", "") %>% 
    str_split(",", simplify = TRUE) %>% 
    as.numeric()

  # Ratio columns
  prop_col <- paste0(weight_var, "_ratio")
  if (!(prop_col %in% names(data))) {
    stop(sprintf("Expected ratio column '%s' not found in `data`.", prop_col))
  }

  # -----------------------
  # Cross-category contrast 
  # -----------------------
  ctgry1 <- comparison_categories[1]
  ctgry2 <- comparison_categories[2]

  # Keep only the two levels to be contrasted
  data <- data %>%
    dplyr::filter(.data[[comparison_variable]] %in% comparison_categories)

  # Identify columns to calculate difference for
  group_vars <- analysis_vars
  if ("DATE" %in% names(data)) group_vars <- c(group_vars, "DATE")
  if ("Moving_avg" %in% names(data)) group_vars <- c(group_vars, "Moving_avg")
  if ("estimate_name" %in% names(data)) group_vars <- c(group_vars, "estimate_name")
  if ("est_type" %in% names(data)) group_vars <- c(group_vars, "est_type")
   
  # remove the categorical variable
  group_vars_no_cat <- group_vars <- setdiff(group_vars, comparison_variable)

  # Identify columns to calculate difference for
  diff_cols <- names(data)[!names(data) %in% c("DATE", "Moving_avg", group_vars, comparison_variable)]
  diff_cols <- diff_cols[!grepl("_suppress$", diff_cols)]
  diff_cols <- diff_cols[!grepl("_num$", diff_cols)]
  diff_cols <- diff_cols[!grepl("_den$", diff_cols)]

  # Get names of columns for numerator and denominator
  num_col <- paste0(weight_var, "_num")
  den_col <- paste0(weight_var, "_den")

  # Make a list of columns we want to keep for both ctgry1 and ctgry2
  suppress_cols <- grep("suppress$", names(data), value = TRUE)
  cols_to_carry <- intersect(c(prop_col, num_col, den_col, suppress_cols), names(data))  # only carry if present

  # calculate difference
  data <- data %>%
    group_by(across(all_of(group_vars_no_cat))) %>%
      # keep rounded ratios, numerators, and denominators for ctgry1 and ctgry2
      summarise(
        across(all_of(cols_to_carry),
        ~ {
          comp <- .data[[comparison_variable]]
          v1 <- .x[comp == ctgry1]
          if (length(v1) == 0L) NA_real_ else v1[1L]
        },
        .names = "{.col}_ctgry1"
      ),
      dplyr::across(
        dplyr::all_of(cols_to_carry),
        ~ {
          comp <- .data[[comparison_variable]]
          v2 <- .x[comp == ctgry2]
          if (length(v2) == 0L) NA_real_ else v2[1L]
        },
        .names = "{.col}_ctgry2"
      ),

        #calculate difference
        across(all_of(diff_cols),  
          ~ {
            v    <- .x
            comp <- .data[[comparison_variable]]
            v1   <- v[comp == ctgry1]
            v2   <- v[comp == ctgry2]
            if (length(v1) == 0L || length(v2) == 0L) NA_real_ else (v1[1L] - v2[1L])
          },
          .names = "{.col}_diff"
      ),
     .groups = "drop"
    )
      
  #Create variable with information on what variable/values were compared
  comp_label <- paste0("Comparison category 1: ", comparison_variable, "=", ctgry1, " vs. Comparison category 2: ", comparison_variable, "=", ctgry2)
  
  # add this variable, drop some unnecessary variables, and reorder
  data <- data %>%
    dplyr::mutate(comparison_categories = comp_label) %>%
    dplyr::select(group_vars_no_cat, comparison_categories, paste0(prop_col, "_diff"), everything())

  return(data)
}
  
  
#' Add suppression flags to columns
#'
#' @param data Data frame containing value columns
#' @param cols_to_check Character vector of column names to check
#' @param has_prov Whether PROV column exists for province-specific thresholds
#' @return Data frame with added suppression flags
#' @export
add_suppression_flag <- function(data, cols_to_check, has_prov = TRUE) {

  cat(format(Sys.time(), "%H:%M:%S"), "Applying suppression flags \n")
  
  # Function to apply suppression rules
  apply_suppression <- function(col, prov) {
    result <- dplyr::case_when(
      prov %in% c("11", "60", "61", "62") ~ col < 200,
      prov %in% c("10", "12", "13", "46", "47") ~ col < 500,
      TRUE ~ col < 1500 # Default case for other provinces
    )
    return(as.logical(result))
  }

  # Apply suppression rules
  if (has_prov && "PROV" %in% names(data)) {
    for (col in cols_to_check) {
      flag_col_name <- paste0(col, "_suppress")
      data[[flag_col_name]] <- apply_suppression(data[[col]], data$PROV)
    }
  } else {
    for (col in cols_to_check) {
      flag_col_name <- paste0(col, "_suppress")
      data[[flag_col_name]] <- as.logical(data[[col]] < 1500)
    }
  }

  return(data)
}


#' Calculate moving average
#'
#' @param data Data frame containing time series data
#' @param group_vars Character vector of grouping variables
#' @param value_cols Character vector of columns to average
#' @param periods Number of periods for moving average
#' @param filter_months Optional numeric vector of months to filter
#' @param filter_years Optional character vector of years to filter
#' @return Data frame with moving averages
#' @export
calculate_moving_avg <- function(data, group_vars, value_cols, periods, filter_months = NULL, filter_years = NULL, lfs_config_list) {

  cat(format(Sys.time(), "%H:%M:%S"), "Calculating moving averages \n")


  # Create target dates from the date range
  target_dates <- unique(data$DATE)

  # Apply month filter if specified
  if (!is.null(filter_months)) {
    if (is.character(filter_months)) {
      filter_months <- as.integer(unlist(strsplit(filter_months, ",\\s*")))
    }
    target_dates <- target_dates[as.integer(format(target_dates, "%m")) %in% filter_months]
  }

  # Apply year filter if specified
  if (!is.null(filter_years)) {
    if (is.character(filter_years) && length(filter_years) == 1) {
      filter_years <- unlist(strsplit(filter_years, ",\\s*"))
    }
    target_dates <- target_dates[format(target_dates, "%Y") %in% filter_years]
  }
  
  # Capture external logical flags from lfs_config_list
  supplement_lmsi <- lfs_config_list$supplement_lmsi
  supplement_dlmi <- lfs_config_list$supplement_dlmi

  # Calculate moving averages
  result <- data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) %>%
    dplyr::arrange(DATE) %>%
    # Add technical columns for calculating consecutive periods
    dplyr::mutate(
      year_month = lubridate::floor_date(DATE, "month"),
      month_diff = as.numeric(difftime(year_month, dplyr::lag(year_month), units = "days")) / 30.44,
      is_consecutive = month_diff <= 2 | is.na(month_diff),
      group_id = cumsum(!is_consecutive)
    ) %>%
    dplyr::group_by(group_id, .add = TRUE) %>%
    dplyr::mutate(
      consecutive_count = dplyr::row_number(),
      dplyr::across(
      dplyr::all_of(value_cols),
      ~ if (dplyr::n() >= periods) {
	     #For LMSI and DLMI, calculate a rolling sum instead of a rolling mean for LFS and LMI
        if (supplement_lmsi | supplement_dlmi) {
          zoo::rollsumr(.x, k = periods, fill = NA, align = "right")
        } else {
          zoo::rollmeanr(.x, k = periods, fill = NA, align = "right")
        }
      } else {
        NA_real_
      }
    )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(consecutive_count >= periods) %>%
    # Only keep rows where DATE matches target_dates
    dplyr::filter(DATE %in% target_dates) %>%
    dplyr::mutate(Moving_avg = periods) %>%
    dplyr::select(DATE, Moving_avg, dplyr::everything(), -year_month, -month_diff, -is_consecutive, -group_id, -consecutive_count)

  return(result)
}


#' @title Complete Dataframe Combinations
#'
#' @description Ensures that a dataframe contains rows for all combinations of DATE
#'              and specified analysis variables within the range of the data.
#'              Implicitly missing combinations are made explicit by adding rows
#'              where specified value columns are filled, typically with 0.
#'              This is crucial before performing operations like moving averages,
#'              lag calculations, or marginal totals on grouped time series data.
#'
#' @importFrom dplyr %>% mutate across all_of arrange filter if_else group_by ungroup n
#' @importFrom tidyr complete nesting
#' @importFrom rlang syms
#' @importFrom stats setNames
#' @importFrom utils str
#' @importFrom lubridate is.Date
#'
#' @export
complete_combinations <- function(data, analysis_vars, weight_var) {
  # Check if there are analysis variables to complete by; return sorted data if none
  if (length(analysis_vars) == 0 || all(analysis_vars == "")) {
    cat(format(Sys.time(), "%H:%M:%S"), "Skipping combination completion: No analysis variables specified.", "\n")
    if (nrow(data) > 0) {
      data <- data %>% dplyr::arrange(DATE)
    }
    return(data)
  }

  # Check if data is empty
  if (nrow(data) == 0) {
    cat("Error: Skipping combination completion. Input data frame is empty.", "\n")
    return(data) # Return the empty dataframe
  }

  # cat(format(Sys.time(), "%H:%M:%S"), "Completing combinations for DATE and analysis variables", "\n")

  # Dynamically find value columns (main weight + bootstrap weights for sum/num/den)
  # Ensure weight_var is valid before constructing pattern
  if (is.null(weight_var) || weight_var == "") {
    stop("Error in complete_combinations: weight_var is missing or empty")
  }
  value_cols_pattern <- paste0("^(", weight_var, "|BW_", weight_var, "_\\d+)_(sum|num|den)$")
  value_cols_to_fill <- grep(value_cols_pattern, names(data), value = TRUE)

  if (length(value_cols_to_fill) == 0) {
    warning("Warning in complete_combinations: No value columns matching the pattern found to fill. Returning original data.")
    # Still sort the data
    data <- data %>% dplyr::arrange(DATE, dplyr::across(dplyr::all_of(analysis_vars)))
    return(data)
  }

  # Create the fill list (filling NAs with 0 for numeric cols)
  # Note: This assumes 0 is the correct fill value for these numeric summaries.
  fill_list <- stats::setNames(rep(list(0), length(value_cols_to_fill)), value_cols_to_fill)

  # Identify grouping columns (DATE + analysis_vars)
  # Check for estimate columns and add them to the combinations

  if ("estimate_name" %in% names(data)) {
    analysis_vars <- c(analysis_vars, "estimate_name")
  }
  if ("est_type" %in% names(data)) {
    analysis_vars <- c(analysis_vars, "est_type")
  }
  grouping_cols <- c("DATE", analysis_vars)

  # Check for non-numeric types in grouping columns before completing
  non_numeric_group_cols <- grouping_cols[!sapply(data[, grouping_cols, drop = FALSE], is.numeric) & !sapply(data[, grouping_cols, drop = FALSE], lubridate::is.Date)]
  if (length(non_numeric_group_cols) > 0) {
    # Ensure character or factor columns are handled correctly by complete
    data <- data %>%
      dplyr::mutate(dplyr::across(dplyr::all_of(non_numeric_group_cols), as.character))
  }


  # Apply tidyr::complete
  # Uses nesting() to only complete combinations of analysis_vars that actually exist in the input data
  completed_data <- data %>%
    tidyr::complete(
      DATE,
      tidyr::nesting(!!!rlang::syms(analysis_vars)),
      fill = fill_list
    ) %>%
    # Re-arrange after completing; essential for subsequent lagged/rolling operations
    # Ensure correct handling if analysis_vars is empty/null checked above
    dplyr::arrange(DATE, dplyr::across(dplyr::all_of(analysis_vars)))

  cat(format(Sys.time(), "%H:%M:%S"), "Ensuring complete combinations by DATE and analysis vars. Input rows: ", nrow(data), " Output rows: ", nrow(completed_data), "\n")

  # Optional: Add a check for unexpected row explosion
  if (nrow(completed_data) > nrow(data) * 2 && nrow(data) > 0) { # Heuristic check
    warning("Warning in complete_combinations: Row count increased significantly (more than double). Check analysis variables and data sparsity.")
    # Consider adding glimpse(data) and glimpse(completed_data) here for debugging if needed
    # utils::str(data)
    # utils::str(completed_data)
  }


  return(completed_data)
}


#' Calculate bootstrap variance
#'
#' Calculates variance, CVs, and confidence intervals using bootstrap weights.
#' Automatically determines the correct columns to use based on estimate type.
#' Uses unrounded columns for calculation if they exist (suffix "_unrounded") for maximum precision.
#'
#' @param data Data frame containing estimate and bootstrap weights
#' @param est_type Type of estimate ("ratio" or "sum")
#' @param weight_var The base weight variable name (e.g., "FINALWT")
#' @param is_change Whether calculating for change values (adds Significance flags)
#' @param is_diff Whether calculating for difference values (adds Significance flags)
#' @return Data frame with bootstrap statistics added and raw bootstrap columns removed
#' @export
calculate_bootstrap_variance <- function(data, est_type, weight_var, is_change = FALSE, is_diff = FALSE) {

  cat(format(Sys.time(), "%H:%M:%S"), "Calculating bootstrap variance \n")
  
  # =========================================================================
  # 1. SETUP: Determine Column Names
  # =========================================================================
  
  # Determine the suffix based on estimate type and change status
  # Example suffixes: "_sum", "_ratio", "_sum_change"
  suffix <- if (est_type == "ratio") "_ratio" else "_sum"
  
  if (is_change) {
    suffix <- paste0(suffix, "_change")
  } else if (is_diff) {
    suffix <- paste0(suffix, "_diff")
  }
  
  # Construct the expected main variable name
  main_var <- paste0(weight_var, suffix)
 
  
  # Construct regex pattern to find matching bootstrap columns
  # Looks for: Start with BW_, match weight var, any digits, match suffix, End of string
  bw_pattern <- paste0("^BW_", weight_var, "_\\d+", suffix, "$")
  
  # Find the actual bootstrap columns in the data
  bw_vars <- grep(bw_pattern, names(data), value = TRUE)
  
  
  # =========================================================================
  # 2. VALIDATION
  # =========================================================================
  
  # If no bootstrap columns found or main variable missing, return unchanged
  if (length(bw_vars) == 0) {
    return(data)
  }
  
  if (!main_var %in% names(data)) {
    # If main var is missing, we can't calculate CVs relative to it
    return(data)
  }
  
  
  # =========================================================================
  # 3. PRECISION HANDLING
  # =========================================================================
  
  # Determine which column to use for the denominator in CV calculation:
  # Prefer unrounded version if it exists to avoid "rounding error on rounding error"
   main_var_to_use <- if (grepl("_change$", main_var)) {     #test to see if main_var ends in _change, and then return the corresponding _unrounded_change
    main_var_uc <- sub("_change$", "_unrounded_change", main_var)
    if (main_var_uc %in% names(data)) main_var_uc else main_var
    } else if (grepl("_diff$", main_var)) {     #test to see if main_var ends in _change, and then return the corresponding _unrounded_change
    main_var_ud <- sub("_diff$", "_unrounded_diff", main_var)
    if (main_var_ud %in% names(data)) main_var_ud else main_var
    } else if (paste0(main_var, "_unrounded") %in% names(data)) {  #if not a change variable, search for and return the _unrounded version
      paste0(main_var, "_unrounded")
    } else {
      main_var
    }


  # =========================================================================
  # 4. CALCULATION
  # =========================================================================
  
  result <- data %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      # A. Bootstrap Standard Deviation (SD)
      # Formula: sqrt( (N-1)/N * sum((x_i - mean)^2) ) -> approximated by var() logic
      bs_sd = sqrt(((length(bw_vars) - 1) *
                      stats::var(dplyr::c_across(dplyr::all_of(bw_vars)), na.rm = TRUE) /
                      length(bw_vars))),
      
      # B. Coefficient of Variation (CV)
      # Calculated against the unrounded main estimate
      bs_cv = abs(bs_sd / .data[[main_var_to_use]]) * 100,
      
      # C. Confidence Intervals (95%)
      # Type 2 quantile is standard for SAS compatibility in some contexts, but default is 7 in R
      bs_lb_95 = stats::quantile(dplyr::c_across(dplyr::all_of(bw_vars)), 0.025, na.rm = TRUE, type = 2),
      bs_ub_95 = stats::quantile(dplyr::c_across(dplyr::all_of(bw_vars)), 0.975, na.rm = TRUE, type = 2)
    ) %>%
    
    # D. Significance Testing (Only if calculating Change)
    {
      if (is_change||is_diff) {
        dplyr::mutate(.,
                      Significant68 = abs(.data[[main_var_to_use]]) >= abs(bs_sd),
                      Significant95 = abs(.data[[main_var_to_use]]) >= abs(bs_sd * 1.96)
        )
      } else {
        .
      }
    } %>%
    
    dplyr::ungroup() %>%
    
    # E. Formatting and Cleanup
    dplyr::mutate(dplyr::across(
      .cols = c(bs_sd, bs_cv, bs_lb_95, bs_ub_95),
      .fns = ~ round(., 2)
    )) %>%
    # Remove the raw bootstrap columns to keep the output light
    dplyr::select(-dplyr::starts_with("bw", ignore.case = TRUE))
  
  return(result)
}


#' Calculate marginal totals
#'
#' @param data A data frame containing the data to analyze
#' @param analysis_vars A character vector of column names to use for marginal totals
#' @return A data frame with the original data plus additional rows for marginal totals
#' @export
calculate_marginal_totals <- function(data, analysis_vars, est_config = NULL) {

  # 1. Clean analysis_vars immediately (Remove empty strings like "")
  # This ensures proper length calculation below
  analysis_vars <- analysis_vars[analysis_vars != "" & !is.na(analysis_vars)]

  # 2. Early Exit Check (Your Logic)
  # If no variables OR (only 1 variable AND it is a distribution), stop here.
  if (length(analysis_vars) == 0) {      
      cat(format(Sys.time(), "%H:%M:%S"), "No marginal totals to calculate\n")
      return(data)
  }
  
  # Check for estimate columns and preserve them during totals calculation
  preserve_cols <- c()
  if ("estimate_name" %in% names(data)) {
    preserve_cols <- c(preserve_cols, "estimate_name")
  }
  if ("est_type" %in% names(data)) {
    preserve_cols <- c(preserve_cols, "est_type")
  }

# --- FIX FOR DISTRIBUTION TOTALS ---
  # If this is a ratio_distribution, we must NEVER sum across the distribution variable ,
  # because the denominators are repeated and summing them creates garbage (e.g. 300% population).
  # We move the variable from 'analysis_vars' (summable) to 'preserve_cols' (fixed).
  if (!is.null(est_config) && 
      !is.null(est_config$est_type) && 
      est_config$est_type == "ratio_distribution") {
      
      # Determine the variable name
		dist_var <- est_config$var_name %||% est_config$ratio_numerator
		      
      # If it is currently in the analysis list, move it to the preserve list
      if (dist_var %in% analysis_vars) {
          analysis_vars <- setdiff(analysis_vars, dist_var)
          preserve_cols <- c(preserve_cols, dist_var)
      }
  }
  # -----------------------------------
  
  # Combine analysis vars with preserve columns for grouping
  group_vars <- c(analysis_vars, preserve_cols)

  # Convert all analysis cols to character to avoid them turning into sums
  data <- data %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(analysis_vars), as.character))


  all_vars <- c("DATE", group_vars)
  
  
  if (length(analysis_vars) == 0 && !is.null(est_config) && est_config$est_type == "ratio_distribution") {
	
     cat(format(Sys.time(), "%H:%M:%S"), "No marginal totals to calculate. Sorting distribution categories \n")
	 data <- data %>% dplyr::arrange(dplyr::across(dplyr::all_of(all_vars)))
     return(data)
  } 
  else {
    cat(format(Sys.time(), "%H:%M:%S"), "Calculating marginal totals.", "\n")
  # Generate marginal totals, preserving estimate information
  data_marginals <- purrr::map(0:(length(analysis_vars) - 1), ~ {
    combinations <- utils::combn(analysis_vars, .x, simplify = FALSE)
    purrr::map_dfr(combinations, ~ {
      # Always include DATE and preserve_cols in the grouping
      current_group_vars <- c("DATE", preserve_cols, .x)

      data %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(current_group_vars))) %>%
        dplyr::summarise(dplyr::across(where(is.numeric), sum), .groups = "drop") %>%
        tibble::add_column(!!!stats::setNames(
          rep(list("Total"), length(setdiff(all_vars, current_group_vars))),
          setdiff(all_vars, current_group_vars)
        ))
    })
  })

  data <- dplyr::bind_rows(data, data_marginals) %>%
    dplyr::arrange(dplyr::across(dplyr::all_of(all_vars)))

  return(data)
  }
}


#' Clean final results
#'
#' Adds some final touches, such as reordering columns to make sure DATE is first, as well as deleting any unrounded raw data
#' @param data Data frame to clean
#' @return Cleaned data frame
#' @export
clean_final_results <- function(data) {
  # Handle estimate name/type columns based only on data content
  has_single_estimate <- FALSE

  #  Check if there's a single, unnamed estimate based on data content
  if ("estimate_name" %in% names(data)) {
    unique_estimates <- unique(data$estimate_name)
    if (length(unique_estimates) == 1 &&
      (unique_estimates[1] == "" || unique_estimates[1] == "est1")) {
      has_single_estimate <- TRUE
    }
  }

  # Process estimate columns based on our determination
  if (has_single_estimate) {
    # Single unnamed estimate - remove these columns
    data <- data %>%
      dplyr::select(-dplyr::any_of(c("estimate_name")))
  } else {
    # Multiple or named estimates - rename to uppercase if they exist
    if ("estimate_name" %in% names(data)) {
      data <- data %>% dplyr::rename(ESTIMATE_NAME = estimate_name)
    }
    if ("est_type" %in% names(data)) {
      data <- data %>% dplyr::rename(EST_TYPE = est_type)
    }
  }

  # Remove all "_unrounded" columns
  data <- data %>% dplyr::select(-dplyr::contains("_unrounded"))

  # Define lists of columns for reordering
  standard_cols <- c("DATE", "Moving_avg", "DATE_lagperiod", "ESTIMATE_NAME", "EST_TYPE")
  bs_sig_cols <- c("bs_sd", "bs_cv", "bs_lb_95", "bs_ub_95", "Significant68", "Significant95")
  suppress_cols <- grep("suppress", names(data), value = TRUE)
  end_cols <- c(bs_sig_cols, suppress_cols)

  # Reorder columns - move standard cols to the front and bootstrap/significance/suppression cols to the end.
  data <- data %>%
    dplyr::relocate(any_of(standard_cols), .before = 1) %>%
    dplyr::relocate(any_of(end_cols), .after = last_col())

  return(data)
} 


#' Null coalescing operator
#'
#' @param x First value
#' @param y Default value if x is NULL
#' @return x if not NULL, otherwise y
#' @keywords internal
`%||%` <- function(x, y) if (is.null(x)) y else x
