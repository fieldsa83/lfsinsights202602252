#' Calculate summaries for a distribution estimate (iterating over levels)
#'
#' @param month_data Data for the month
#' @param estimate Estimate configuration
#' @param current_date Current date
#' @param lfs_config_list Overall configuration
#' @param required_months All required months
#' @return Data frame with stacked summaries for all levels
#' @keywords internal
calculate_distribution_summaries <- function(month_data, estimate, current_date, lfs_config_list, required_months, est_name) {
  
  # 1. Get the variable name
  target_var <- estimate$var_name %||% estimate$ratio_numerator
  
  # 2. Identify levels (including real NAs)
  levels <- unique(month_data[[target_var]])
  
  stacked_results <- list()
  
  for (lvl in levels) {
    # Create a temp config
    temp_est <- estimate
    temp_est$est_type <- "ratio" 
    temp_est$ratio_numerator   <- paste0(target_var, "_", lvl)
    temp_est$ratio_denominator <- "TOTAL"
    
    # 3. Create dummy variables
    # --- FIX: Use %in% instead of == ---
    # %in% handles real NAs correctly:
    #   "A" %in% "A"   -> TRUE
    #   NA  %in% NA    -> TRUE
    #   NA  %in% "A"   -> FALSE
    month_data[[temp_est$ratio_numerator]] <- ifelse(month_data[[target_var]] %in% lvl, 1, 0)
    month_data[["TOTAL"]] <- 1
    
    # 4. Calculate
    lvl_result <- calculate_estimate_summaries(month_data, temp_est, current_date, lfs_config_list, required_months, est_name)
    
    # 5. Add the identifying column 
    # (If lvl is NA, this puts a real NA in the column)
    lvl_result[[target_var]] <- lvl
    
    # --- Fix Column Order ---
    if (length(lfs_config_list$analysis_vars) > 0) {
       last_var <- dplyr::last(lfs_config_list$analysis_vars)
       if (last_var %in% names(lvl_result)) {
         lvl_result <- lvl_result %>%
           dplyr::relocate(!!rlang::sym(target_var), .after = !!rlang::sym(last_var))
       }
    } else {
       lvl_result <- lvl_result %>%
         dplyr::relocate(!!rlang::sym(target_var), .after = "DATE")
    }
    
    stacked_results[[length(stacked_results) + 1]] <- lvl_result
  }
  
  return(dplyr::bind_rows(stacked_results))
}

#' Calculate summaries for a specific estimate on a single month's data
#' Combines configuration, filtering, and aggregation into one efficient step.
#'
#' @param month_data Data for the month
#' @param estimate Estimate configuration
#' @param current_date Current date
#' @param lfs_config_list Overall configuration
#' @param required_months All required months
#' @return Data frame with summaries
#' @keywords internal
calculate_estimate_summaries <- function(month_data, estimate, current_date, lfs_config_list, required_months, est_name) {
  
  # --- STEP 1: PREPARATION & CONFIGURATION ---
  
  # Create a temporary config with the specific estimate settings
  est_config <- lfs_config_list
  est_config$est_type <- estimate$est_type

  # Apply estimate-specific filter if provided
  # (e.g., if this specific estimate is only for "Youth", filter now)
  filtered_data <- month_data
  if (!is.null(estimate$est_filter)) {
    .lfs_sub(paste("Applying estimate filter:", estimate$est_filter))
    filtered_data <- filtered_data %>%
      dplyr::filter(!!rlang::parse_expr(estimate$est_filter))
  }

  # Setup ratio parameters if applicable
  if (estimate$est_type == "ratio") {
    est_config$ratio_numerator   <- estimate$ratio_numerator
    est_config$ratio_denominator <- estimate$ratio_denominator
    est_config$ratio_type        <- estimate$ratio_type %||% "percent"
    est_config$ratio_decimals    <- estimate$ratio_decimals %||% 1
  }

  # --- STEP 2: CALCULATION (Formerly calculate_summaries) ---

  # Emit the "Determining summary stats for: YYYY-MM" top-level beat.
  # The configuration details follow as a dim continuation line (Option A
  # split pattern). If est_type is NULL it effectively means "sum".
  {
    effective_type <- est_config$est_type %||% "sum"
    if (effective_type %in% c("ratio", "ratio_distribution")) {
      details <- paste0(
        "(Type: ", effective_type,
        ", Num: ", est_config$ratio_numerator,
        ", Den: ", est_config$ratio_denominator,
        ", Weight: ", est_config$weight_var,
        ", Bootstraps: ", est_config$bootstraps,
        ")"
      )
    } else {
      details <- paste0(
        "(Type: ", effective_type,
        ", Weight: ", est_config$weight_var,
        ", Bootstraps: ", est_config$bootstraps,
        ")"
      )
    }
.lfs_sub(paste(
  paste0("Determining summary stats for ", "'", est_name, "'"),
  cli::col_silver(details)
))
  }

  # Get summaries definition based on the configured type
  summaries <- determine_summaries_to_generate(est_config)

  .lfs_sub(paste0(
    "Calculating summary levels for '", 
	est_name,
	"' by analysis variables (DATE, ",
    paste0(est_config$analysis_vars, collapse = ", "),
	")" 
  ))

  # Run dplyr group_by and summarise
  # Note: We use 'filtered_data' and 'est_config' prepared in Step 1
  result <- filtered_data %>%
    dplyr::filter(DATE %in% required_months) %>%
    dplyr::group_by(DATE, !!!rlang::syms(est_config$analysis_vars)) %>%
    dplyr::summarise(!!!summaries, .groups = "drop")

  return(result)
}

#' Determine which summaries to generate
#'
#' @param lfs_config_list Configuration list
#' @return List of summarise expressions
#' @keywords internal
determine_summaries_to_generate <- function(lfs_config_list) {
  # Error check for ratio est_type
  effective_type <- lfs_config_list$est_type %||% "sum"
  if (effective_type == "ratio" && (is.null(lfs_config_list$ratio_numerator) ||
    is.na(lfs_config_list$ratio_numerator) ||
    lfs_config_list$ratio_numerator == "" ||
    is.null(lfs_config_list$ratio_denominator) ||
    is.na(lfs_config_list$ratio_denominator) ||
    lfs_config_list$ratio_denominator == "")) {
    cli::cli_abort("When {.field est_type} is set to {.val ratio}, {.field ratio_numerator} and {.field ratio_denominator} must be specified.")
  }

  # Note: the "Determining summary stats" message is emitted by the caller
  # (calculate_estimate_summaries) where current_date is in scope.

  weight_var <- lfs_config_list$weight_var
  summaries <- list()

  # Main weight variable
  if (lfs_config_list$est_type == "sum") {
    summaries[[paste0(weight_var, "_sum")]] <-
      rlang::expr(sum(!!rlang::sym(weight_var), na.rm = TRUE))
  } else if (lfs_config_list$est_type == "ratio") {
    summaries[[paste0(weight_var, "_num")]] <-
      rlang::expr(sum(
        !!rlang::sym(weight_var) * !!rlang::sym(lfs_config_list$ratio_numerator),
        na.rm = TRUE
      ))
    summaries[[paste0(weight_var, "_den")]] <-
      rlang::expr(sum(
        !!rlang::sym(weight_var) * !!rlang::sym(lfs_config_list$ratio_denominator),
        na.rm = TRUE
      ))
  }

  # Bootstrap weights
  if (lfs_config_list$bootstraps) {
    for (i in 1:1000) {
      col_name <- paste0("BW_", weight_var, "_", i)
      if (lfs_config_list$est_type == "sum") {
        summaries[[paste0(col_name, "_sum")]] <-
          rlang::expr(sum(!!rlang::sym(col_name), na.rm = TRUE))
      } else if (lfs_config_list$est_type == "ratio") {
        summaries[[paste0(col_name, "_num")]] <-
          rlang::expr(sum(
            !!rlang::sym(col_name) * !!rlang::sym(lfs_config_list$ratio_numerator),
            na.rm = TRUE
          ))
        summaries[[paste0(col_name, "_den")]] <-
          rlang::expr(sum(
            !!rlang::sym(col_name) * !!rlang::sym(lfs_config_list$ratio_denominator),
            na.rm = TRUE
          ))
      }
    }
  }

  return(summaries)
}
