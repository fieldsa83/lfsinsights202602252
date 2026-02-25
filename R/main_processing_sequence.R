#' Process LFS data with multiple estimates
#'
#' @description
#' The main orchestration engine for the package. It manages the timeline loop,
#' aggregates data across months, and applies statistical post-processing.
#'
#' @param lfs_config_list Configuration list containing parameters for data processing
#' @return A data frame with processed LFS data
#' @keywords internal
process_lfs_data <- function(lfs_config_list) {
    
  # =========================================================================
  # PHASE 1: SETUP & PRE-FLIGHT CHECKS
  # =========================================================================
  
  # Start timer
  if (requireNamespace("tictoc", quietly = TRUE)) {
    tictoc::tic()
  }
  
  # Validate required parameters
  if (missing(lfs_config_list)) {
    stop("lfs_config_list is required")
  }
  
  # Check dates and create list of required dates based on ranges, filters and moving averages
  required_months <- determine_required_months(lfs_config_list$start_date, lfs_config_list$end_date, lfs_config_list = lfs_config_list)
  
  # Special case for microdata --skips the rest of this script
  if (lfs_config_list$microdata_only) {
    return(process_microdata(lfs_config_list, required_months))
  }
  
  # Script continues if not microdata
  # Make sure estimates is provided and is a list
  if (is.null(lfs_config_list$estimates) || !is.list(lfs_config_list$estimates)) {
    stop("The estimates parameter is required and must be a list")
  }
  
  # Clean analysis vars (if string with commas, split it)
  if (is.character(lfs_config_list$analysis_vars) && length(lfs_config_list$analysis_vars) == 1) {
    lfs_config_list$analysis_vars <- unlist(strsplit(lfs_config_list$analysis_vars, ", "))
  }
  
  # Initialize custom DVs if applicable
  if (lfs_config_list$add_custom_dvs) {
    load_custom_dvs()
  }
  
  
  # =========================================================================
  # PHASE 2: EXECUTION LOOP (read in and calculate estimate(s) one month at a time)
  # Note: this does not include ratios or moving averages yet --this is simply levels (num and den) on monthly file
  # =========================================================================
  
  # Create a list to store each estimate's results
  # Each element will be a list of data frames (one per month)
  estimate_results <- vector("list", length(lfs_config_list$estimates))
  
  # Name each estimate slot
  for (i in seq_along(lfs_config_list$estimates)) {
    estimate <- lfs_config_list$estimates[[i]]
    # Use provided name or generate a sequential name
    est_name <- if (!is.null(estimate$est_name) && estimate$est_name != "") {
      estimate$est_name
    } else {
      paste0("est", i)
    }
    names(estimate_results)[i] <- est_name
    # Initialize as empty list to hold months
    estimate_results[[i]] <- list()
  }
  
  # Process each month one at a time
  for (current_date in required_months) {
    current_date <- as.Date(current_date)
    tryCatch(
      {
        # Load data for this month (applies global filter)
        month_data <- load_month_data(current_date, lfs_config_list)
        
        # Process each estimate for this month
        for (i in seq_along(lfs_config_list$estimates)) {
          estimate <- lfs_config_list$estimates[[i]]
          
          # Generate estimate name consistently
          est_name <- if (!is.null(estimate$est_name) && estimate$est_name != "") {
            estimate$est_name
          } else {
            paste0("est", i)
          }
          
          # Determine if estimate type is ratio distribution (ie needs auto dummy vars) or standard var
          if (!is.null(estimate$est_type) && estimate$est_type == "ratio_distribution") {
             # Call ratio distribution calculation
             est_result <- calculate_distribution_summaries(month_data, estimate, current_date, lfs_config_list, required_months)
             
          } else {
             # Call the standard function (Sum or standard Ratio)
             est_result <- calculate_estimate_summaries(month_data, estimate, current_date, lfs_config_list, required_months)
          }
          # -------------------------------------------
          
          # Add estimate identification columns
          est_result$estimate_name <- est_name
          est_result$est_type <- estimate$est_type
          
          # Store result in the appropriate list slot
          estimate_results[[i]][[length(estimate_results[[i]]) + 1]] <- est_result
        }
        
        # Discard raw data to free memory
        rm(month_data)
        gc()
      },
      error = function(e) {
        warning("Failed to process data for ", format(current_date, "%Y-%m"), ": ", e$message)
      }
    )
  }
  
  cat(format(Sys.time(), "%H:%M:%S"), "*** All months loaded, beginning post-processing ***\n")
  
  
  # =========================================================================
  # PHASE 3: POST-PROCESS EACH ESTIMATE SEPARATELY (once all months are read in)
  # =========================================================================
  
  # Post-process each estimate separately
  final_results <- list()
  
  for (i in seq_along(estimate_results)) {
    est_name <- names(estimate_results)[i]
    
    # Use the index to get the correct estimate configuration
    est_config <- lfs_config_list$estimates[[i]]
    
    # Combine all months for this estimate
    est_months <- estimate_results[[i]]
    
    if (length(est_months) == 0) {
      warning("No data found for estimate: ", est_name)
      next
    }
    
    est_data <- dplyr::bind_rows(est_months)

  # Post-process this estimate
  cat(format(Sys.time(), "%H:%M:%S"), "Post-processing estimate:", est_name, "\n")
  
  # --- LOGIC FROM post_process_estimate BEGINS HERE ---
  
  # Extract analysis parameters
  analysis_vars <- lfs_config_list$analysis_vars
  
    # If this is a distribution, we MUST add the variable to analysis_vars locally. This ensures complete_combinations fills missing months correctly.
    if (est_config$est_type == "ratio_distribution") {
        dist_var <- est_config$var_name %||% est_config$ratio_numerator
        analysis_vars <- unique(c(analysis_vars, dist_var))
    }
	
  # Identify weight variables
  weight_var <- lfs_config_list$weight_var
  moving_avg <- lfs_config_list$moving_avg
  
  # Identify value columns
  value_cols <- grep(weight_var, names(est_data), value = TRUE)
  
  # 1. Complete combinations
  est_data <- complete_combinations(est_data, lfs_config_list$analysis_vars, lfs_config_list$weight_var)
  
  # 2. Include marginal totals if requested
  if (!is.null(lfs_config_list$include_marginals) && lfs_config_list$include_marginals) {
    est_data <- calculate_marginal_totals(est_data, analysis_vars, est_config)
  }
  
  # 3. Apply moving average if requested
  if (!is.null(moving_avg) && moving_avg > 1) {
    est_data <- calculate_moving_avg(
      est_data,
      analysis_vars,
      value_cols,
      moving_avg,
      lfs_config_list$filter_months,
      lfs_config_list$filter_years,
	  lfs_config_list
    )
  }
  
  # 4. Apply suppression flag if requested
  if (!is.null(lfs_config_list$add_suppression_flag) && lfs_config_list$add_suppression_flag) {
    est_data <- add_suppression_flag(est_data, value_cols[!grepl("^BW_", value_cols)], TRUE)
  }
  
  # 5. Apply rounding to levels (including numerator and denominator --but not the ratio itself)
  if (!is.null(lfs_config_list$weight_rounding) && lfs_config_list$weight_rounding > 0) {
    cols_to_round <- value_cols[!grepl("BW_|_suppress$", value_cols)]
    est_data <- apply_rounding(est_data, cols_to_round, lfs_config_list$weight_rounding)
  }
  
  # 6. Calculate ratio 
  if (est_config$est_type %in% c("ratio", "ratio_distribution")) {
    is_percent <- (est_config$ratio_type %||% "percent") == "percent"
    # Grab decimals from config or default to 1
    decimals   <- est_config$ratio_decimals %||% 1
    
    est_data <- calculate_ratio(
      data = est_data,
      is_percent = is_percent,
      decimals = decimals, 
      weight_var = weight_var
    )
  }
  
  # 7. Calculate change if requested
	if (!is.null(lfs_config_list$calculate_change) && lfs_config_list$calculate_change) {
      # Treat distribution as a ratio for change calc
      type_for_change <- if(est_config$est_type == "ratio_distribution") "ratio" else est_config$est_type
      is_percent <- (est_config$ratio_type %||% "percent") == "percent"
      
      est_data <- calculate_change(
        est_data,
        analysis_vars, # <--- Uses the LOCAL variable we updated above
        lfs_config_list$weight_var,
        type_for_change,
        lfs_config_list$lag_period %||% 1,
        is_percent,
        TRUE
      )
    }

  # 8. Calculate difference between two categories for comparison variable if requested
  if (!is.null(lfs_config_list$calculate_difference) && lfs_config_list$calculate_difference) {
      est_data <- calculate_difference(
        est_data,
        lfs_config_list$analysis_vars,
        lfs_config_list$weight_var,
        estimate$est_type,
        estimate,
        lfs_config_list$comparison_variable,
        lfs_config_list$comparison_categories
      )
  }
  
  # 9. Calculate bootstrap variance if requested
	if (isTRUE(lfs_config_list$bootstraps)) {
      # Treat distribution as a "ratio" for variance calculation
      type_for_var <- if(est_config$est_type == "ratio_distribution") "ratio" else est_config$est_type
      
      est_data <- calculate_bootstrap_variance(
        data = est_data, 
        est_type = type_for_var, 
        weight_var = lfs_config_list$weight_var,
        is_change = isTRUE(lfs_config_list$calculate_change),
        is_diff = isTRUE(lfs_config_list$calculate_difference)
      )
    }


  
  # --- LOGIC FOR EACH ESTIMATE IN LIST ENDS HERE, RESULT IS est_data ---
  
  # Add to final results
  final_results[[length(final_results) + 1]] <- est_data
  }
  
  # Combine all processed estimates into a single dataframe
  if (length(final_results) == 0) {
    stop("No estimates were post-processed")
  }
  
  
  # =========================================================================
  # PHASE 4: FINAL CLEANUP
  # =========================================================================
  
  if (length(final_results) == 0) {
    stop("No estimates were post-processed")
  }

  # Bind all estimate dataframes together
  final_data <- dplyr::bind_rows(final_results)
  
  # Apply labels if requested
  if (!is.null(lfs_config_list$add_labels) && lfs_config_list$add_labels) {
    final_data <- label_maker(final_data, lfs_config_list$language %||% "EN")
  }
  
  # Final cleaning (column ordering and dropping of temp vars)
   final_data <- clean_final_results(final_data)
  
  # End timer
  timer_result <- tictoc::toc(quiet = TRUE)
  cat(format(Sys.time(), "%H:%M:%S"), paste0("Completed in ", format(timer_result$toc - timer_result$tic, nsmall = 1), " secs \n"))
  
  return(final_data)
}