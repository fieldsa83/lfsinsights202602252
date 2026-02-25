#' @importFrom vroom vroom_fwf fwf_widths
#' @importFrom haven read_sas
#' @importFrom dplyr filter mutate select rename_with starts_with %>%
#' @keywords internal

#' Load TABS data for a specific month
#'
#' @title Load TABS Data
#' @description Loads TABS data for a specific month from the appropriate data source.
#'
#' @param current_date Date to process
#' @param use_prerelease Whether to use pre-release data
#' @param lfs_config_list Configuration list
#' @return Data frame with TABS data
#' @keywords internal
load_tabs_data <- function(current_date, use_prerelease = FALSE, lfs_config_list) {
  # Show progress
  cat(format(Sys.time(), "%H:%M:%S"), "-Loading LFS TABS for", format(current_date, "%Y%m"), "\n")

  # Format the filename using 2-digit month and 2-digit year
  file_name <- paste0("tab", format(current_date, "%m%y"), ".prn")

  file_path <- if (use_prerelease) {
    file.path(get_config("lfs_prerelease_path"), "LFS-Tabs", file_name)
  } else {
    file.path(get_config("lfs_client_data_path"), "LFS-Tabs", file_name)
  }

  # Get tabs layout from package environment
  tabs_layout <- get_config("tabs_layout")

  # Read in PRN fixed-weight-file according to the tabs layout
  data <- vroom::vroom_fwf(
    file_path,
    vroom::fwf_widths(tabs_layout$Length, tabs_layout$Name),
    col_types = paste(substr(tabs_layout$FORMAT, 1, 1), collapse = ""),
    .name_repair = "minimal"
  )

  # Add DATE and HHLDID
  data <- data %>%
    dplyr::mutate(
      DATE = current_date,
      HHLDID = as.character(paste(PROV, PROV1, PSEUDOUI, FRAME, STRAFRAM, TYPE, CLUST, ROTATION, LISTLINE, MULT, sep = ""))
    )

  if (is.null(data) | nrow(data) <1) {
    stop("Error in processing TABS.")
  }

  return(data)
}

#' Load TABS Plus data for a specific month
#'
#' @param current_date Date to process
#' @param use_prerelease Whether to use pre-release data
#' @return Data frame with TABS Plus data or NULL if not found
#' @keywords internal
load_plus_data <- function(current_date, use_prerelease = FALSE) {
  cat(format(Sys.time(), "%H:%M:%S"), "-Loading TABPLUS for", format(current_date, "%Y%m"), "\n")

  # Try both common variants
  variants <- c(
    paste0("tabplus_", format(current_date, "%Y%m"), ".sas7bdat"),
    paste0("Tabplus_", format(current_date, "%Y%m"), ".sas7bdat")
  )

  base_path <- if (use_prerelease) {
    file.path(get_config("lfs_prerelease_path"), "LFS-Tabplus")
  } else {
    file.path(get_config("lfs_client_data_path"), "LFS-Tabs")
  }

  # Try each variant
  for (variant in variants) {
    file_path <- file.path(base_path, variant)
    if (file.exists(file_path)) {
      return(haven::read_sas(file_path) %>% dplyr::mutate(DATE = current_date))
    }
  }

  warning("No matching Tabplus file found for date: ", format(current_date, "%Y%m"))
  return(NULL)
}

#' Load NORTH data for a specific month
#'
#' @param current_date Date to process
#' @param use_prerelease Whether to use pre-release data
#' @param lfs_config_list Configuration list
#' @return Data frame with NORTH data or NULL if not found
#' @keywords internal
load_north_data <- function(current_date, use_prerelease = FALSE, lfs_config_list) {
  # Show progress
  cat(format(Sys.time(), "%H:%M:%S"), "-Loading LFS NORTH (Nunavut, N.W.T. and Yukon) for", format(current_date, "%Y%m"), "\n")

  # Format month and year components
  month <- format(current_date, "%m")
  year <- format(current_date, "%y")

  # Define the territory codes
  territories <- c("nun", "nwt", "yuk")

  # Determine base path based on prerelease flag
  base_path <- if (use_prerelease) {
    file.path(get_config("lfs_prerelease_path"), "LFS-North")
  } else {
    file.path(get_config("lfs_client_data_path"), "LFS-North")
  }

  # Create file paths for each territory
  file_paths <- sapply(territories, function(territory) {
    file.path(base_path, paste0(territory, month, year, ".txt"))
  })

  # Get tabs layout from package environment
  tabs_layout <- get_config("tabs_layout")

  # Initialize empty list to store data frames
  territory_data_list <- list()

  # Read each territory file and combine
  for (i in seq_along(territories)) {
    territory <- territories[i]
    file_path <- file_paths[i]

    # Read in TXT fixed-width-file according to the tabs layout
    tryCatch(
      {
        data <- vroom::vroom_fwf(
          file_path,
          vroom::fwf_widths(tabs_layout$Length, tabs_layout$Name),
          col_types = paste(substr(tabs_layout$FORMAT, 1, 1), collapse = ""),
          .name_repair = "minimal"
        )

        # Add territory identifier and store in list
        data <- data %>%
          dplyr::mutate(
            DATE = current_date,
            TERRITORY = toupper(territory),
            HHLDID = as.character(paste(PROV, PROV1, PSEUDOUI, FRAME, STRAFRAM, TYPE, CLUST, ROTATION, LISTLINE, MULT, sep = ""))
          )

        territory_data_list[[territory]] <- data
      },
      error = function(e) {
        warning(paste("Could not read", territory, "file:", e$message))
        NULL
      }
    )
  }

  # Combine all territory data
  if (length(territory_data_list) == 0) {
    warning("No territory data could be processed for date: ", format(current_date, "%Y%m"))
    return(NULL)
  }

  # Combine all data frames in the list
  combined_data <- dplyr::bind_rows(territory_data_list)

  if (nrow(combined_data) == 0) {
    warning("No records found in NORTH data files for date: ", format(current_date, "%Y%m"))
    return(NULL)
  }

  return(combined_data)
}

#' Load Monthly SUPPLEMENT (LMI) data for a specific month
#'
#' @param current_date Date to process
#' @param use_prerelease Whether to use pre-release data
#' @return Data frame with SUPPLEMENT data or NULL if not found
#' @keywords internal
load_supplement_data <- function(current_date, use_prerelease = FALSE, lfs_config_list) {
  df_main <- NULL
  cat(format(Sys.time(), "%H:%M:%S"), "-Loading Monthly Supplement data for", format(current_date, "%Y%m"), "\n")

  base_path <- file.path(get_config("lfs_supplement_path"), "LMI", format(current_date, "%Y"), format(current_date, "%Y%m"))
  base_name <- paste0("supp_final_", format(current_date, "%Y%m"), ".sas7bdat")

  file_path <- file.path(base_path, base_name)
  if (file.exists(file_path)) {
    df_main <- haven::read_sas(file_path) %>% dplyr::mutate(DATE = current_date)
  } else {
    warning("No matching Monthly Supplement file found for date: ", format(current_date, "%Y%m"))
  }
 
  #Load bootstrap weights if requested
  if (lfs_config_list$bootstraps) {
    df_main <- load_supplement_bootstraps(df_main, current_date, base_path, base_name, lfs_config_list)
  }

  return(df_main)
}

#' Load Quarterly SUPPLEMENT (LMSI) data for a specific month
#'
#' @param current_date Date to process
#' @return Data frame with SUPPLEMENT data or NULL if not found
#' @keywords internal
load_quarterly_supplement_data <- function(current_date, lfs_config_list) {
  cat(format(Sys.time(), "%H:%M:%S"), "-Loading Quarterly Supplement data for", format(current_date, "%Y%m"), "\n")
  
  current_month <- as.numeric(format(current_date, "%m"))
 
  if (current_month %in% c(7, 8, 9)) {
    season_upper <- "Summer"
    season_lower <- "summer"
  } else if (current_month %in% c(10, 11, 12)) {
    season_upper <- "Fall"
    season_lower <- "fall"
  } else {
    warning("No Quarterly Supplement data for January to June")
  }

  base_path <- file.path(get_config("lfs_supplement_path"), "LMSI", format(current_date, "%Y"), season_upper)
  base_name <- paste0("supp_final_", format(current_date, "%Y"), "_", season_lower, ".sas7bdat")

  file_path <- file.path(base_path, base_name)
  if (file.exists(file_path)) {
    df_main <- haven::read_sas(file_path) %>% dplyr::mutate(DATE = current_date)
  } else {
    warning("No matching Quarterly Supplement file found for date: ", format(current_date, "%Y%m"))
  return(NULL)
  }

  #Load bootstrap weights if requested
  if (lfs_config_list$bootstraps) {
    df_main <- load_supplement_bootstraps(df_main, current_date, base_path, base_name, lfs_config_list)
  }

  return(df_main)
}

#' Load Disability SUPPLEMENT (DLMI) data for a specific month
#'
#' @param current_date Date to process
#' @return Data frame with SUPPLEMENT data or NULL if not found
#' @keywords internal
load_disability_supplement_data <- function(current_date, lfs_config_list) {
  cat(format(Sys.time(), "%H:%M:%S"), "-Loading Disability Supplement data for", format(current_date, "%Y%m"), "\n")

  base_path <- file.path(get_config("lfs_supplement_path"), "DLMI", format(current_date, "%Y"))
  base_name <- paste0("dlmi_final_", format(current_date, "%Y"), ".sas7bdat")

  file_path <- file.path(base_path, base_name)
  if (file.exists(file_path)) {
    df_main <- haven::read_sas(file_path) %>% dplyr::mutate(DATE = current_date)
  } else {
    warning("No matching Disability Supplement file found for date: ", format(current_date, "%Y%m"))
    return(NULL)
  }

  #Load bootstrap weights if requested
  if (lfs_config_list$bootstraps) {
    df_main <- load_supplement_bootstraps(df_main, current_date, base_path, base_name, lfs_config_list)
  }

  return(df_main)
}

#' Load Bootstrap data for a specific month
#'
#' @param current_date Date to process
#' @param use_prerelease Whether to use pre-release data
#' @param lfs_config_list Configuration list
#' @return Data frame with Bootstrap data or NULL if not found
#' @keywords internal
load_bootstrap_data <- function(current_date, use_prerelease = FALSE, lfs_config_list) {
  bw_weight <- toupper(lfs_config_list$weight_var) # Ensure uppercase

  cat(format(Sys.time(), "%H:%M:%S"), "-Loading Bootstraps (", bw_weight, ")", format(current_date, "%Y%m"), "\n")

  file_name <- paste0("bootstrap_prov_", tolower(bw_weight), "_", format(current_date, "%Y%m"), ".sas7bdat")

  file_path <- if (use_prerelease) {
    base_dir <- get_config("lfs_prerelease_path")
    bootstrap_dir <- list.files(base_dir, pattern = "^LFS-Bootstrap$", ignore.case = TRUE, full.names = TRUE)[1]
    prov_dir <- list.files(bootstrap_dir, pattern = paste0("^PROV_", bw_weight, "$"), ignore.case = TRUE, full.names = TRUE)[1]
    file.path(prov_dir, file_name)
  } else {
    base_dir <- get_config("lfs_client_data_path")
    bootstrap_dir <- list.files(base_dir, pattern = "^LFS-Bootstrap$", ignore.case = TRUE, full.names = TRUE)[1]
    prov_dir <- list.files(bootstrap_dir, pattern = paste0("^PROV_", bw_weight, "$"), ignore.case = TRUE, full.names = TRUE)[1]
    file.path(prov_dir, file_name)
  }

  if (!file.exists(file_path)) {
    warning("Bootstrap file not found: ", file_path)
    return(NULL)
  }

  bootstrap_data <- haven::read_sas(file_path) %>%
    dplyr::mutate(ADM_DATE = as.Date(paste0(ADM_SDATE, "01"), format = "%Y%m%d")) %>%
    dplyr::select(-ADM_SDATE)

  # Rename bootstrap weight columns based on the weight type
  bootstrap_data <- bootstrap_data %>%
    dplyr::rename_with(~ paste0("BW_", bw_weight, "_", sub("^bw", "", .x)), dplyr::starts_with("bw"))

  return(bootstrap_data)
}

#' Load supplement Bootstrap data for a specific month
#'
#' @param df_main Main supplement data
#' @param current_date Date to process
#' @param base_path Folder path of supplement data
#' @param base_name Filename of main supplement data
#' @return Data frame with SUPPLEMENT data joined with Bootstrap weights or NULL if not found
#' @keywords internal
load_supplement_bootstraps <- function(df_main, current_date, base_path, base_name, lfs_config_list){
	df_bootstrap <- NULL

	#Creates bootstrap filename based on name of main datafile
	if (lfs_config_list$supplement_lmi) {
		file_name_bootstraps <- sub("supp_final", "supp_bootstrap_weights", base_name)
		supp_type = "Monthly Supplement "
		join_cols_bootstraps="HHLDID"
	} else if (lfs_config_list$supplement_lmsi) {
		file_name_bootstraps <- sub("supp_final", "supp_bs_weights", base_name)
		supp_type = "Quarterly Supplement "
		join_cols_bootstraps="HHLDID"
	} else if (lfs_config_list$supplement_dlmi) {
		file_name_bootstraps <- sub("dlmi_final", "dlmi_bootstrap", base_name)
		supp_type = "Disability Supplement "
		join_cols_bootstraps=c("HHLDID","LINE","SYEAR","SMTH")
	}

	# Print loading message with timestamp
	cat(format(Sys.time(), "%H:%M:%S"), "-Loading Bootstraps for ", supp_type , "for date: ", format(current_date, "%Y%m"), "\n")

	# Create file name
	file_path_bootstraps <- file.path(base_path, file_name_bootstraps)

	  if (file.exists(file_path_bootstraps)) {
	  df_bootstrap <- haven::read_sas(file_path_bootstraps)
	} else {
	  warning("No matching Bootstrap file found for ", supp_type, "for date: ", format(current_date, "%Y%m"))
	}

	# Rename bootstrap weight columns based on the weight type (all three supplements have the same naming convention for bootstrap weights)
	col_pattern <- paste0("^calwgt_BS(", paste(1:1000, collapse = "|"), ")$")
	boostrap_name_pattern <- paste0("BW_", lfs_config_list$weight_var, "_\\1")

	df_bootstrap <- df_bootstrap %>%
	  dplyr::rename_with(
		~ sub("^calwgt_BS(\\d+)$", boostrap_name_pattern, .x),
		.cols = dplyr::matches(col_pattern)
	  )

	# Rename to consistent case if needed
	if ("hhldid" %in% names(df_bootstrap)) {
	  df_bootstrap <- df_bootstrap %>% dplyr::rename(HHLDID = hhldid)
	}
	# Ensure uppercase values
	df_bootstrap <- df_bootstrap %>% mutate(HHLDID = toupper(HHLDID))

	#Join bootstraps to main supplement data, return dataframe
	return(dplyr::left_join(df_main, df_bootstrap, by = join_cols_bootstraps))
}

#' Load Bootstrap North data for a specific month
#'
#' @param current_date Date to process
#' @param use_prerelease Whether to use pre-release data
#' @param lfs_config_list Configuration list
#' @return Data frame with Bootstrap data or NULL if not found
#' @keywords internal
load_bootstrap_north_data <- function(current_date, use_prerelease = FALSE, lfs_config_list) {
  bw_weight <- toupper(lfs_config_list$weight_var) # Ensure uppercase

  cat(format(Sys.time(), "%H:%M:%S"), "-Loading North Bootstraps (", bw_weight, ")", format(current_date, "%Y%m"), "\n")

  file_name <- paste0("bootstrap_terr_", tolower(bw_weight), "_", format(current_date, "%Y%m"), ".sas7bdat")

  file_path <- if (use_prerelease) {
    base_dir <- get_config("lfs_prerelease_path")
    bootstrap_dir <- list.files(base_dir, pattern = "^LFS-Bootstrap$", ignore.case = TRUE, full.names = TRUE)[1]
    prov_dir <- list.files(bootstrap_dir, pattern = paste0("^TERR_", bw_weight, "$"), ignore.case = TRUE, full.names = TRUE)[1]
    file.path(prov_dir, file_name)
  } else {
    base_dir <- get_config("lfs_client_data_path")
    bootstrap_dir <- list.files(base_dir, pattern = "^LFS-Bootstrap$", ignore.case = TRUE, full.names = TRUE)[1]
    prov_dir <- list.files(bootstrap_dir, pattern = paste0("^TERR_", bw_weight, "$"), ignore.case = TRUE, full.names = TRUE)[1]
    file.path(prov_dir, file_name)
  }

  if (!file.exists(file_path)) {
    warning("Bootstrap North file not found: ", file_path)
    return(NULL)
  }

  bootstrap_north_data <- haven::read_sas(file_path) %>%
    dplyr::mutate(ADM_DATE = as.Date(paste0(ADM_SDATE, "01"), format = "%Y%m%d")) %>%
    dplyr::select(-ADM_SDATE)

  # Rename bootstrap weight columns based on the weight type
  bootstrap_north_data <- bootstrap_north_data %>%
    dplyr::rename_with(~ paste0("BW_", bw_weight, "_", sub("^bw", "", .x)), dplyr::starts_with("bw"))

  return(bootstrap_north_data)
}



#' Load TABS Plus North data for a specific month
#'
#' @param current_date Date to process
#' @param use_prerelease Whether to use pre-release data
#' @return Data frame with TABS Plus data or NULL if not found
#' @keywords internal
load_plus_north_data <- function(current_date, use_prerelease = FALSE) {
  cat(format(Sys.time(), "%H:%M:%S"), "-Loading TABPLUS NORTH for", format(current_date, "%Y%m"), "\n")

  # Try both common variants
  variants <- c(
    paste0("tabplus_north_", format(current_date, "%Y%m"), ".sas7bdat"),
    paste0("Tabplus_north_", format(current_date, "%Y%m"), ".sas7bdat")
  )

  base_path <- if (use_prerelease) {
    file.path(get_config("lfs_prerelease_path"), "LFS-Tabplus")
  } else {
    file.path(get_config("lfs_client_data_path"), "LFS-North")
  }

  # Try each variant
  for (variant in variants) {
    file_path <- file.path(base_path, variant)
    if (file.exists(file_path)) {
      return(haven::read_sas(file_path) %>% dplyr::mutate(DATE = current_date))
    }
  }

  warning("No matching Tabplus North file found for date: ", format(current_date, "%Y%m"))
  return(NULL)
}

