#' Load data for a single month
#'
#' @param current_date Date to process
#' @param lfs_config_list Configuration list
#' @return Data frame with loaded data
#' @keywords internal
load_month_data <- function(current_date, lfs_config_list) {
  cat(format(Sys.time(), "%H:%M:%S"), "Loading data for:", format(current_date, "%Y%m"), "\n")

  # Determine if using prerelease
  use_prerelease <- lfs_config_list$prerelease &&
    current_date == as.Date(lfs_config_list$end_date)

  # Load TABS data
  data <- load_tabs_data(current_date, use_prerelease, lfs_config_list)

  # Load and join NORTH data if enabled
  if (lfs_config_list$north) {
    north_data <- load_north_data(current_date, use_prerelease, lfs_config_list)
    if (!is.null(north_data)) {
      data <- dplyr::bind_rows(data, north_data)
    }
  }

  # Load and join TABS Plus data if enabled
  if (lfs_config_list$tabs_plus) {
    plus_data <- load_plus_data(current_date, use_prerelease)
    # Load and append Plus North to TABS Plus data if necessary
    if (lfs_config_list$north) {
      plus_north_data <- load_plus_north_data(current_date, use_prerelease)
      plus_data <- rbind(plus_data, plus_north_data)
    }
    if (!is.null(plus_data)) {
      join_cols <- c("HHLDID", "DATE")
      if ("LINE" %in% names(data) && "LINE" %in% names(plus_data)) {
        join_cols <- c(join_cols, "LINE")
      }
      data <- dplyr::left_join(data, plus_data, by = join_cols, relationship = "many-to-many")
    }
  }

  # Load and join Monthly SUPPLEMENT data if enabled
  if (lfs_config_list$supplement_lmi) {
    supplement_data <- load_supplement_data(current_date, use_prerelease, lfs_config_list)
    if (!is.null(supplement_data)) {
      join_cols <- c("HHLDID", "DATE")
      if ("LINE" %in% names(data) && "LINE" %in% names(supplement_data)) {
        join_cols <- c(join_cols, "LINE")
      }
      data <- dplyr::left_join(data, supplement_data, by = join_cols, relationship = "many-to-many")
    }
  }

    # Load and join Quarterly SUPPLEMENT data if enabled
  if (lfs_config_list$supplement_lmsi) {
    quarterly_supplement_data <- load_quarterly_supplement_data(current_date, lfs_config_list)
    if (!is.null(quarterly_supplement_data)) {
      join_cols <- c("HHLDID", "SYEAR", "SMTH")
      if ("LINE" %in% names(data) && "LINE" %in% names(quarterly_supplement_data)) {
        join_cols <- c(join_cols, "LINE")
      }
     
      # Remove DATE from the right-hand side before joining
      quarterly_supplement_data <- quarterly_supplement_data %>% dplyr::select(-DATE)

      data <- dplyr::inner_join(data, quarterly_supplement_data, by = join_cols, relationship = "many-to-many")
    }
  }

      # Load and join Disability SUPPLEMENT data if enabled
  if (lfs_config_list$supplement_dlmi) {
    disability_supplement_data <- load_disability_supplement_data(current_date, lfs_config_list)

    if (!is.null(disability_supplement_data)) {
      join_cols <- c("HHLDID", "SYEAR", "SMTH")
      if ("LINE" %in% names(data) && "LINE" %in% names(disability_supplement_data)) {
        join_cols <- c(join_cols, "LINE")
      }

      # Remove DATE from the right-hand side before joining
      disability_supplement_data <- disability_supplement_data %>% dplyr::select(-DATE)

      data <- dplyr::inner_join(data, disability_supplement_data, by = join_cols, relationship = "many-to-many")

    }
  }

  # Add custom DVs
  if (lfs_config_list$add_custom_dvs) {
    data <- data %>% custom_dvs()
  }

  # Apply global filter condition if provided
  if (!is.null(lfs_config_list$filter_condition) && lfs_config_list$filter_condition != "") {
    cat(format(Sys.time(), "%H:%M:%S"), "  Applying global filter:", lfs_config_list$filter_condition, "\n")
    data <- data %>%
      dplyr::filter(!!rlang::parse_expr(lfs_config_list$filter_condition))
  }

  # Load bootstrap weights if needed
if (lfs_config_list$bootstraps & !lfs_config_list$supplement_lmi & !lfs_config_list$supplement_lmsi & !lfs_config_list$supplement_dlmi) {
  bootstrap_data <- load_bootstrap_data(current_date, use_prerelease, lfs_config_list)
  # append north bootstrap data to bootstrap data if needed
  if (lfs_config_list$north) {
	bootstrap_north_data <- load_bootstrap_north_data(current_date, use_prerelease, lfs_config_list)
	bootstrap_data <- rbind(bootstrap_data, bootstrap_north_data)
  }

  if (!is.null(bootstrap_data)) {
	data <- dplyr::left_join(data, bootstrap_data, by = c("HHLDID", "DATE" = "ADM_DATE"))
  }
}

  return(data)
}
