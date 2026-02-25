# =============================================================================
# FOLDER PATH CONFIGURATION 
# Define default Windows network paths here 
# The functions afterwards will convert them to "The Zone" paths if applicable
# =============================================================================
DEFAULT_PATHS <- list(
  LFS_CLIENT_DATA_PATH = "//fld8filer/LFS-Client/",
  LFS_PRERELEASE_PATH  = "//fld8filer/LFS Pre-release TABS/RV2021/",
  LFS_SUPPLEMENT_PATH  = "//fld8filer/public/Labour Force Survey - Supplements/Data/Analysis/"
)

# Internal environment to hold configuration
.lfsinsights_env <- new.env(parent = emptyenv())


# Function to convert Windows network path to Linux path
convert_to_linux_path <- function(windows_path) {
  # Replace Windows network path prefix with Linux path
  linux_path <- gsub("^//fld8filer/", "~/filers/fld8filersvm/", windows_path)
  return(linux_path)
}

# Function to find case-insensitive directory match
find_case_insensitive_dir <- function(path) {
  # Parse the path into components
  components <- unlist(strsplit(path, "/"))
  current_path <- ""

  for (component in components) {
    if (component == "" || component == "~") {
      current_path <- paste0(current_path, component)
      next
    }

    # Get all directories in the current path
    if (current_path == "~") {
      dir_listing <- list.files(path.expand(current_path), full.names = TRUE)
    } else {
      if (current_path == "") {
        # Handle absolute paths starting with /
        dir_listing <- list.files("/", full.names = TRUE)
      } else {
        dir_listing <- list.files(current_path, full.names = TRUE)
      }
    }

    # Extract just the directory names from the full paths
    dir_names <- basename(dir_listing)

    # Find a case-insensitive match
    match_idx <- which(tolower(dir_names) == tolower(component))

    if (length(match_idx) > 0) {
      # Use the actual case from the filesystem
      current_path <- dir_listing[match_idx[1]]
    } else {
      # If no match, just use the component as provided (will likely fail later)
      if (current_path == "~") {
        current_path <- path.expand(paste0(current_path, "/", component))
      } else {
        current_path <- paste0(current_path, "/", component)
      }
    }
  }

  # Add trailing slash if missing
  if (nchar(current_path) > 0 && substr(current_path, nchar(current_path), nchar(current_path)) != "/") {
    current_path <- paste0(current_path, "/")
  }

  return(current_path)
}


# Function run automatically when package is loaded
.onLoad <- function(libname, pkgname) {
  options(warning.r.version = FALSE)

  cfg_path <- system.file("config", package = pkgname)

  # Load TABS layout directly into the environment
  try(.lfsinsights_env$tabs_layout <- read.csv(file.path(cfg_path, "TABS_Layout.csv")), silent = TRUE)

  # Source default paths into a temporary environment, then copy
  temp_env <- new.env()
  
  # Load the defaults defined at the top of this file
  temp_env$LFS_CLIENT_DATA_PATH <- DEFAULT_PATHS$LFS_CLIENT_DATA_PATH
  temp_env$LFS_PRERELEASE_PATH  <- DEFAULT_PATHS$LFS_PRERELEASE_PATH
  temp_env$LFS_SUPPLEMENT_PATH  <- DEFAULT_PATHS$LFS_SUPPLEMENT_PATH

  # Check if the system is Linux and update paths accordingly
  if (Sys.info()["sysname"] == "Linux") {
    # Define path variables to process
    path_vars <- c("LFS_CLIENT_DATA_PATH", "LFS_PRERELEASE_PATH", "LFS_SUPPLEMENT_PATH")

    for (var_name in path_vars) {
      if (exists(var_name, envir = temp_env)) {
        # Get the Windows path
        windows_path <- get(var_name, envir = temp_env)

        # Convert to Linux path
        linux_path <- convert_to_linux_path(windows_path)

        # Apply case-insensitive resolution
        tryCatch(
          {
            actual_path <- find_case_insensitive_dir(linux_path)

            # Store with lowercase name for consistency
            lower_name <- tolower(var_name)
            .lfsinsights_env[[lower_name]] <- actual_path

            cat("Converted and resolved path for", var_name, ":", actual_path, "\n")
          },
          error = function(e) {
            warning(paste("Error finding case-insensitive path for", var_name, ":", e$message))
            # Fall back to the original converted path without case resolution
            lower_name <- tolower(var_name)
            .lfsinsights_env[[lower_name]] <- linux_path
          }
        )
      }
    }
  } else {
    # Use Windows paths from the config file directly
    .lfsinsights_env$lfs_client_data_path <- temp_env$LFS_CLIENT_DATA_PATH
    .lfsinsights_env$lfs_prerelease_path <- temp_env$LFS_PRERELEASE_PATH
    .lfsinsights_env$lfs_supplement_path <- temp_env$LFS_SUPPLEMENT_PATH
  }

  # Add metadata path directly into the environment
  .lfsinsights_env$lfs_metadata_path <- system.file("metadata", package = pkgname)
}

#' Get a configuration value
#'
#' @param name Name of the configuration value to retrieve
#' @return The configuration value
#' @export
get_config <- function(name) {
  if (!exists(name, envir = .lfsinsights_env, inherits = FALSE)) {
    stop(paste("Configuration value not found:", name))
  }
  get(name, envir = .lfsinsights_env, inherits = FALSE)
}







#' Manage custom derived variables for LFS data
#'
#' @description
#' This file provides functionality to manage custom derived variables
#' for Labour Force Survey data processing.
#'
#' @details
#' The functions in this file enable users to:
#' - Create a custom_dvs.R file in the user's documents if it doesn't exist
#' - Load custom derived variables defined in the custom_dvs.R file
#' - Open the custom_dvs.R file for editing
#'
#' @importFrom dplyr mutate case_when
#' @keywords internal

# Path to the custom_dvs.R file
custom_dvs_file <- file.path(
  ifelse(.Platform$OS.type == "windows",
    file.path(Sys.getenv("USERPROFILE"), "Documents", "lfsinsights-config"),
    file.path(Sys.getenv("HOME"), "lfsinsights-config")
  ),
  "custom_dvs.R"
)

#' Create custom derived variables file
#'
#' @description
#' Checks for the existence of the custom derived variables file and creates it
#' if it doesn't exist. The file includes default transformation logic for commonly
#' used derived variables.
#'
#' @return None
#' @export
create_custom_dvs_file <- function() {
  # Get the directory path from custom_dvs_file
  custom_dvs_dir <- dirname(custom_dvs_file)

  # Check if the directory exists and create it if it does not
  if (!dir.exists(custom_dvs_dir)) {
    suppressWarnings(dir.create(custom_dvs_dir, recursive = TRUE)) # Create dir if doesn't exist
  }

  if (!file.exists(custom_dvs_file)) {
    message(format(Sys.time(), "%H:%M:%S"), " Custom derived variables file not found. Creating the file with default content.")

    # Default content for the custom_dvs.R file
    custom_dvs_content <- '# Custom Derived Variables - function returns a data frame with custom DVs added.

# Load tidyverse quietly prior to run
suppressWarnings(suppressPackageStartupMessages(library(tidyverse)))

### Enter your custom derived variables within the function below ###
custom_dvs <- function(data) {
  data %>%
    mutate(
      # Binary groupings for lfsstat (especially useful to calc ratios)
      POP = 1,
      EMPLOYED = case_when(LFSSTAT %in% c(1,2) ~ 1, .default = 0),
      LABOURFORCE = case_when(LFSSTAT %in% c(1,2,3,4,5) ~ 1, .default = 0),
      UNEMPLOYED = case_when(LFSSTAT %in% c(3,4,5) ~ 1, .default = 0),
      EMPLOYEE = case_when(LFSSTAT %in% c(1, 2) & COWMAIN %in% c(1, 2) ~ 1, .default = 0),

      # Other common dvs
      LFSSTAT3 = case_when(
        LFSSTAT %in% c(1,2) ~ "1",
        LFSSTAT %in% c(3,4,5) ~ "2",
        LFSSTAT %in% c(6,7) ~ "3"
      ),

      AGEGROUP_MAIN = case_when(
        AGE >= 15 & AGE <= 24 ~ "1",
        AGE >= 25 & AGE <= 54 ~ "2",
        AGE >= 55             ~ "3"
      ),

      INDUSTRY = case_when(
        substr(NAICS_5, 2, 3) %in% c("31", "32", "33") ~ "31",
        substr(NAICS_5, 2, 3) %in% c("44", "45") ~ "44",
        substr(NAICS_5, 2, 3) %in% c("48", "49") ~ "48",
        substr(NAICS_5, 2, 3) %in% c("52", "53") ~ "52",
        substr(NAICS_5, 2, 3) %in% c("55", "56") ~ "55",
        substr(NAICS_5, 2, 3) %in% c("51", "71") ~ "51",
        substr(NAICS_5, 2, 5) %in% c("1131", "1132", "1133", "1141", "1142", "1153") |
          substr(NAICS_5, 2, 3) == "21" ~ "21",
        NAICS_5 == "nan" ~ "nan",
        TRUE ~ substr(NAICS_5, 2, 3)
      ),

      NAICS3 = case_when(
        !is.na(NAICS_5) ~ substr(NAICS_5, 2, 4),
        TRUE ~ NA_character_  # This sets NAICS3 to NA if NAICS_5 is NA
      ),

      NAICS4 = case_when(
        !is.na(NAICS_5) ~ substr(NAICS_5, 2, 5),
        TRUE ~ NA_character_  # This sets NAICS4 to NA if NAICS_5 is NA
      ),

      NOC1 = substr(NOC_5, 1, 1),
      NOC2 = substr(NOC_5, 1, 2),
      NOC3 = substr(NOC_5, 1, 3),
      NOC4 = substr(NOC_5, 1, 4),

      # Add decimals to HRLYEARN and others
      HRLYEARN = HRLYEARN/100,
      WKLYEARN = WKLYEARN/100,
      AHRSMAIN = AHRSMAIN/10
    )
}'

    # Write the default content to the custom_dvs.R file
    writeLines(custom_dvs_content, custom_dvs_file)
  }
}

#' Load custom derived variables
#'
#' @description
#' Loads the custom derived variables from the `custom_dvs.R` file.
#' If the file is missing, it will be created automatically.
#'
#' @return None
#' @export
load_custom_dvs <- function() {
  create_custom_dvs_file() # Ensure the file exists

  # Source the custom_dvs.R file to load the transformations
  invisible(source(custom_dvs_file)) # Use invisible() to suppress output
  custom_dvs_file_print <- gsub("/", "\\\\", custom_dvs_file)
  cat(format(Sys.time(), "%H:%M:%S"), "Custom derived variables loaded from: ", custom_dvs_file_print, "\n")
}

#' Open custom derived variables file
#'
#' @description
#' Opens the `custom_dvs.R` file in the user's default text editor.
#' If running in RStudio, it will open in the RStudio source editor.
#' If the file doesn't exist, it will be created with default content.
#'
#' @return None
#' @export
open_custom_dvs <- function() {
  # Ensure the file exists
  create_custom_dvs_file() # Create the file if it doesn't exist

  # Open the file in the appropriate editor
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    # Open in RStudio source editor
    rstudioapi::navigateToFile(custom_dvs_file)
  } else {
    # Open in the system's default editor
    utils::file.edit(custom_dvs_file)
  }
}
