# lfsinsights

An R package for aggregating Labour Force Survey data.

## Installation


```r
# Install package remotes package if needed
install.packages("remotes") # install remotes package if not already

# Install lfsinsights package
remotes::install_git("https://gitlab.k8s.cloud.statcan.ca/clmi-cimt/clmi-insights/lfsinsights")

# Initialize package
library(lfsinsights)
```

## 🚀 Live Application

Click the link below to access and use the LFS Table Configurator:

**[Launch LFS Configurator](https://clmi-cimt.pages.cloud.statcan.ca/clmi-insights/lfsinsights/index.html)**

Or visit: https://clmi-cimt.pages.cloud.statcan.ca/clmi-insights/lfsinsights/index.html


## Basic usage

```r
library(lfsinsights)

# Calculate basic summary table
df_result <- lfs_table(
  # Date
  start_date = "2025-01-01",
  end_date = "2025-04-01",
  # Filter
  filter_condition = "AGE>=15 & LFSSTAT %in% c(1,2)",
  # Analysis vars
  analysis_vars = "GENDER, NOC_5"
)

# Calculate multiple estimates in one pass (showing all options)
df_result <- lfs_table(
  # Date 
  start_date = "2025-01-01",
  end_date = "2025-03-01",
  moving_avg = 1,                 # Number of periods for moving average
  prerelease = FALSE,             # Use pre-release data for latest month
  filter_months = NULL,           # Alternatively: "1, 2, 3"
  filter_years = NULL,            # Alternatively: "2025"
  
  # Analysis parameters
  filter_condition = "AGE >= 15 & !is.na(LFSSTAT)",  # Global filter applied at data loading
  analysis_vars = "PROV, GENDER",
  weight_var = "FINALWT",         # Alternative: "IVMWT", "supp_weight"

  # Estimates configuration
  estimates = list(
    # Count of employed people
    list(
      est_name = "Employment",
      est_type = "sum",
      est_filter = "EMPLOYED == 1" 
    ),
    # Unemployment rate estimate
    list(
      est_name = "Unemployment",
      est_type = "ratio",
      ratio_numerator = "UNEMPLOYED",
      ratio_denominator = "LABOURFORCE",
      ratio_type = "percent",    # Options: "percent" or "average"
      ratio_decimals = 1,        # Number of decimal places
      est_filter = NULL          # No additional filter needed, same as global filter
    ),
    
    # Employment rate estimate
    list(
      est_name = "Wages for employees",
      est_type = "ratio",
      ratio_numerator = "HRLYEARN",
      ratio_denominator = "POP",
      ratio_type = "average",
      ratio_decimals = 2,
      est_filter = "LFSSTAT %in% c(1,2) & COWMAIN %in% c(1,2)"
    )
  ),

  # Other data source options
  bootstraps = FALSE,             # Include bootstrap weights
  tabs_plus = FALSE,              # Include TABS Plus data
  supplement_lmi = FALSE,         # Include LMI (monthly) supplement data
  supplement_lmsi = FALSE,        # Include LMSI (quarterly) supplement data
  supplement_dlmi = FALSE,        # Include DLMI (annual disability) supplement data
  north = FALSE,                  # Include territories data
  
  # Processing options
  add_custom_dvs = TRUE,          # Apply custom derived variables
  add_labels = TRUE,              # Apply labels to variables
  language = "EN",                # Language for labels: "EN" or "FR"
  
  # Calculation options
  include_marginals = TRUE,       # Include marginal totals
  weight_rounding = 100,          # Rounding factor for estimate levels (sum, numerator, denominator)
  add_suppression_flag = TRUE,    # Add basic suppression flags
  
  # Change calculation options
  calculate_change = FALSE,        # Calculate period-over-period changes
  lag_period = 1                  # Periods to lag for change calculation
)

# Load microdata only
microdata <- lfs_microdata(
  start_date = "2025-01-01",
  end_date = "2025-01-01",
  filter_condition = "AGE >= 15 & EMPLOYED == 1"
)


# Calculate year-over-year, three-month moving average change for VISMIN unemployment rates with bootstrap weights and variance
df_result <- lfs_table(
  # Date configuration
  start_date = "2024-04-01",
  end_date = "2025-04-01",
  moving_avg = 3,
  filter_months = "4",
  
  # Analysis parameters    
  filter_condition = "AGE >= 15 & !is.na(LFSSTAT)",
  analysis_vars = "PROV, GENDER, VISMIN",
  weight_var = "IVMWT",
  
  # Estimates configuration
  estimates = list(
    list(
      est_type = "ratio",
      ratio_numerator = "UNEMPLOYED",
      ratio_denominator = "LABOURFORCE",
      ratio_type = "percent",
      ratio_decimals = 1
    )
  ),
  
  # Data sources
  bootstraps = TRUE,
  
  # Processing options
  add_custom_dvs = TRUE,
  add_labels = TRUE,
  language = "EN",
  include_marginals = TRUE,
  weight_rounding = 100,
  add_suppression_flag = FALSE,
  calculate_change = TRUE,
  lag_period = 1
)
```

## Package Workflow

```mermaid
graph TD
    Start([LFS Insights Process Start<br>lfs_table / lfs_microdata]) --> Setup[Initialization & Setup]
    Setup --> |Validate dates, determine months, load DVs| Loop
    
    subgraph Iterative Monthly Processing
        Loop{For Each Required Month} --> LoadRaw[Load Raw Data (TABS, NORTH, SUPPLEMENTS)]
        LoadRaw --> ApplyFilter[Apply Global Filters & Custom DVs]
        ApplyFilter --> Boot[Optional: Load BOOTSTRAPS]
        Boot --> ModeCheck{Microdata Only Mode?}
        ModeCheck -->|Yes| StoreRaw[Store Raw Microdata]
        ModeCheck -->|No| CalcSumm[Calculate Estimate Summaries]
    end
    
    StoreRaw --> FinalizeMicro[Combine All Raw Microdata]
    CalcSumm --> PostProcLoop
    
    subgraph Post-Processing & Finalization
        PostProcLoop{FOR EACH DEFINED ESTIMATE:} --> Combine[Combine monthly summaries]
        Combine --> Complete[Complete Data Combinations<br>fill missing rows]
        Complete --> Marginals[Optional: Calculate Marginal Totals]
        Marginals --> MovingAvg[Optional: Calculate Moving Average]
        MovingAvg --> Suppression[Optional: Add Suppression Flags]
        Suppression --> Rounding[Apply Rounding to Estimate Levels]
        Rounding --> Ratios[IF Ratio/Distribution: Calculate Ratios]
        Ratios --> Change[IF Change: Calculate Period Changes]
        Change --> Differences[IF Difference: Calculate Category Differences]
        Differences --> Variance[IF Bootstraps: Calculate Bootstrap Variance]
        Variance --> CombineAll[Combine All Processed Estimates]
    end
    
    CombineAll --> Labels
    FinalizeMicro --> Labels[Optional: Apply Human-Readable Labels]
    
    Labels --> CleanUp[Final Result Clean-up]
    CleanUp --> FinalOut([Final LFS Data Output])
```


## Built-in Visualizations

The package includes standardized plotting functions that use `plotly` to automatically handle formatting, source annotations, and responsive layouts.

```r
library(lfsinsights)

# Generate a formatted line chart for the unemployment rate
lfs_plotly_line(
  data = df_result,
  x = "DATE",
  y = "Unemployment",
  color = "GENDER",
  title = "Unemployment Rate by Gender",
  subtitle = "Monthly rates, seasonally adjusted",
  y_title = "Percentage (%)"
)

# Generate a formatted bar chart
lfs_plotly_bar(
  data = df_result,
  x = "DATE",
  y = "Employment",
  color = "PROV",
  title = "Employment Levels by Province"
)
```

