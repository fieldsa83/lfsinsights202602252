#' Create a formatted line chart using Plotly
#'
#' This function creates a standardized line chart with Plotly, designed with consistent
#' styling and appropriate spacing based on the elements included. It can handle both
#' single and multi-series data.
#'
#' @param data A data frame containing the variables to plot
#' @param x Character string specifying the column name for the x-axis
#' @param y Character string specifying the column name for the y-axis
#' @param color Optional character string specifying the column name for color grouping
#' @param title Chart title. Default is "Chart: "
#' @param subtitle Optional subtitle text. Default is empty
#' @param source Source information text. Default is "Statistics Canada (Labour Force Survey)."
#' @param y_range Optional numeric vector of length 2 specifying y-axis range. If NULL (default),
#'        Plotly's automatic ranging is used
#' @param y_title Optional y-axis title. Default is empty
#' @param colors Color palette for lines. Default is c("#234399", "#B52049", "grey50", "#8FAADC")
#' @param width Chart width in pixels. Default is 900
#' @param height Chart height in pixels. Default is 650
#'
#' @return A plotly object
#'
#' @details
#' The function automatically adjusts spacing and layout based on whether subtitle and color
#' grouping are used. It applies standardized formatting for titles, annotations, and legend
#' positioning.
#'
#' @examples
#' \dontrun{
#' # Basic usage with single line
#' lfs_plotly_line(
#'   data = unemployment_data,
#'   x = "DATE",
#'   y = "RATE",
#'   title = "Unemployment Rate"
#' )
#'
#' # With color grouping and subtitle
#' lfs_plotly_line(
#'   data = unemployment_data,
#'   x = "DATE",
#'   y = "RATE",
#'   color = "GENDER",
#'   title = "Unemployment Rate by Gender",
#'   subtitle = "Monthly rates, seasonally adjusted"
#' )
#' }
#'
#' @importFrom plotly plot_ly layout %>%
#' @importFrom stats as.formula
#' @export
lfs_plotly_line <- function(
    data,
    x,
    y,
    color = NULL, # Optional color parameter
    title = "Chart ",
    subtitle = "", # Default to empty string
    source = "Statistics Canada, Labour Force Survey.",
    y_range = NULL, # Set to NULL by default
    y_title = "", # Default y-axis title is blank
    colors = c("#234399", "#B52049", "grey50", "#8FAADC"),
    width = 800,
    height = 600) {
  # Generate formula expressions from column names
  x_formula <- as.formula(paste0("~", x))
  y_formula <- as.formula(paste0("~", y))

  # Create the basic yaxis settings
  yaxis_settings <- list(title = y_title)

  # Add range only if it's provided
  if (!is.null(y_range)) {
    yaxis_settings$range <- y_range
  }

  # Create annotations list - always include source
  annotations_list <- list(
    list(
      text = paste0("<b>Source: </b>", source),
      font = list(family = "Arial", size = 15),
      x = -0.061, y = -0.2, xref = "paper", yref = "paper", xanchor = "left", showarrow = FALSE
    )
  )

  # Determine margin based on subtitle and color presence
  has_subtitle <- subtitle != ""
  has_color <- !is.null(color)

  subtitle_margin <- 1.19

  # Set margin based on combination of factors
  if (has_subtitle && has_color) {
    top_margin <- 123 # Both subtitle and color (needs most space)
  } else if (!has_subtitle && !has_color) {
    top_margin <- 90 # Neither subtitle nor color (needs least space)
  } else if (has_subtitle && !has_color) {
    top_margin <- 110 # subtitle but no color
    subtitle_margin <- 1.15
  } else {
    top_margin <- 110 # color but no subtitle
  }

  plot_margin <- list(l = 100, r = 50, t = top_margin, b = 120, pad = 20)

  # Add subtitle annotation only if it's not empty
  if (has_subtitle) {
    subtitle_annotation <- list(
      text = subtitle,
      font = list(family = "Arial", size = 15),
      x = -0.061, y = subtitle_margin, xref = "paper", yref = "paper", xanchor = "left", showarrow = FALSE
    )
    annotations_list <- c(list(subtitle_annotation), annotations_list)
  }

  # Create the plot based on whether color is provided
  if (!has_color) {
    # No color provided - create simple plot
    p <- plot_ly(
      data = data,
      x = x_formula,
      y = y_formula,
      type = "scatter",
      mode = "lines",
      width = width,
      height = height
    )
  } else {
    # Color provided - create color-based plot
    color_formula <- as.formula(paste0("~", color))
    p <- plot_ly(
      data = data,
      x = x_formula,
      y = y_formula,
      color = color_formula,
      colors = colors,
      type = "scatter",
      mode = "lines",
      width = width,
      height = height
    )
  }

  # Add layout settings to the plot
  p %>%
    layout(
      title = list(
        text = paste0("<b>", title, "</b>"),
        font = list(family = "Arial", size = 20),
        x = 0.076, y = 0.95, xanchor = "left"
      ),
      annotations = annotations_list,
      xaxis = list(
        title = "",
        range = c(min(data$DATE) - 15, max(data$DATE) + 15),
        showgrid = FALSE
      ),
      yaxis = yaxis_settings,
      legend = list(
        orientation = "h",
        x = -0.005,
        y = 1.1,
        font = list(family = "Arial", size = 15),
        categoryorder = "trace",
        traceorder = "factor"
      ),
      margin = plot_margin
    )
}




#' Create a formatted bar chart using Plotly
#'
#' This function creates a standardized bar chart with Plotly, designed with consistent
#' styling and appropriate spacing based on the elements included. It can handle both
#' single and multi-series data.
#'
#' @param data A data frame containing the variables to plot
#' @param x Character string specifying the column name for the x-axis
#' @param y Character string specifying the column name for the y-axis
#' @param color Optional character string specifying the column name for color grouping.
#'   If NULL, all bars will be colored with the first color from the `colors` palette.
#' @param title Chart title. Default is "Chart "
#' @param subtitle Optional subtitle text. Default is empty.
#' @param source Source information text. If empty (""), the source annotation will not be included.
#' @param y_range Optional numeric vector of length 2 specifying y-axis range. If NULL (default),
#'   Plotly's automatic ranging is used.
#' @param y_title Optional y-axis title. Default is empty.
#' @param colors Color palette for bars. Default is c("#234399", "#B52049", "grey50", "#8FAADC").
#' @param width Chart width in pixels. Default is 800.
#' @param height Chart height in pixels. Default is 600.
#'
#' @return A plotly object
#'
#' @details
#' The function automatically adjusts spacing and layout based on whether subtitle and color
#' grouping are used. It applies standardized formatting for titles, annotations, and legend
#' positioning. When `color` is not specified, all bars will adopt the first color from the
#' `colors` vector.
#'
#' @examples
#' \dontrun{
#' # Basic usage with single line
#' lfs_plotly_bar(
#'   data = unemployment_data,
#'   x = "DATE",
#'   y = "RATE",
#'   title = "Unemployment Rate"
#' )
#'
#' # With color grouping and subtitle
#' lfs_plotly_bar(
#'   data = unemployment_data,
#'   x = "DATE",
#'   y = "RATE",
#'   color = "GENDER",
#'   title = "Unemployment Rate by Gender",
#'   subtitle = "Monthly rates, seasonally adjusted"
#' )
#' }
#'
#' @importFrom plotly plot_ly layout %>%
#' @importFrom stats as.formula
#' @export
lfs_plotly_bar <- function(
    data,
    x,
    y,
    color = NULL, # Optional color parameter
    title = "Chart ",
    subtitle = "", # Default to empty string
    source = "",
    y_range = NULL, # Set to NULL by default
    y_title = "", # Default y-axis title is blank
    colors = c("#234399", "#B52049", "grey50", "#8FAADC"),
    width = 800,
    height = 600) {
  # Generate formula expressions from column names
  x_formula <- as.formula(paste0("~", x))
  y_formula <- as.formula(paste0("~", y))

  # Create the basic yaxis settings
  yaxis_settings <- list(title = y_title)
  # Add range only if it's provided
  if (!is.null(y_range)) {
    yaxis_settings$range <- y_range
  }

  # Initialize annotations list as empty
  annotations_list <- list()
  # Add source annotation only if 'source' parameter is not blank
  if (source != "") {
    source_annotation <- list(
      text = paste0("<b>Source: </b>", source),
      font = list(family = "Arial", size = 15),
      x = -0.059, y = -0.2, xref = "paper", yref = "paper", xanchor = "left", showarrow = FALSE
    )
    annotations_list <- c(annotations_list, list(source_annotation))
  }


  # Determine margin based on subtitle and color presence
  has_subtitle <- subtitle != ""
  has_color <- !is.null(color)
  # subtitle_margin <- 1.19

  # Set margin based on combination of factors
  if (has_subtitle && has_color) {
    top_margin <- 165 # Both subtitle and color (needs most space)
    subtitle_margin <- 1.33
  } else if (!has_subtitle && !has_color) {
    top_margin <- 80 # Neither subtitle nor color (needs least space)
  } else if (has_subtitle && !has_color) {
    top_margin <- 110 # subtitle but no color
    subtitle_margin <- 1.14
  } else {
    top_margin <- 140 # color but no subtitle
  }
  plot_margin <- list(l = 50, r = 50, t = top_margin, b = 100, pad = 20)

  # Add subtitle annotation only if it's not empty
  if (has_subtitle) {
    subtitle_annotation <- list(
      text = subtitle,
      font = list(family = "Arial", size = 15),
      x = -0.059, y = subtitle_margin, xref = "paper", yref = "paper", xanchor = "left", showarrow = FALSE
    )
    annotations_list <- c(list(subtitle_annotation), annotations_list)
  }

  # Create the plot based on whether color is provided
  if (!has_color) {
    # No color provided - create simple bar plot
    p <- plot_ly(
      data = data,
      x = x_formula,
      y = y_formula,
      marker = list(color = colors[1], line = list(color = "black", width = .5)), # Explicitly set color here
      type = "bar", # Changed to bar chart
      width = width,
      height = height
    )
  } else {
    # Color provided - create color-based bar plot
    color_formula <- as.formula(paste0("~", color))
    p <- plot_ly(
      data = data,
      x = x_formula,
      y = y_formula,
      color = color_formula,
      colors = colors,
      marker = list(line = list(color = "black", width = .5)),
      type = "bar", # Changed to bar chart
      width = width,
      height = height
    )
  }

  # Add layout settings to the plot
  p %>%
    layout(
      title = list(
        text = paste0("<b>", title, "</b>"),
        font = list(family = "Arial", size = 20),
        x = 0.02, y = 0.95, xanchor = "left"
      ),
      annotations = annotations_list,
      xaxis = list(
        title = "",
        # For bar charts with dates, a slight range padding might still be useful,
        # but often it's best to let plotly auto-determine for categorical x-axes.
        # If x is truly a date column, keeping the -15/+15 range might still be
        # desired to ensure labels are visible.
        range = c(min(data[[x]]) - 25, max(data[[x]]) + 25),
        showgrid = FALSE
      ),
      yaxis = yaxis_settings,
      legend = list(
        orientation = "h",
        x = -0.005,
        y = 1.2,
        font = list(family = "Arial", size = 15),
        categoryorder = "trace",
        traceorder = "factor"
      ),
      margin = plot_margin
    )
}
