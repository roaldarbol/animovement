#' Visualize Distribution of Missing Values Across Time
#'
#' @description
#' Creates a stacked barplot visualization of missing values across time intervals.
#' Particularly useful for examining patterns in missing data across large datasets.
#' Adapted from the `ggplot_na_distribution2` function in the imputeTS package
#' (Moritz & Gatscha, GPL-3).
#'
#' @param data Numeric vector containing NAs.
#' @param number_intervals Integer specifying number of bins. Default (NULL) uses
#'   Sturges' formula via `nclass.Sturges()`. Ignored if interval_size is set.
#' @param interval_size Integer specifying number of observations per bin. If set,
#'   overrides number_intervals.
#' @param measure Character string specifying display units:
#'   - `"percent"`: Show as percentages (default)
#'   - `"count"`: Show absolute numbers
#' @param color_missing Color for missing values (default: "indianred2")
#' @param color_existing Color for existing values (default: "steelblue")
#' @param alpha_missing Transparency for missing values (default: 0.8)
#' @param alpha_existing Transparency for existing values (default: 0.3)
#' @param title Plot title (default: "Missing Values per Interval")
#' @param subtitle Plot subtitle (default: "Amount of NA and non-NA for successive intervals")
#' @param xlab X-axis label (default shows interval size)
#' @param ylab Y-axis label (automatically set based on measure)
#' @param color_border Color for interval borders (default: "white")
#' @param theme ggplot2 theme (default: theme_linedraw())
#' @param keypoint Optional keypoint name for subtitle
#'
#' @return A ggplot2 object that can be further customized using ggplot2 syntax
#'
#' @details
#' This function creates a stacked barplot showing the distribution of missing values
#' across time. Instead of showing individual observations, it groups them into
#' intervals/bins, making it easier to visualize patterns in large datasets.
#'
#' The size of intervals can be controlled either by specifying the total number
#' of intervals (`number_intervals`) or by setting a fixed number of observations
#' per interval (`interval_size`).
#'
#' @examples
#' \dontrun{
#' # Basic usage with default settings
#' ggplot_na_timing(movement_data)
#'
#' # Show counts instead of percentages
#' ggplot_na_timing(movement_data, measure = "count")
#'
#' # Custom interval size
#' ggplot_na_timing(movement_data, interval_size = 300)
#'
#' # Customize colors and transparency
#' ggplot_na_timing(movement_data,
#'                  color_missing = "red",
#'                  color_existing = "blue",
#'                  alpha_missing = 0.6)
#'
#' # Add custom ggplot2 modifications
#' ggplot_na_timing(movement_data) +
#'   theme(axis.text.x = element_text(angle = 45))
#' }
#'
#' @importFrom grDevices nclass.Sturges
#' @importFrom ggplot2 theme_linedraw alpha ggplot aes scale_fill_manual
#' theme element_blank scale_x_continuous scale_y_continuous
#' labs xlab ylab stat_bin after_stat theme_classic
#' @importFrom ggtext element_markdown
#'
#' @note
#' This function is adapted from the imputeTS package (version 3.3) by
#' Steffen Moritz and Sebastian Gatscha, available under GPL-3 license.
#'
#' @keywords internal
ggplot_na_timing <- function(data,
                             number_intervals = NULL,
                             interval_size = NULL,
                             measure = "percent",
                             color_missing = "indianred2",
                             color_existing = "steelblue",
                             alpha_missing = 0.8,
                             alpha_existing = 0.3,
                             title = "Missing Values per Interval",
                             subtitle = "Amount of NA and non-NA for successive intervals",
                             xlab = "Time Lapse (Interval Size: XX)",
                             ylab = NULL,
                             color_border = "white",
                             theme = ggplot2::theme_linedraw(),
                             keypoint = NULL) {

  ##
  ## 1. Input Check and Transformation
  ##

  # 1.1 special handling data types
  if (any(class(data) == "tbl_ts")) {
    data <- as.vector(as.data.frame(data)[, 2])
  }
  else if (any(class(data) == "tbl")) {
    data <- as.vector(as.data.frame(data)[, 1])
  }

  # 1.2 Check if the input is multivariate
  if (!is.null(dim(data)[2]) && dim(data)[2] > 1) {
    stop("x is not univariate. The function only works with univariate
    input for x. For data types with multiple variables/columns only input
    the column you want to plot as parameter x.")
  }


  # 1.3 Checks and corrections for wrong data dimension

  # Altering multivariate objects with 1 column (which are essentially
  # univariate) to be dim = NULL
  if (!is.null(dim(data)[2])) {
    data <- data[, 1]
  }


  # 1.4 Input as vector
  data <- as.vector(data)


  # 1.5 Check if input is numeric
  if (!is.numeric(data)) {
    stop("Input x is not numeric")
  }


  # 1.6 Check preconditions about amount of NAs

  # exclude NA only inputs
  missindx <- is.na(data)
  if (all(missindx)) {
    stop("Input data consists only of NAs. At least one non-NA numeric value is needed
    for creating a meaningful ggplot_na_distribution plot)")
  }


  ##
  ## End Input Check and Transformation
  ##



  ##
  ## 2. Preparations
  ##

  # 2.1 Calculation default number of intervals
  if (is.null(number_intervals)) {
    number_intervals <- grDevices::nclass.Sturges(data)
  }


  # 2.2 Calculation break points

  if (!is.null(interval_size)) {
    breaks <- seq(from = 0, to = length(data) - 1, by = interval_size)
    breaks <- c(breaks, length(data))
  }
  else {
    breaks <- seq(from = 0, to = length(data) - 1, by = floor(length(data) / number_intervals))
    breaks <- c(breaks, length(data))
  }
  binwidth <- breaks[2]


  # 2.3 Process parameter settings

  # Add alpha values to colors
  color_missing <- ggplot2::alpha(color_missing, alpha_missing)
  color_existing <- ggplot2::alpha(color_existing, alpha_existing)

  # Set subtitle to default
  # (needed because .Rd usage section gives error when using defaults > 90 chars )
  if ( (!is.null(subtitle)) && (subtitle == "Amount of NA and non-NA for successive intervals" && !is.null(keypoint))) {
    subtitle <- paste0("Amount of <b style='color:", color_missing, ";' >NA</b>
                and  <b style='color:", color_existing, "' >non-NA</b>
                for successive intervals for keypoint=", keypoint)
  } else if ((is.null(subtitle) && !is.null(keypoint))){
    subtitle <- paste0(keypoint)
  }

  # Set ylab according to choosen measure
  if (is.null(ylab)) {
    ifelse(measure == "percent", ylab <- "Percent", ylab <- "Count")
  }

  # Set xlab according to choosen parameters
  if (xlab == "Time Lapse (Interval Size: XX)") {
    xlab <- paste0("Frame Number (Interval Size: ", binwidth, ")")
  }


  # 2.4 Create dataframe for ggplot2

  index <- seq_along(data)
  miss <- as.factor(is.na(data))
  df <- data.frame(index, miss)

  ##
  ## End Preparations
  ##


  ##
  ## 3. Create the ggplot2 plot
  ##

  # Create the ggplot2 plot
  gg <- ggplot2::ggplot(df, ggplot2::aes(index, fill = miss)) +
    ggplot2::scale_fill_manual(
      values = c(color_existing, color_missing),
      labels = c("NAs", "non-NAs")
    ) +
    theme +
    ggplot2::theme(
      legend.position = "none",
      legend.title = ggplot2::element_blank(),
      plot.subtitle = ggtext::element_markdown(),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
    ) +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::labs(title = title, subtitle = subtitle) +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab)

  count <- NULL
  if (measure == "percent") {
    gg <- gg + ggplot2::stat_bin(ggplot2::aes(y = ggplot2::after_stat(count / binwidth)),
                                 col = color_border, breaks = breaks, closed = "right"
    ) +
      ggplot2::scale_y_continuous(expand = c(0, 0), labels = function(x) paste0(x*100, "%"))
  }
  else {
    gg <- gg + ggplot2::stat_bin(ggplot2::aes(y = ggplot2::after_stat(count)),
                                 col = color_border, breaks = breaks, closed = "right"
    ) +
      ggplot2::scale_y_continuous(expand = c(0, 0))
  }

  return(gg)
}
