#' Visualize NA Gap Sizes in Time Series Data
#'
#' @description
#' Creates a visualization of the frequency of different NA gap sizes (consecutive NAs)
#' in time series data. Adapted from the `ggplot_na_gapsize` function in the
#' imputeTS package (Moritz & Gatscha, GPL-3).
#'
#' @param data Numeric vector containing NAs.
#' @param limit Integer specifying how many of the most common gap sizes to show
#'   (default: 10).
#' @param include_total Logical. If TRUE, shows total NA count for each gap size
#'   (gap size × frequency) (default: TRUE).
#' @param ranked_by Character string specifying sorting method. Options:
#'   - `"occurrence"`: Sort by frequency of gap size (default)
#'   - `"total"`: Sort by total number of NAs (frequency × gap size)
#' @param color_occurrence Color for frequency bars (default: "indianred")
#' @param color_total Color for total NA bars (default: "steelblue")
#' @param color_border Color for bar borders (default: "black")
#' @param alpha_bars Transparency value for bars (default: 1)
#' @param title Plot title (default: "Occurrence of gap sizes")
#' @param subtitle Plot subtitle (default: "Gap sizes (NAs in a row) ordered by most common")
#' @param xlab X-axis label (default: NULL)
#' @param ylab Y-axis label (default: "Number occurrence")
#' @param legend Logical. If TRUE, shows legend (default: TRUE)
#' @param orientation Character string. Either "vertical" or "horizontal" (default: "horizontal")
#' @param label_occurrence Legend label for frequency bars (default: "Number occurrence gapsize")
#' @param label_total Legend label for total bars (default: "Resulting NAs for gapsize")
#' @param theme ggplot2 theme (default: theme_linedraw())
#' @param keypoint Optional keypoint name for subtitle
#'
#' @return A ggplot2 object that can be further customized using ggplot2 syntax
#'
#' @details
#' This function visualizes the distribution of NA gaps in your data, showing how
#' often gaps of different lengths occur. For example, it might show that:
#' - 2-NA gaps occur 27 times
#' - 9-NA gaps occur 11 times
#' - 27-NA gaps occur 1 time
#'
#' The plot can be customized using the function parameters or by adding
#' additional ggplot2 layers to the returned object.
#'
#' @examples
#' \dontrun{
#' # Basic usage with default settings
#' ggplot_na_gapsize(movement_data)
#'
#' # Show top 20 gap sizes horizontally
#' ggplot_na_gapsize(movement_data, limit = 20, orientation = "horizontal")
#'
#' # Sort by total NAs and change colors
#' ggplot_na_gapsize(movement_data,
#'                   ranked_by = "total",
#'                   color_occurrence = "darkred",
#'                   color_total = "navy")
#'
#' # Customize using ggplot2
#' ggplot_na_gapsize(movement_data) +
#'   theme(legend.position = "right") +
#'   ggtitle("Custom Title")
#' }
#'
#' @importFrom ggplot2 theme_linedraw ggplot geom_bar position_dodge aes scale_x_discrete
#' scale_fill_manual ggtitle xlab ylab theme element_text element_blank
#' coord_flip theme_classic
#'
#' @note
#' This function is adapted from the imputeTS package (version 3.3) by
#' Steffen Moritz and Sebastian Gatscha, available under GPL-3 license.
#'
#' @export
ggplot_na_gapsize <- function(data,
                              limit = 10,
                              include_total = TRUE,
                              ranked_by = "occurrence",
                              color_occurrence = "indianred",
                              color_total = "steelblue",
                              color_border = "black",
                              alpha_bars = 1,
                              title = "Occurrence of gap sizes",
                              subtitle = "Gap sizes (NAs in a row) ordered by most common",
                              xlab = NULL,
                              ylab = "Number occurrence",
                              legend = TRUE,
                              orientation = "horizontal",
                              label_occurrence = "Number occurrence gapsize",
                              label_total = "Resulting NAs for gapsize",
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
    for creating a meaningful ggplot_na_gapsize plot)")
  }

  # exclude inputs without NAs
  if (!anyNA(data)) {
    stop("Input data contains no NAs. At least one missing value is needed
         to create a meaningful ggplot_na_gapsize plot)")
  }



  ##
  ## End Input Check and Transformation
  ##



  ##
  ## 2. Preparations
  ##


  # 2.1 Create required data


  # Calculation consecutive NA information
  rle_na <- base::rle(is.na(data))
  vec <- rle_na$lengths[rle_na$values]
  occurrence_bar <- table(vec)
  gaps_vec <- as.integer(names(occurrence_bar))
  totals_bar <- occurrence_bar * gaps_vec
  labels1 <- paste0(gaps_vec, " NA-gap")



  # 2.2 Adjust to parameter selection by user

  # Sorting for ranked_by param
  if (ranked_by == "occurrence") {
    # sort according to occurrence of gapsizes
    fooind <- order(occurrence_bar)
    occurrence_bar <- occurrence_bar[fooind]
    totals_bar <- totals_bar[fooind]
    labels1 <- labels1[fooind]
  } else if (ranked_by == "total") {
    # sort accoding to total NAs
    fooind <- order(totals_bar)
    occurrence_bar <- occurrence_bar[fooind]
    totals_bar <- totals_bar[fooind]
    labels1 <- labels1[fooind]
  }
  else {
    stop("Wrong input for parameter ranked_by. Input must be either 'occurrence' or 'total'.
    Call ?ggplot_na_gapsize to view the documentation.")
  }

  # Adjust to show only a limited amount of bars for limit param
  if (length(occurrence_bar) > limit) {
    occurrence_bar <- occurrence_bar[(length(occurrence_bar) - limit + 1):length(occurrence_bar)]
    totals_bar <- totals_bar[(length(totals_bar) - limit + 1):length(totals_bar)]
    labels1 <- labels1[(length(labels1) - limit + 1):length(labels1)]
  }



  # 2.3 Create dataframe for ggplot2

  # data.frame for ggplot
  id <- seq_along(occurrence_bar)
  val <- c(occurrence_bar, totals_bar)
  label <- c(
    rep("occurrence_bar", length(occurrence_bar)),
    rep("totals_bar", length(totals_bar))
  )
  df <- data.frame(id, val, label)

  # Only number of occurrences bar
  if (include_total == FALSE) {
    df <- subset(df, label == "occurrence_bar")
  }

  ##
  ## End Preparations
  ##



  ##
  ## 3. Create the ggplot2 plot
  ##


  # Create ggplot
  gg <- ggplot2::ggplot(data = df) +
    ggplot2::geom_bar(aes(x = id, y = val, fill = label),
                      color = color_border,
                      width= 0.6,
                      alpha = alpha_bars,
                      stat = "identity", position = position_dodge(width = 0.7)
    ) +
    ggplot2::scale_x_discrete(
      labels = labels1,
      limits = labels1
    ) +
    ggplot2::scale_fill_manual(
      values = c(color_occurrence, color_total),
      labels = c(label_occurrence, label_total),
    ) +
    ggplot2::ggtitle(title, subtitle = keypoint) +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab) +
    theme +
    ggplot2::theme(
      legend.position = "bottom",
      axis.text.x = ggplot2::element_text(angle = 30, hjust = 1),
      legend.title = ggplot2::element_blank()
    )

  # For flipping from vertical to horizontal bars
  if (orientation == "horizontal") {
    gg <- gg + ggplot2::coord_flip()
  }

  # Removing legend
  if (!legend) {
    gg <- gg +
      ggplot2::theme(
        legend.position = "none",
      )
  }


  ##
  ##  End creating the ggplot2 plot
  ##


  return(gg)
}
