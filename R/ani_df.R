new_ani_df <- function(x, metadata) {
  class(x) <- c("ani_df", class(x))
  x <- init_metadata(x)

  # Return the data frame
  x
}

#' @export
ani_df <- function(
    ...,
    metadata = list(),
    .rows = NULL,
    .name_repair = c("check_unique", "unique", "universal", "minimal")
  ) {

  # Create using tibble
  df <- tibble(..., .rows = .rows, .name_repair = .name_repair)

  # Check columns
  if (!"x" %in% names(df) && !"y" %in% names(df) && !"z" %in% names(df)){
    cli::cli_abort("Missing positional data. Make sure to include x/y/z at least.")
  }

  if (!"time" %in% names(df)){
    cli::cli_abort("Missing a time column.")
  }

  if (!"keypoint" %in% names(df)){
    cli::cli_alert_info("No 'keypoint' column was found. The column was created and was filled with 'centroid'.")
    df <- df |>
      dplyr::mutate(keypoint = factor("centroid"))
  } else {
    df <- df |>
      dplyr::mutate(keypoint = factor(keypoint))
  }

  if (!"individual" %in% names(df)){
    cli::cli_alert_info("No 'individual' column was found. The column was created and was filled with 'NA'.")
    df <- df |>
      dplyr::mutate(individual = factor(NA))
  } else {
    df <- df |>
      dplyr::mutate(individual = factor(individual))
  }

  if ("trial" %in% names(df)){
    df <- df |>
      dplyr::mutate(trial = as.integer(trial))
  }

  if ("session" %in% names(df)){
    df <- df |>
      dplyr::mutate(session = as.integer(session))
  }

  # Relocate columns
  df <- df |>
    dplyr::relocate(dplyr::any_of(c("session", "trial", "individual", "keypoint", "time", "x", "y", "z", "confidence")))

  # Group data frame
  potential_groups <- c("session", "trial", "individual", "keypoint")
  groupings <- potential_groups[potential_groups %in% names(df)]
  group_syms <- rlang::syms(groupings)
  df <- df |>
    dplyr::group_by(!!!group_syms) |>
    dplyr::arrange(.by_group = TRUE)

  # Make as ani_df
  new_ani_df(df)
}

as_ani_df <- function(
    data
) {
  ani_df(data)
}

is_ani_df <- function(x) {
  inherits(x, "ani_df")
}

example_tbl <- function(class = NULL) {
  ani_df(
    individual   = rep(1:2, each = 25),
    time = rep(1:10, 5),
    x    = rnorm(50),
    y    = rnorm(50),
    trial = 1
  )
}

#' @importFrom pillar tbl_sum
#' @export
tbl_sum.ani_df <- function(x, ...) {
  default_header <- NextMethod()

  # Count unique values in key columns
  n_individuals <- length(unique(x$individual))
  n_keypoints <- length(unique(x$keypoint))

  # Start with default header
  # new_header <- default_header

  # Add individuals and keypoints (always present)
  new_header <- c(
    # new_header,
    "Individuals" = paste(unique(x$individual), collapse = ", "),
    "Keypoints" = paste(unique(x$keypoint), collapse = ", ")
  )

  # Add sessions if column exists
  if ("session" %in% names(x)) {
    n_sessions <- length(unique(x$session))
    new_header <- c(new_header, "Sessions" = as.character(n_sessions))
  }

  # Add trials if column exists
  if ("trial" %in% names(x)) {
    new_header <- c(new_header, "Trials" = paste(unique(x$trial), collapse = ", "))
  }

  # Sampling rate
  if (!is.na(get_metadata(x)$sampling_rate) && !is.null(get_metadata(x)$sampling_rate)) {
    new_header <- c(new_header, "Sampling rate" = paste(get_metadata(x)$sampling_rate, "Hz"))
  }

  # Add default header
  # new_header <- c(new_header, default_header)

  new_header
}

# ----------
# Dplyr methods, preserve class
#' @importFrom dplyr ungroup
#' @export
ungroup.ani_df <- function(x, quiet = FALSE, ...) {
  if (!quiet) {
    cli::cli_warn("Ungrouping an ani_df data frame makes errors more likely. Proceed with care.")
  }

  # Remove ani_df class temporarily and call ungroup
  class(x) <- setdiff(class(x), "ani_df")
  x <- dplyr::ungroup(x, ...)

  # Restore ani_df class
  class(x) <- c("ani_df", class(x))
  x
}

#' @importFrom dplyr group_by
#' @export
group_by.ani_df <- function(.data, ..., .add = FALSE, .drop = TRUE) {
  x <- NextMethod()
  class(x) <- c("ani_df", class(x))
  x
}

#' @importFrom dplyr mutate
#' @export
mutate.ani_df <- function(.data, ...) {
  x <- NextMethod()
  class(x) <- c("ani_df", class(x))
  x
}

#' @importFrom dplyr select
#' @export
select.ani_df <- function(.data, ...) {
  x <- NextMethod()
  class(x) <- c("ani_df", class(x))
  x
}

#' @importFrom dplyr filter
#' @export
filter.ani_df <- function(.data, ..., .preserve = FALSE) {
  x <- NextMethod()
  class(x) <- c("ani_df", class(x))
  x
}

#' @importFrom dplyr arrange
#' @export
arrange.ani_df <- function(.data, ..., .by_group = FALSE) {
  x <- NextMethod()
  class(x) <- c("ani_df", class(x))
  x
}

#' @importFrom dplyr rename
#' @export
rename.ani_df <- function(.data, ...) {
  x <- NextMethod()
  class(x) <- c("ani_df", class(x))
  x
}

#' @importFrom dplyr relocate
#' @export
relocate.ani_df <- function(.data, ..., .before = NULL, .after = NULL) {
  x <- NextMethod()
  class(x) <- c("ani_df", class(x))
  x
}

#' @importFrom dplyr slice
#' @export
slice.ani_df <- function(.data, ..., .preserve = FALSE) {
  x <- NextMethod()
  class(x) <- c("ani_df", class(x))
  x
}
#' @export
`[.ani_df` <- function(x, i, j, ..., drop = FALSE) {
  class(x) <- setdiff(class(x), "ani_df")
  x <- NextMethod()
  class(x) <- c("ani_df", class(x))
  x
}

#' @export
`[[.ani_df` <- function(x, i, ...) {
  class(x) <- setdiff(class(x), "ani_df")
  x <- NextMethod()
  if (is.data.frame(x)) {
    class(x) <- c("ani_df", class(x))
  }
  x
}

#' @export
`$.ani_df` <- function(x, name) {
  class(x) <- setdiff(class(x), "ani_df")
  NextMethod()
}

#' @export
`[<-.ani_df` <- function(x, i, j, ..., value) {
  class(x) <- setdiff(class(x), "ani_df")
  x <- NextMethod()
  class(x) <- c("ani_df", class(x))
  x
}

#' @export
`[[<-.ani_df` <- function(x, i, ..., value) {
  class(x) <- setdiff(class(x), "ani_df")
  x <- NextMethod()
  class(x) <- c("ani_df", class(x))
  x
}

#' @export
`$<-.ani_df` <- function(x, name, value) {
  class(x) <- setdiff(class(x), "ani_df")
  x <- NextMethod()
  class(x) <- c("ani_df", class(x))
  x
}

#' @export
`names<-.ani_df` <- function(x, value) {
  class(x) <- setdiff(class(x), "ani_df")
  x <- NextMethod()
  class(x) <- c("ani_df", class(x))
  x
}

# Conversion method
#' @export
as.data.frame.ani_df <- function(x, ...) {
  class(x) <- setdiff(class(x), "ani_df")
  NextMethod()
}

# print.ani_df <- function(x, ...) {
#   cat(format(x, ...), "\n")
#   invisible(x)
# }
# summary.ani_df    <- function(object, ...) { ... }
# plot.ani_df       <- function(x, ...) { ... }
