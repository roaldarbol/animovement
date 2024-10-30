#' Read idtracker.ai data
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' @param path Path to an idtracker.ai data frame
#'
#' @import dplyr
#' @import tidyr
#' @importFrom vroom vroom
#' @importFrom janitor clean_names
#'
#' @return a movement dataframe
#' @export
read_idtracker <- function(path) {
  data <- vroom::vroom(
    path,
    delim = ",",
    show_col_types = FALSE
  ) |>
    suppressMessages() |>
    janitor::clean_names() |>
    tidyr::pivot_longer(cols = !1,
                 values_to = "value",
                 names_to = c("axis", "id"),
                 names_pattern = "(.)(.)") |>
    dplyr::mutate(id = as.factor(.data$id)) |>
    tidyr::pivot_wider(id_cols = c("id", "seconds"),
                values_from = "value",
                names_from = "axis") |>
    dplyr::rename(time = "seconds")

  return(data)
}
