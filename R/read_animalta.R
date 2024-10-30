#' Read AnimalTA data
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' @param path An AnimalTA data frame
#'
#' @return a movement dataframe
#' @export
read_animalta <- function(path) {
  data <- vroom::vroom(
    path,
    delim = ";",
    show_col_types = FALSE
  ) |>
    janitor::clean_names() |>
    dplyr::mutate(frame = as.numeric(frame),
                  time = as.numeric(time)) |>
    dplyr::rename(individual = "ind") |>
    dplyr::select(-c("frame", "arena"))
  cli::cli_abort("`read_animalta` has not yet been implemented. Coming soon!")
}
# path <- here::here("tests", "data", "animalta", "single_individual_multi_arena.csv")
# path <- here::here("tests", "data", "animalta", "variable_individuals_single_arena.csv")
