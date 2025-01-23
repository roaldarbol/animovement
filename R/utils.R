.find_nearest <- function(x, v) {
  which.min(abs(v - x))
}

.get_file_ext <- function(filename) {
  nameSplit <- strsplit(x = filename, split = "\\.")[[1]]
  return(nameSplit[length(nameSplit)])
}

.is.POSIXt <- function(x) inherits(x, "POSIXt")

.scale_values <- function(data, variables, scaling_factor) {
  # Adjust distances for mouse sensor "dots-per-cm"
  if (!is.null(scaling_factor)) {
    data <- data |>
      dplyr::mutate(across(all_of(variables), ~ .x / scaling_factor))
  }
}

.has_attributes <- function(data, attributes){
  attributes %in% names(attributes(data))
}

.has_all_attributes <- function(data, attributes){
  attributes %in% names(attributes(data)) |>
    all()
}

convert_nan_to_na <- function(data){
  dplyr::mutate(data, across(where(is.numeric), function(x) ifelse(is.nan(x), NA, x)))
}

# For TRex files
get_individual_from_path <- function(path){
  strsplit(tools::file_path_sans_ext(basename(path)), "_(?!.*_)", perl=TRUE)[[1]]
}


.update_version <- function(type){
  usethis::use_version(which = type)
  cff_write(keys = list("date-released" = Sys.Date(),
                        "doi" = desc::desc_get("DOI"),
                        "preferred-citation" = NULL),
            r_citation = TRUE,
            dependencies = FALSE)
}
