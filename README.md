# trackballr

<!-- badges: start -->
[![Codecov test coverage](https://codecov.io/gh/roaldarbol/trackballr/branch/main/graph/badge.svg)](https://app.codecov.io/gh/roaldarbol/trackballr?branch=main)
<!-- badges: end -->

The goal of {trackballr} is to make analysis of trackball data easy.

## Installation

You can install the development version of trackballr from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("roaldarbol/trackballr")
```
or from my -universe:
```
install.packages("trackballr", repos = c("https://roaldarbol.r-universe.dev", "https://cran.r-project.org"))
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(trackballr)

## Read all files into list
data_list <- read_trackball_data(folder_path, sensors = c("Right", "Left"))

# Augment all data in list
data_list <- lapply(data_list, beetle_augment)

# Bind list into single data frame
data_df <- bind_rows(sensor_data_temp)
```

Once the data has been pre-processed, it can then easily generate useful summaries - here are some examples.
```r
# Compute translational summary
sensor_summary <- sensor_data_df |> 
  na.omit()  |> 
  group_by(id, date) |> 
  filter(abs(x) > 0 | abs(y) > 0) |> # Only keep rows containing movement
  summarise(total_translation = sum(distance),
            v_translation_mean = mean(v_translation),
            v_translation_sd = sd(v_translation),
            sinuosity = sqrt(last(cum_x)^2 + last(cum_y)^2)/sum(distance)
            ) |> 
  mutate(trial = if_else(date <= min(date), "first", "second")) |> # Each animal has been on the trackball on two days - here we assign which day/trial
  slice(1:2) |> 
  filter(total_translation > 0) |> # Filter away trials with no translation
  filter(n() == 2) # Keep only observations where both trials are present

# Compute rotational summary
# Here wa are also filtering out trials with a faulty sensor/no data for one sensor
rotation_summary <- sensor_data_df |> 
  na.omit() |>
  group_by(id, date) |> 
  filter(v_rotation > 0) |>
  summarise(total_rotation = sum(rotation, na.rm = TRUE),
            v_rotation_mean = mean(v_rotation, na.rm = TRUE)) |>
  mutate(trial = if_else(date <= min(date), "first", "second")) |> 
  slice(1:2) |>
  filter(n() == 2)
```
