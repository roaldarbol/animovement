coords <- data.frame(
  keypoint = rep("centroid", 4),
  rho = c(1,1,1,1),
  theta = c(pi*0.25, pi*0.75, pi*1.25, pi*1.75), digits = 5
  )

# Test coordinate mappings
test_that("Test coordinate mappings", {
  # Test theta
  expect_equal(
    coords |>
      dplyr::mutate(x = polar_to_x(rho, theta),
                    y = polar_to_y(rho, theta),
                    theta_new = cartesian_to_theta(x, y)) |>
      dplyr::pull(theta_new),
    expected = coords$theta)

  # Test theta range, non-centered
  c_two_pi <- coords |>
    dplyr::mutate(x = polar_to_x(rho, theta),
                  y = polar_to_y(rho, theta),
                  theta_new = cartesian_to_theta(x, y, centered = FALSE)) |>
    dplyr::pull(theta_new)
  expect_true(all(between(c_two_pi, 0, 2*pi)))

  # Test theta range, centered
  c_centered <- coords |>
    dplyr::mutate(x = polar_to_x(rho, theta),
                  y = polar_to_y(rho, theta),
                  theta_new = cartesian_to_theta(x, y, centered = TRUE)) |>
    dplyr::pull(theta_new)
  expect_true(all(between(c_centered, -pi, pi)))

  # Test rho
  expect_identical(
    coords |>
      dplyr::mutate(x = polar_to_x(rho, theta),
                    y = polar_to_y(rho, theta),
                    rho_new = cartesian_to_rho(x, y)) |>
      dplyr::pull(rho_new),
    expected = coords$rho)
})

# On whole data frames
test_that("Test coordinate mappings", {
  # Test theta
  expect_equal(
    coords |>
      convert_polar_to_cartesian() |>
      convert_cartesian_to_polar(),
    expected = coords)
})
