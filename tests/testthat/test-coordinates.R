coords <- data.frame(
  keypoint = rep("centroid", 4),
  rho = c(1,1,1,1),
  theta = c(pi*0.25, pi*0.75, pi*1.25, pi*1.75), digits = 5
  )

# Test basic functions
test_that("Test coordinate mappings", {
  # Radians to degrees
  expect_equal(rad_to_deg(pi), 180)
  expect_equal(rad_to_deg(0), 0)
  expect_equal(rad_to_deg(pi/2), 90)
  expect_equal(rad_to_deg(pi*1.5), 270)

  # Degrees to radians
  expect_equal(deg_to_rad(180), pi)
  expect_equal(deg_to_rad(0), 0)
  expect_equal(deg_to_rad(90), pi/2)
  expect_equal(deg_to_rad(270), pi*1.50)

  # Needs some tests with negative values
  # expect_equal(deg_to_rad(-90), pi*1.50)
})


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
      map_to_cartesian() |>
      map_to_polar(),
    expected = coords)
})
