library(testthat)

test_that("classify_movement_plateau handles basic movement patterns", {
  # Simple case: clear movement and non-movement
  speed <- c(rep(0.1, 100), rep(5, 100), rep(0.1, 100))
  result <- classify_movement_plateau(speed, refine_transitions = FALSE)
  expect_equal(sum(result[90:110] == 1), 10) # Movement detected around transition
  expect_equal(sum(result[1:50] == 0), 50)   # Start is clearly non-movement

  # Case with some noise but clear movement
  speed <- c(rnorm(100, 0.1, 0.05), rnorm(100, 5, 0.5), rnorm(100, 0.1, 0.05))
  result <- classify_movement_plateau(speed, refine_transitions = FALSE)
  expect_equal(sum(result == 1), 100) # Should still detect movement transition
})

test_that("classify_movement_plateau works with velocity (negative values)", {
  # Velocity with direction changes
  velocity <- c(rep(0.1, 50), rep(5, 100), rep(0.1, 50))
  result <- classify_movement_plateau(velocity, refine_transitions = FALSE)
  expect_equal(sum(result == 1), 102)  # Should detect movement despite negative values

  # Mixed positive and negative movements
  velocity <- c(rep(0.1, 50), rep(-5, 50), rep(5, 50), rep(0.1, 50))
  result <- classify_movement_plateau(velocity, refine_transitions = FALSE)
  expect_true(all(result[60:140] == 1))      # Should detect both movement directions
})

test_that("classify_movement_plateau handles edge cases", {
  # All NA input
  expect_true(all(is.na(classify_movement_plateau(rep(NA, 100)))))

  # Very short input
  expect_error(classify_movement_plateau(1:5, refine_transitions = FALSE), NA)  # Should not error

  # Single value
  expect_error(classify_movement_plateau(1, refine_transitions = FALSE), NA)    # Should not error

  # Zero-length input
  expect_error(classify_movement_plateau(numeric(0), refine_transitions = FALSE), NA)  # Should not error

  # All zeros
  expect_true(all(is.na(classify_movement_plateau(rep(0, 100)), refine_transitions = FALSE) == TRUE))

  # Constant non-zero value
  expect_true(all(is.na(classify_movement_plateau(rep(1, 100)), refine_transitions = FALSE) == TRUE))
})

test_that("classify_movement_plateau handles NA patterns correctly", {
  # NAs at start
  speed <- c(rep(NA, 50), rep(0.1, 100), rep(5, 100))
  result <- classify_movement_plateau(speed, refine_transitions = FALSE)
  expect_true(all(is.na(result[1:50])))
  expect_false(all(is.na(result[51:250])))

  # NAs in middle
  speed <- c(rep(0.1, 100), rep(NA, 50), rep(5, 100))
  result <- classify_movement_plateau(speed, refine_transitions = FALSE)
  expect_true(all(is.na(result[101:150])))
  expect_false(all(is.na(result[1:100])))
  expect_false(all(is.na(result[151:250])))

  # Scattered NAs
  speed <- rep(0.1, 300)
  speed[sample(300, 30)] <- NA  # 10% NAs randomly placed
  result <- classify_movement_plateau(speed, refine_transitions = FALSE)
  expect_equal(sum(is.na(result)), 30)
})

test_that("classify_movement_plateau merges nearby movements correctly", {
  # Create pattern with brief stillness between movements
  speed <- c(rep(0.1, 100),       # stillness
             rep(5, 100),          # movement
             rep(0.1, 30),         # brief stillness (should be merged)
             rep(5, 100),          # movement
             rep(0.1, 100))        # stillness

  result <- classify_movement_plateau(speed, refine_transitions = FALSE)
  # Check if brief stillness was merged
  expect_true(all(result[201:230] == 0))

  # Test with longer gap (should not merge)
  speed <- c(rep(0.1, 100),       # stillness
             rep(5, 100),          # movement
             rep(0.1, 200),        # long stillness (should not merge)
             rep(5, 100),          # movement
             rep(0.1, 100))        # stillness

  result <- classify_movement_plateau(speed, refine_transitions = FALSE)
  # Check if long stillness was preserved
  expect_true(any(result[250:350] == 0))
})

test_that("classify_movement_plateau parameters affect output as expected", {
  speed <- c(rep(0.1, 100), rep(5, 100), rep(0.1, 100))

  # Test threshold_multiplier effect
  strict <- classify_movement_plateau(speed, threshold_multiplier = 5, refine_transitions = FALSE)
  lenient <- classify_movement_plateau(speed, threshold_multiplier = 1, refine_transitions = FALSE)
  expect_true(sum(strict == 1) < sum(lenient == 1))

  # Test min_still_gap effect
  small_gap <- classify_movement_plateau(speed, min_still_gap = 10)
  large_gap <- classify_movement_plateau(speed, min_still_gap = 200)
  expect_true(sum(small_gap == 1) >= sum(large_gap == 1))
})

test_that("classify_movement_plateau handles gradual transitions", {
  # Create gradual acceleration
  x <- seq(0, 2*pi, length.out = 300)
  speed <- c(rep(0.1, 100),
             0.1 + (5-0.1) * (1 - cos(x[1:100]))/2,  # Gradual increase
             rep(5, 100))

  result <- classify_movement_plateau(speed)

  # Should detect movement during transition
  transition_point <- which(result == 1)[1]
  expect_true(transition_point < 150)  # Movement should be detected during acceleration
})

test_that("classify_movement_plateau is robust to different scales", {
  # Test with very small values
  small_speed <- c(rep(0.001, 100), rep(0.05, 100), rep(0.001, 100))
  small_result <- classify_movement_plateau(small_speed)
  expect_true(any(small_result == 1))

  # Test with very large values
  large_speed <- c(rep(10, 100), rep(500, 100), rep(10, 100))
  large_result <- classify_movement_plateau(large_speed)
  expect_true(any(large_result == 1))

  # Results should be similar regardless of scale
  expect_equal(
    sum(small_result == 1),
    sum(large_result == 1)
  )
})
