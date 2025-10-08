#' Find contour points in a binary image
#'
#' @param binary_matrix A logical matrix where TRUE represents foreground pixels
#' @param mode Character string specifying contour retrieval mode:
#'             "external" (only outer contours) or "all" (all contours)
#' @return List of contours, where each contour is a matrix of boundary x,y coordinates
find_contours <- function(binary_matrix, mode = "external") {
  # Convert logical matrix to numeric (0s and 1s)
  if (is.logical(binary_matrix)) {
    img <- matrix(as.numeric(binary_matrix), nrow = nrow(binary_matrix))
  } else {
    img <- binary_matrix
  }

  # Pad the image with zeros to handle border cases
  padded_img <- cbind(0, rbind(0, img, 0), 0)

  # Initialize variables
  contours <- list()
  visited <- matrix(FALSE, nrow = nrow(padded_img), ncol = ncol(padded_img))
  directions <- list(
    c(0, 1),   # right
    c(1, 1),   # down-right
    c(1, 0),   # down
    c(1, -1),  # down-left
    c(0, -1),  # left
    c(-1, -1), # up-left
    c(-1, 0),  # up
    c(-1, 1)   # up-right
  )

  #' Helper function to trace a contour starting from a point
  #' @param start_row Starting row coordinate
  #' @param start_col Starting column coordinate
  trace_contour <- function(start_row, start_col) {
    contour <- matrix(ncol = 2, nrow = 0)
    current_row <- start_row
    current_col <- start_col
    start_dir <- 0  # Start searching right

    repeat {
      # Add current point to contour
      contour <- rbind(contour, data.frame(x = current_row - 1, y = current_col - 1))  # Adjust for padding
      visited[current_row, current_col] <<- TRUE

      # Search in all directions starting from the current direction
      found_next <- FALSE
      for (i in 0:7) {
        search_dir <- (start_dir + i) %% 8
        next_row <- current_row + directions[[search_dir + 1]][1]
        next_col <- current_col + directions[[search_dir + 1]][2]

        if (padded_img[next_row, next_col] == 1 &&
            (!visited[next_row, next_col] ||
             (next_row == start_row && next_col == start_col))) {
          current_row <- next_row
          current_col <- next_col
          start_dir <- (search_dir + 5) %% 8  # Start searching from 3 positions back
          found_next <- TRUE
          break
        }
      }

      # If we're back at the start or can't find next point, stop
      if (!found_next || (length(contour) > 2 &&
                          all(contour[1,] == c(current_row - 1, current_col - 1)))) {
        break
      }
    }

    # Remove the duplicate last point if it exists
    if (nrow(contour) > 1 &&
        all(contour[1,] == contour[nrow(contour),])) {
      contour <- contour[-nrow(contour),]
    }

    return(contour)
  }

  # Main loop to find contours
  for (row in 1:nrow(padded_img)) {
    for (col in 1:ncol(padded_img)) {
      if (padded_img[row, col] == 1 && !visited[row, col]) {
        # Check if this is an external contour
        is_external <- any(padded_img[row + c(-1,0,1),
                                      col + c(-1,0,1)] == 0)

        if (mode == "all" || (mode == "external" && is_external)) {
          contour <- trace_contour(row, col)
          if (nrow(contour) > 2) {  # Only add contours with at least 3 points
            contours[[length(contours) + 1]] <- contour
          }
        }
      }
    }
  }

  return(contours)
}

#' Compress contour points to minimum necessary corners
#'
#' @param contour Matrix of x,y coordinates from find_contours()
#' @param tolerance Numeric value for detecting collinear points (default: 1e-10)
#' @return Matrix of corner x,y coordinates
compress_contour <- function(contour, tolerance = 1e-10) {
  if (nrow(contour) <= 2) return(contour)

  # Helper function to check if three points are collinear
  are_collinear <- function(p1, p2, p3) {
    # Calculate the area of the triangle formed by the three points
    # If area is close to 0, points are collinear
    area <- abs(p1[1] * (p2[2] - p3[2]) +
                  p2[1] * (p3[2] - p1[2]) +
                  p3[1] * (p1[2] - p2[2])) / 2
    return(area < tolerance)
  }

  # Initialize with first point
  corners <- data.frame(x = contour$x[1], y = contour$y[1])
  prev_point <- contour[1,]

  # Add points that create non-collinear segments
  for (i in 2:(nrow(contour) - 1)) {
    current_point <- contour[i,]
    next_point <- contour[i + 1,]

    if (!are_collinear(prev_point, current_point, next_point)) {
      corners <- rbind(corners, data.frame(x = current_point[1], y = current_point[2]))
      prev_point <- current_point
    }
  }

  # Always add the last point if it's not already included
  if (!all(corners[nrow(corners),] == contour[nrow(contour),])) {
    corners <- rbind(corners, data.frame(x = contour$x[nrow(contour)], y = contour$y[nrow(contour)]))
  }

  return(corners)
}

#' Plot contours for visualization
#'
#' @param contours List of contours
#' @param width Original image width
#' @param height Original image height
#' @param compressed Logical indicating whether to plot compressed corners only
plot_contours <- function(contours, width, height, compressed = FALSE) {
  # Create empty plot
  plot(0, 0, type = "n", xlim = c(0, width), ylim = c(0, height),
       xlab = "X", ylab = "Y", asp = 1)

  # Plot each contour
  for (i in seq_along(contours)) {
    points <- if (compressed) {
      compress_contour(contours[[i]])
    } else {
      contours[[i]]
    }

    # Draw lines connecting all points
    points <- rbind(points, points[1,])  # Close the contour
    lines(points$y, points$x, col = i)

    # Add points to show vertices
    points(points$y[-nrow(points)], points$x[-nrow(points)],
           col = i, pch = 16)
  }
}
