select_roi <- function(path,
                       frame_number = 1,
                       timestamp = NULL,
                       close_polygon = TRUE) {

  # Check for required packages
  required_pkgs <- c("shiny", "magick")
  missing_pkgs <- required_pkgs[!sapply(required_pkgs, rlang::is_installed)]

  if (length(missing_pkgs) > 0) {
    cli::cli_alert_warning("This function requires the following packages: {.pkg {missing_pkgs}}")
    cli::cli_alert_info("Would you like to install them now?")
    if (utils::menu(c("Yes", "No")) == 1) {
      if (rlang::is_installed("renv") && renv::activated()) {
        cli::cli_alert_info("Installing with renv...")
        renv::install(missing_pkgs)
      } else {
        cli::cli_alert_info("Installing with install.packages()...")
        utils::install.packages(missing_pkgs)
      }
    } else {
      cli::cli_abort("Cannot proceed without required packages")
    }
  }

  requireNamespace("shiny", quietly = TRUE)
  requireNamespace("magick", quietly = TRUE)

  # Load image based on type
  cli::cli_alert_info("Loading image...")

  if (tolower(tools::file_ext(path)) %in% c("mp4", "avi", "mov")) {
    if (!rlang::is_installed("av")) {
      cli::cli_alert_warning("The {.pkg av} package is required for video files")
      cli::cli_alert_info("Would you like to install it now?")
      if (utils::menu(c("Yes", "No")) == 1) {
        if (rlang::is_installed("renv") && renv::activated()) {
          cli::cli_alert_info("Installing with renv...")
          renv::install("av")
        } else {
          cli::cli_alert_info("Installing with install.packages()...")
          utils::install.packages("av")
        }
      } else {
        cli::cli_abort("Cannot proceed without {.pkg av} package")
      }
    }
    requireNamespace("av", quietly = TRUE)

    # Modified video loading
    cli::cli_alert_info("Reading video frame {frame_number}...")

    # Get all frames and select the one we want
    video_frames <- av::av_video_images(path)
    if (frame_number > length(video_frames)) {
      cli::cli_abort("Requested frame {frame_number} exceeds video length ({length(video_frames)} frames)")
    }

    # Convert to raster
    img <- as.raster(video_frames[[frame_number]])
  } else {
    img_magick <- magick::image_read(path)
    img <- as.raster(img_magick)
  }

  img_width <- ncol(img)
  img_height <- nrow(img)

  # Create a Shiny app for ROI selection
  ui <- shiny::fluidPage(
    shiny::tags$head(
      shiny::tags$style(shiny::HTML("
        #plot-container { position: relative; }
        #roi-plot { cursor: crosshair; }
      "))
    ),
    shiny::titlePanel("Select ROI Points"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::actionButton("reset", "Reset Points"),
        shiny::actionButton("undo", "Undo Last Point"),
        shiny::actionButton("done", "Done"),
        shiny::checkboxInput("close_polygon", "Close Polygon", value = close_polygon),
        shiny::hr(),
        shiny::h4("Selected Points:"),
        shiny::tableOutput("points_table"),
        shiny::hr(),
        shiny::h4("Instructions:"),
        shiny::tags$ul(
          shiny::tags$li("Click to add points"),
          shiny::tags$li("Double-click near a point to select it"),
          shiny::tags$li("Move mouse to drag selected point"),
          shiny::tags$li("Click to place dragged point")
        ),
        width = 3
      ),
      shiny::mainPanel(
        shiny::div(
          id = "plot-container",
          shiny::plotOutput("roi_plot",
                            click = "plot_click",
                            hover = "plot_hover",
                            dblclick = "plot_dblclick",
                            width = "100%",
                            height = "600px")
        )
      )
    )
  )

  server <- function(input, output, session) {
    # Store points and dragging state
    points <- shiny::reactiveVal(tibble::tibble(
      point_id = integer(),
      x = numeric(),
      y = numeric()
    ))
    is_dragging <- shiny::reactiveVal(FALSE)
    selected_point <- shiny::reactiveVal(NULL)
    hover_distance_threshold <- 20  # pixels

    # Function to find nearest point
    find_nearest_point <- function(x, y, current_points, threshold) {
      if (nrow(current_points) == 0) return(NULL)

      distances <- sqrt((current_points$x - x)^2 + (current_points$y - y)^2)
      nearest_idx <- which.min(distances)

      if (distances[nearest_idx] < threshold) {
        return(nearest_idx)
      } else {
        return(NULL)
      }
    }

    # Plot the image and points
    output$roi_plot <- shiny::renderPlot({
      # Set up the plot area
      par(mar = c(0,0,0,0))
      plot(c(0, img_width), c(0, img_height),
           type = "n", asp = 1, xlab = "", ylab = "",
           xaxs = "i", yaxs = "i")

      # Draw the image
      rasterImage(img, 0, 0, img_width, img_height)

      # Get current points
      current_points <- points()

      if (nrow(current_points) > 0) {
        # Plot points with different color for selected point
        for (i in 1:nrow(current_points)) {
          point_color <- if (is_dragging() && i == selected_point()) "blue" else "red"
          point_size <- if (is_dragging() && i == selected_point()) 2 else 1.5

          graphics::points(x = current_points$x[i],
                           y = current_points$y[i],
                           pch = 16,
                           col = point_color,
                           cex = point_size)

          # Add point numbers
          graphics::text(x = current_points$x[i],
                         y = current_points$y[i],
                         labels = current_points$point_id,
                         pos = 3,
                         col = "yellow",
                         font = 2)
        }

        # Draw lines between points
        if (nrow(current_points) > 1) {
          for (i in 1:(nrow(current_points) - 1)) {
            graphics::lines(x = c(current_points$x[i], current_points$x[i+1]),
                            y = c(current_points$y[i], current_points$y[i+1]),
                            col = "red", lwd = 2)
          }

          # Close polygon if requested and enough points
          if (input$close_polygon && nrow(current_points) > 2) {
            graphics::lines(x = c(current_points$x[nrow(current_points)], current_points$x[1]),
                            y = c(current_points$y[nrow(current_points)], current_points$y[1]),
                            col = "red", lwd = 2, lty = 2)
          }
        }
      }
    })

    # Display points table
    output$points_table <- shiny::renderTable({
      points() |>
        dplyr::mutate(
          x = round(x, 1),
          y = round(y, 1)
        )
    })

    # Handle double-click to start dragging
    shiny::observeEvent(input$plot_dblclick, {
      current_points <- points()
      nearest_idx <- find_nearest_point(
        input$plot_dblclick$x,
        input$plot_dblclick$y,
        current_points,
        hover_distance_threshold
      )

      if (!is.null(nearest_idx)) {
        selected_point(nearest_idx)
        is_dragging(TRUE)
      }
    })

    # Handle hover for dragging
    shiny::observeEvent(input$plot_hover, {
      if (is_dragging()) {
        current_points <- points()
        idx <- selected_point()

        if (!is.null(idx)) {
          current_points$x[idx] <- input$plot_hover$x
          current_points$y[idx] <- input$plot_hover$y
          points(current_points)
        }
      }
    })

    # Handle click to either add point or stop dragging
    shiny::observeEvent(input$plot_click, {
      if (is_dragging()) {
        is_dragging(FALSE)
        selected_point(NULL)
      } else {
        new_point <- tibble::tibble(
          point_id = nrow(points()) + 1,
          x = input$plot_click$x,
          y = input$plot_click$y
        )
        points(dplyr::bind_rows(points(), new_point))
      }
    })

    # Reset points
    shiny::observeEvent(input$reset, {
      points(tibble::tibble(
        point_id = integer(),
        x = numeric(),
        y = numeric()
      ))
      is_dragging(FALSE)
      selected_point(NULL)
    })

    # Undo last point
    shiny::observeEvent(input$undo, {
      current_points <- points()
      if (nrow(current_points) > 0) {
        points(current_points |> dplyr::slice(-dplyr::n()))
      }
      is_dragging(FALSE)
      selected_point(NULL)
    })

    # Return value when done
    return_points <- shiny::reactive({
      if (input$done > 0) {
        return(points())
      }
    })

    shiny::observeEvent(input$done, {
      shiny::stopApp(return_points())
    })
  }

  # Run the app and capture the results
  cli::cli_alert_info("Opening ROI selection window...")
  roi_points <- shiny::runApp(shiny::shinyApp(ui, server))

  if (!is.null(roi_points) && nrow(roi_points) > 0) {
    cli::cli_alert_success("ROI selection complete!")
    cli::cli_alert_info("Number of points selected: {.val {nrow(roi_points)}}")
    return(roi_points)
  } else {
    cli::cli_alert_warning("No points were selected")
    return(NULL)
  }
}
