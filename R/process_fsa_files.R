#' Process FSA Files
#'
#' This function processes .fsa files from a specified folder, applies necessary transformations,
#' and generates an interactive plot for visualization.
#'
#' @param folder_path A character string specifying the folder containing .fsa files.
#' @param ladder A vector or object representing the ladder information for processing.
#' @return A Plotly object containing the interactive plot.
#' @import plotly
#' @export
#Alternative process_fsa_files function with interactive toggle buttons, so not all the plots showing up at once
process_fsa_files <- function(folder_path, ladder) {
    # Get a list of all .fsa files in the folder
    files <- list.files(path = folder_path, pattern = "\\.fsa$", full.names = TRUE)
    file_names <- basename(files)
    
    # Initialize variables to track global y-axis limits
    global_y_min <- Inf
    global_y_max <- -Inf

    # Store the traces for adding later
    trace_list <- list()

    # Loop through each file and process it
    for (i in seq_along(files)) {
        file <- files[i]
        file_name <- file_names[i]
        print(paste("Processing file:", file_name))
        
        # Process the file using your modified function
        my.plants <- storing.inds1(folder = folder_path, file = file_name, channels = 4, rawPlot = FALSE, fourier = TRUE)
        ladder.info.attach(stored = my.plants, ladder = ladder)
        
        # Generate the plot data using overview_interactive1
        plot_data <- overview_interactive1(my.inds = my.plants, channel = 1, ladder = ladder, init.thresh = 7000)
        
        # Check if x and y are available in plot_data
        if ("x" %in% names(plot_data) && "y" %in% names(plot_data)) {
            # Track the global y-axis limits
            global_y_min <- min(global_y_min, min(plot_data$y, na.rm = TRUE))
            global_y_max <- max(global_y_max, max(plot_data$y, na.rm = TRUE))
            
            # Store the trace for later plotting
            trace_list[[file_name]] <- list(
                x = plot_data$x,
                y = plot_data$y,
                name = file_name
            )
        } else {
            print(paste("Skipping file due to missing x or y columns:", file_name))
        }
    }

    # Initialize an empty plotly object
    p <- plot_ly()

    # Add each trace to the plotly object
    for (trace in trace_list) {
        p <- add_trace(p, x = trace$x, y = trace$y, type = 'scatter', mode = 'lines', name = trace$name, visible = FALSE)
    }

    # Add toggle buttons to control trace visibility
    p <- layout(p,
                updatemenus = list(
                    list(
                        buttons = lapply(names(trace_list), function(fn) {
                            list(
                                method = 'update',
                                args = list(
                                    list(visible = sapply(names(trace_list), function(name) name == fn)),
                                    list(title = paste("Showing:", fn))
                                ),
                                label = fn
                            )
                        }),
                        direction = 'down',
                        showactive = TRUE
                    )
                ),
                yaxis = list(title = "Y-axis", range = c(global_y_min, global_y_max))
    )

    # Render the final plot
    p
}

