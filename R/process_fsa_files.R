#' Process .fsa Files and Generate Plot
#'
#' This function processes all .fsa files in a given folder, applies the ladder and 
#' generates an interactive plot of the data.
#'
#' @param folder_path A character string specifying the path to the folder containing .fsa files.
#' @param ladder A numeric vector specifying the ladder values used in the analysis.
#' @param init_thresh A numeric value specifying the initial threshold for plot generation (default is 7000).
#' @return A plotly object containing the plot of all processed .fsa files.
#' @export
#'
#' @examples
#' folder_path <- "/home/sliu/ShaneLiu/Genotype/fsa1"
#' ladder <- c(35, 50, 75, 100, 139, 150, 160, 200, 250, 300, 340, 350, 400, 450, 490, 500)
#' plot <- process_fsa_files(folder_path, ladder)
#' plot
process_fsa_files <- function(folder_path, ladder, init_thresh = 7000) {
    # Get a list of all .fsa files in the folder
    files <- list.files(path = folder_path, pattern = "\\.fsa$", full.names = TRUE)
    
    # Initialize variables to track global y-axis limits
    global_y_min <- Inf
    global_y_max <- -Inf
    
    # Store the traces for adding later
    trace_list <- list()
    
    # Loop through each file and process it
    for (file in files) {
        print(paste("Processing file:", basename(file)))
        
        # Process the file using your modified function
        my.plants <- storing.inds1(folder = folder_path, file = basename(file), channels = 4, rawPlot = FALSE, fourier = TRUE)
        ladder.info.attach(stored = my.plants, ladder = ladder)
        
        # Generate the plot data using overview_interactive1
        plot_data <- overview_interactive1(my.inds = my.plants, channel = 1, ladder = ladder, init.thresh = init_thresh)
        
        # Check if x and y are available in plot_data
        if ("x" %in% names(plot_data) && "y" %in% names(plot_data)) {
            # Track the global y-axis limits
            global_y_min <- min(global_y_min, min(plot_data$y, na.rm = TRUE))
            global_y_max <- max(global_y_max, max(plot_data$y, na.rm = TRUE))
            
            # Store the trace for later plotting
            trace_list[[basename(file)]] <- list(
                x = plot_data$x,
                y = plot_data$y,
                name = basename(file)
            )
        } else {
            print(paste("Skipping file due to missing x or y columns:", basename(file)))
        }
    }
    
    # Initialize an empty plotly object
    p <- plot_ly()
    
    # Add each trace to the plotly object
    for (trace in trace_list) {
        p <- add_trace(p, x = trace$x, y = trace$y, type = 'scatter', mode = 'lines', name = trace$name)
    }
    
    # Adjust the y-axis limits based on the global min and max
    p <- layout(p, yaxis = list(range = c(global_y_min, global_y_max)))
    
    # Render the final plot
    return(p)
}
