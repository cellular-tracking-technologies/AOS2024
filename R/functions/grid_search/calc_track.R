library(mapview)
source("R/functions/utils/rssi_v_dist.R")
source("R/functions/utils/get_time_value.R")
source("R/functions/grid_search/calc_receiver_values.R")
source("R/functions/grid_search/calc_grid_values.R")
source("R/functions/grid_search/map_grid_solution.R")

calculate_track <- function(
    start_time,
    length_seconds,
    step_size_seconds,
    det_time_window,
    filter_alpha,
    filter_time_range,
    grid_df,
    detection_df,
    node_locs,
    node_t_offset = NULL,
    rssi_coefs,
    track_frame_output_path = NULL,
    tile_url = "https://tile.openstreetmap.org/{z}/{x}/{y}.png") {
    #################################################################
    num_steps <- length_seconds / step_size_seconds
    start_time_value <- get_time_value(start_time)

    track_df <- data.frame(
        i = integer(),
        time = integer(),
        lat = double(),
        lon = double(),
        max_rssi = double(),
        avg_rssi = double(),
        error = double()
    )

    for (i in 1:num_steps) {
        print(paste("Starting: ", i, "/", num_steps))
        # Get the time range for this bin
        #  -Stop at the current bin time
        #  -Take all detections within one time_window prior to current bin time
        bin_stop_value <- start_time_value + (i - 1) * step_size_seconds
        # bin_start_value <- bin_stop_value - det_time_window

        # For all detections in this bin calculate the avg rssi in each receiver
        print("Calculating receiver values...")
        rec_df <- calc_receiver_values(
            current_time = bin_stop_value,
            det_window = det_time_window,
            station_tag_df = detection_df,
            node_locs = node_locs,
            node_t_offset,
            rssi_coefs = rssi_coefs,
            filter_alpha = filter_alpha,
            filter_time_range = filter_time_range
        )
        print(rec_df)

        # Check that at least three nodes have detections in time window
        nodes_with_dets <- sum(rec_df$n > 0)
        if (nodes_with_dets >= 3) {
            # Calculate the "grid value" for each spatial bin in the grid
            print("Calculating grid values...")
            grid_values <- calc_grid_values(grid_df, rec_df, rssi_coefs)

            # Find the bin with best "grid value"
            solution <- subset(grid_values, grid_values$value == max(grid_values$value))

            # Add solution to the track
            track_point <- data.frame(
                i = i,
                time = bin_stop_value,
                lat = solution$center_lat,
                lon = solution$center_lon,
                max_rssi = max(rec_df$avg_rssi),
                avg_rssi = mean(rec_df$avg_rssi),
                error = NaN
            )
            track_df <- rbind(track_df, track_point)

            # Only draw and export the individual frame if a output path was specified
            if (!is.null(track_frame_output_path)) {
                print("Drawing map...")
                map <- map_latest_solution(node_locs, rec_df, grid_values, solution, track_df, tile_url)
                # Save the map to a png
                print("Exporting map...")
                map_file_path <- paste(track_frame_output_path, "t", i, ".png", sep = "")
                mapshot(map, file = map_file_path)
            }
        } else {
            print("Skipping time bin due to not enough detections")
        }
    }

    print("Track calulcation complete!!!")
    return(track_df)
}
