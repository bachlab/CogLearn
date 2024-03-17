#' Read and tidy all Unity output csv files from a single study
#'
#' Reads the "trial_results.csv" output files from all participants in a parent
#' folder, updates file pointers for the referenced files, and reads all
#' head and hand tracker information
#'
#' @param user_dir Parent directory where data are stored
#' @return A trial results tibble
#'
#' @export
read_raw_data <- function(user_dir) {
  column_spec <- readr::cols(ppid = readr::col_character())

  raw_data <-
    vrthreat::read_trial_results(user_dir,
                                 col_types = column_spec) %>%
    vrthreat::update_file_pointers() %>%
    # read nested CSV files (movement files)
    vrthreat::read_csv_files(
      c(
        head_movement_location_0,
        lefthand_movement_location_0,
        righthand_movement_location_0
      ),
      col_types = readr::cols(
        time = readr::col_double(),
        pos_x = readr::col_double(),
        pos_y = readr::col_double(),
        pos_z = readr::col_double(),
        rot_x = readr::col_double(),
        rot_y = readr::col_double(),
        rot_z = readr::col_double(),
      )
    )

  return(raw_data)

}
