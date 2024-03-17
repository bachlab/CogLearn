#' Determine CS+ and CS- for each subject
#'
#' Uses trial-by-trial CS and US information from a trial results data frame
#' to determine CS+ and CS-, ignoring trials in which no CS was presented. This
#' function assumes that there is just one CS+ and one CS-
#'
#' @param CS A string vector indicates stimulus name for each trial
#' @param US An integer vector indicating the US presence for each trial
#' @return A list of CS- and CS+
#' @export
#'
#' @examples
#' CS <- c('blue_cube', 'yellow_cube', 'blue_cube', 'yellow_cube')
#' US <- c(1,0,1,0)
#' find_CS(CS, US)
find_CS <- function(CS, US) {
  df <- tibble::tibble(CS, US)

  CSp <-
    df %>%
    dplyr::filter(US == 1) %>%
    dplyr::pull(CS) %>%
    unique(.)
  CSm <-
    df %>%
    dplyr::filter(!(CS %in% c(CSp, "nothing"))) %>%
    dplyr::pull(CS) %>%
    unique(.)
  list(c(CSm, CSp))
}


#' Separate experiment phases
#'
#' Phases are defined by the presence of breaks before a trial. This functions
#' finds these breaks
#'
#' @param break_before A logical vector (TRUE/FALSE) with trial-by-trial information
#' @returns a vector of phase number for each trial
#'
#' @examples
#' break_before <- c(TRUE,TRUE,FALSE,FALSE,FALSE)
#' parse_phases(break_before)
#' @export
#' @export
parse_phases <- function(break_before) {
  break_before %>%
    ifelse(1, 0) %>%
    cumsum()
}


#' Re-number trials across separate VR sessions
#'
#' If an experiment spans several VR sessions or ppids (e.g. days) then trial numbers start
#' from 1 for each session. This functions re-numbers to successive trial numbers
#'
#' @param session A trial-by-trial integer vector with session numbers
#' @param trial_num A trial-by-trial integer vector with trial numbers
#'
#' @returns A trial_by_trial vector of successive trial number
#'
#'
#' @examples
#' d <- c(1:7)
#' t <- c(1:7)
#' renumber_trials(d, t)
#' @export
renumber_trials <- function(session, trial_num) {

  trials <-  tibble::tibble(session = session, trial_num = trial_num)

  sum_session <- trials %>%
    dplyr::mutate(session = session + 1) %>%
    dplyr::group_by(session) %>%
    dplyr::summarise(prev_trials = max(trial_num)) %>%
    dplyr::add_row(session = 1, prev_trials = 0) %>%
    dplyr::arrange(session) %>%
    dplyr::mutate(prev_trials = cumsum(prev_trials))

  trials %>%
    dplyr::left_join(sum_session) %>%
    dplyr::mutate(all_trials = trial_num + prev_trials) %>%
    dplyr::pull(all_trials)

}


#' Update session number based on experimental days per participant
#'
#' For studies that span multiple days, the ppid might include an indication of
#' experimental day. This function parses the ppid and extracts the day as
#' numerical variable
#'
#' @param ppid A string vector of participant ids that include the day
#'
#' @examples
#' exp <- c('coglearn_exp1_s3_day1', 'coglearn_exp1_s3_day2', 'coglearn_exp1_s3_day3')
#' update_session_number(exp)
#' @export
update_session_number <- function(exp) {
  ifelse(stringr::str_detect(exp, "day"),
         as.numeric(
           stringr::str_extract(
             stringr::str_extract(exp, "day[0-9]"),
             "[0-9]")), 1)

}

#' Normalise data to mean of CS- trials for each participant
#'
#' @param data A numerical vector
#' @param condition  A factor with several levels of which one must be CSm,
#' indicating the CS condition
#' @returns A normalised data vector of the same length as data
#'
#' @examples
#' scr <- c(3, 9, 10, 2)
#' cs <- c('CSm', 'CSp', 'CSp', 'CSm')
#' normalise_data(scr, cs)
#' @export
normalise_data <- function(data, condition) {
  df <- tibble::tibble(data, condition)

  divisor <-
    df %>%
    dplyr::filter(condition == "CSm") %>%
    dplyr::pull(data) %>%
    mean(na.rm = T)

  data / divisor

}

#' Clean up and check tracker data
#'
#' This function removes duplicate time stamps from tracker data frames, and
#' returns number of duplicates, within tracker data frames. It also checks the
#' validity of head tracker starting position and momentary speed using the
#' vrthreat package function check_valid_movement
#'
#' @param df A trial results tibble. Must contain the columns trial_type,
#' walk_pos_x, walk_pos_z, head_movement_data_0
#'
#' @returns A tibble with head tracker data frames cleaned up and added column
#' valid_head_movement_data_0
#' @export
check_tracker_data <- function(df) {
  df %>%
    dplyr::rowwise() %>%
    vrthreat::summarise_movement_cols() %>%
    dplyr::mutate(start_pos =
                    dplyr::if_else(trial_type == "static",
                                   list(list(
                                     pos_x = NA, pos_z = NA
                                   )),
                                   list(
                                     list(pos_x = walk_pos_x, pos_z = walk_pos_z)
                                   ))) %>%
    vrthreat::check_tracker_cols(start_pos = "start_pos",
                                 ref_tracker = "head_movement_data_0") %>%
    dplyr::select(!start_pos)

}


#' Generate behavioral outcome variables
#'
#' This function generates some commonly used outcome variables:
#'- tracker validity and max number of tracker timestamp duplicates
#'- CS type (CSm, CSp)
#'- timing information (CS onset, US onset, US offset)
#'- mean/min/max distance from US during CS presentation
#'- distance travelled during CS presentation
#'- maximum movement speed during CS presentation
#'
#' @param df A trial results tibble
#'
#' @return A trial results tibble with variables added
#' @export
#'
generate_outcome_variables <- function(df) {
  CS_pos <- list(pos_x = 0,
                 pos_y = 0,
                 pos_z = 2.5)

  df <-
    df %>%
    dplyr::mutate(
      valid_head_tracker = valid_head_movement_data_0$valid,
      max_timestamp_duplicates = vrthreat::extract_timestamp_duplicates(head_movement_data_0),
      CS = dplyr::recode(
        stimulus_name,
        "{CS_type[[1]]}" := "CSm",
        "{CS_type[[2]]}" := "CSp"
      ),
      CS_onset = start_time + pre_stimulus_duration,
      US_onset = start_time + pre_stimulus_duration + pre_punishment_duration,
      US_offset = start_time + pre_stimulus_duration + pre_punishment_duration + punishment_duration,
      min_dist_CS = vrthreat::extract_movement_dist(
        head_movement_data_0,
        CS_pos,
        min = CS_onset,
        max = US_onset,
        method = "min"
      ),
      mean_dist_CS = vrthreat::extract_movement_dist(
        head_movement_data_0,
        CS_pos,
        min = CS_onset,
        max = US_onset,
        method = "mean"
      ),
      max_dist_CS = vrthreat::extract_movement_dist(
        head_movement_data_0,
        CS_pos,
        min = CS_onset,
        max = US_onset,
        method = "max"
      ),
      distance_CS = pre_punishment_duration *
        vrthreat::extract_speed(
          head_movement_data_0,
          min_time = CS_onset,
          max_time = US_onset
        ),
      max_speed_CS = vrthreat::extract_speed(
        head_movement_data_0,
        min_time = CS_onset,
        max_time = US_onset,
        method = "max"
      )
    )

  return(df)

}


#' Verify US rate for each CS
#'
#' Extracts the US rate for each participant, phase, and CS, and prints them
#' on the screen. This provides a basic manipulation check.
#'
#' @param df A trial results tibble
#' @importFrom dplyr %>%
#'
#' @export
#'
#' @examples
#' d <- tibble::tibble(ppid = rep("pp.1", times = 8),
#'             phase = rep(2, times = 8),
#'             CS = rep("CSp", times = 8),
#'             punishment_provided = c(1,0,1,1,1,0,1,1))
#'
#' print_US_rate(d)
print_US_rate <- function(df) {
  df  %>%
    dplyr::group_by(ppid, phase, CS) %>%
    dplyr::summarise(reinforcement = mean(punishment_provided)) %>%
    tidyr::pivot_wider(values_from = reinforcement, names_from = c(phase, CS)) %>%
    print(n = 100)

}

