#' Test the number of unique CS per trial for a specific phase
#'
#' @param df A tibble that include all individuals' trial structures
#' @param i_phase An integer (e.g., 1) or a string (e.g., 'first_avoid_conditioning')
#' @param trial The column name in the df tibble that contain trial number information
#' @param CS The column name in the df tibble that contain CS information
#'
#' @return A boolean value, either TRUE or FALSE
#' @export
#'
test_trial_CS_num <- function(df, i_phase, trial, CS) {
  # compute how many unique CS per trial
  CS_num_trial <-
    df %>%
    dplyr::filter(phase_name == i_phase) %>%
    dplyr::group_by({{trial}}, {{CS}}) %>%
    dplyr::summarise(n = n_distinct({{CS}})) %>%
    dplyr::ungroup() %>%
    dplyr::group_by({{trial}}) %>%
    dplyr::summarise(f = sum(n)) %>%
    dplyr::pull(f) %>%
    unique()

  # compute how many unique CS in the current phase
  CS_num_phase <-
    df %>%
    dplyr::filter(phase_name == i_phase) %>%
    dplyr::pull({{CS}}) %>%
    unique() %>%
    length()

  # unit test: CS_num_trial should equal to CS_num_phase
  if (CS_num_trial == CS_num_phase) {
    return(TRUE)
  } else {
    return(FALSE)
  }

}


#' Test how many trials per participant have
#'
#' @param df A tibble that include all individuals' trial structures
#' @param ppid The name of the column that contains participant ID
#' @param trial The name of the column that contains trial information
#'
#' @return A boolean value (TRUE/FALSE)
#' @export
#'
test_trial_pp <- function(df, ppid, trial) {

  trial_pp <-
    df %>%
    dplyr::group_by({{ppid}}) %>%
    dplyr::summarise(n = length(punishment_probability)) %>%
    dplyr::pull(n) %>%
    unique()

  trial_total <-
    df %>%
    dplyr::pull({{trial}}) %>%
    unique() %>%
    length()

  if (trial_pp == trial_total) {
    return(TRUE)
  } else {
    return(FALSE)
  }

}


#' Test the punishment probability per participant
#'
#' @param df A tibble that include all individuals' trial structures
#' @param i_phase A string (e.g., 'first_avoid_conditioning') or An integer (e.g., 1)
#' @param ppid Name of column for participant ID
#' @param punish_prob Name of the column for punishment probability (binary: 0/1)
#' @param ... Punishment probability given by researcher (e.g., 0.375)
#'
#' @return A boolean value (TRUE/FALSE)
#' @export
#'
test_punish_pp <- function(df, i_phase, ppid, punish_prob, ...) {
  punish_pp <-
    df %>%
    dplyr::filter(phase_name == i_phase) %>%
    dplyr::group_by({{ppid}}) %>%
    dplyr::summarise(p = mean({{punish_prob}})) %>%
    dplyr::pull(p) %>%
    unique()

  if (punish_pp == ...) {
    return(TRUE)
  } else {
    return(FALSE)
  }

}


#' Test number of stimuli per condition per phase per participant
#'
#' @param df A tibble that include all individuals' trial structures
#' @param i_phase A string (e.g., 'first_avoid_conditioning') or An integer (e.g., 1)
#' @param ppid Name of column for participant ID
#' @param stimulus Name of column for stimulus names
#' @param ... Number of stimuli per condition that is defined by the researcher
#'            Should be an integer (e.g., 32)
#'
#' @return A boolean value (TRUE/FALSE)
#' @export
#'
test_stimuli_phase_pp <- function(df, i_phase, ppid, stimulus, ...) {

  stimuli_phase_pp <-
    df %>%
    dplyr::filter(phase_name == i_phase) %>%
    dplyr::group_by({{ppid}}, {{stimulus}}) %>%
    dplyr::summarise(n = length({{stimulus}})) %>%
    dplyr::pull(n) %>%
    unique()

  if (stimuli_phase_pp == ...) {
    return(TRUE)
  } else {
    return(FALSE)
  }


}


#' Test for counterbalance
#' @description This function will test for a specific phase, whether a specific
#' CS (e.g., CS1+/CS1-) is counterbalanced across participants
#'
#' @param df A tibble that include all individuals' trial structures
#' @param i_phase A string (e.g., 'first_avoid_conditioning') or An integer (e.g., 1)
#' @param cs_column Name of the column that refers to CS information
#' @param cs_type A string that indicates a specific CS type (e.g., 'CS1p_USm')
#' @param stimulus Name of the column that refers to specific stimuli
#' @param ppid Name of the column that refer to participants' ID
#' @param sub_num An integer that indicates number of participants. The default value is 0
#'
#' @return A boolean value (TRUE/FALSE)
#' @export
#'
test_counterbalance <- function(df, i_phase, cs_column, cs_type,
                                stimulus, ppid, sub_num = 0) {

  # within each phase, extract the unique stimuli
  all_stimuli <-
    df %>%
    dplyr::filter(phase_name == i_phase) %>%
    dplyr::pull({{stimulus}}) %>%
    unique()

  # for a given phase, extract the stimuli for a specific CS type across participants
  cs_pp_phase <-
    df %>%
    dplyr::filter(phase_name == i_phase,
           {{cs_column}} == cs_type) %>%
    dplyr::group_by({{ppid}}, {{cs_column}}) %>%
    dplyr::summarise(which_CS = unique({{stimulus}})) %>%
    dplyr::mutate(counterbalance_check = dplyr::case_when(
      which_CS == all_stimuli[1] ~ 1,
      which_CS == all_stimuli[2] ~ -1
    ))

  # if counterbalance succeed, it should be number 1 and -1 across participants
  f <- sum(cs_pp_phase[["counterbalance_check"]])
  k <- (sub_num) %% (2)

  if (k == 0 & f == 0) {
    return(TRUE)
  } else if (k == 1 & f == 1) {
    return(TRUE)
  } else if (k == 1 & f == -1) {
    return(TRUE)
  } else {
    return(FALSE)
  }

}


#' Check whether the stimuli allocation is consistent for the CS conditions across all experimental phases for each participant
#' @description For example, pp.1 has two CS stimuli 'CSp_USp_A' and 'CSp_USm_AC', this function examines whether the corresponding...
#' stimulus_names are the same for these two CS stimuli across all experimental phases.
#'
#' @param df A tibble that includes all individuals' trial structures.
#' @param CS_col Name (without ''/"") of the column that refer to CS conditions (e.g., CS_names)
#' @param cs_types A string vector for CS conditions (e.g., c('CSp_USp_A', 'CSp_USm_AC'))
#' @param stimulus_col Name (without ''/"") of the column that refer to stimulus names (e.g., stimulus_name or stimulus_name2)
#' @param pp_col Name (without ''/"") of the column that refer to participants ID
#'
#' @return A boolean value (TRUE/FALSE)
#' @export
#'
#' @examples
#' # test_stimuli_allocation_consistentcy(d, CS_names, c("CSp_USp_BC", "CSp_USm_B", "BD"), stimulus_name, ppid)
#' # test_stimuli_allocation_consistentcy(d, CS_names, c("AB"), stimulus_name2, ppid)
#'
test_stimuli_allocation_consistency <- function(df, CS_col, cs_types,
                                                 stimulus_col, pp_col) {

  number <-
    df %>%
    dplyr::filter({{CS_col}} %in% cs_types) %>%
    dplyr::group_by({{pp_col}}, {{CS_col}}) %>%
    dplyr::summarise(n = dplyr::n_distinct({{stimulus_col}})) %>%
    dplyr::summarise(n2 = dplyr::n_distinct(n)) %>%
    dplyr::pull(n2) %>%
    unique()

  if (number == 1) {
    print(TRUE)
  } else {
    print(FALSE)
  }

}


#' Check the consistency of stimulus allocation across two visual platforms
#' @description for each participant, if one condition stimulus (e.g., A) can appear on both platforms
#' (i.e., stimulus_name and stimulus_name2) on different phases, this function checks
#' whether the allocation is consistent for both platforms
#'
#' @param df A tibble that includes all individuals' trial structures.
#' @param CS_col Name (without ''/"") of the column that refer to CS conditions (e.g., CS_names)
#' @param s_vector1 A string vector for stimulus names (e.g., 'CSp_USp_BC')
#' @param s_vector2 A string vector for stimulus names (e.g., 'CSp_USp_AB')
#' @param stimulus_col Name (without ''/"") of the column that refer to stimulus names (e.g., stimulus_name or stimulus_name2)
#' @param stimulus2_col Name (without ''/"") of the column that refer to stimulus names (e.g., stimulus_name or stimulus_name2)
#' @param pp_col Name (without ''/"") of the column that refer to participants ID
#'
#' @export
#'
#' @examples
#' # test_stimuli_platform_consistentcy(d, CS_names, c("CSp_USm_C", "CD"), c("AC", "CSp_USp_BC"), stimulus_name, stimulus_name2, ppid)
#'
#'
test_stimuli_platform_consistency <- function(df, CS_col, s_vector1, s_vector2,
                                               stimulus_col, stimulus2_col, pp_col) {

  sukan <- NULL

  for (pp in unique(df[["ppid"]])) {
    name_1 <-
      df %>%
      dplyr::filter({{pp_col}} == pp) %>%
      dplyr::filter({{CS_col}} %in% s_vector1) %>%
      dplyr::pull({{stimulus_col}}) %>%
      unique()

    name_2 <-
      df %>%
      dplyr::filter({{pp_col}} == pp) %>%
      dplyr::filter({{CS_col}} %in% s_vector2) %>%
      dplyr::pull({{stimulus2_col}}) %>%
      unique()

    sukan <- c(sukan, (name_1 == name_2))

  }

  all(sukan)


}


#' Summarize the maximum successive number per CS condition
#'
#' @param x A string vector (e.g., c('CS1', 'CS1', 'CS2'))
#'
#' @return A tibble with two columns
#' @export
#'
#' @examples
#' # k <- summarize_max_consecutive(x = c('CS1', 'CS3', 'CS1', 'CS1', 'CS2', 'CS2', 'CS2'))
summarize_max_consecutive <- function(x) {

  # Initialize counters
  count <- setNames(rep(0, length(unique(x))), unique(x))

  max_count <- count


  for (i in x) {
    # Increment counter for the current element
    count[i] <- count[i] + 1


    # Update the max_count for the current element
    max_count[i] <- max(max_count[i], count[i])


    # Reset counters for non-current elements
    count[names(count) != i] <- 0

  }

  # Summarize and arrange the data
  sukan <-
    tibble::tibble(Condition = names(max_count),
                   MaxConsecutive = as.numeric(max_count)) %>%
    dplyr::arrange(-MaxConsecutive)  # Order by descending counts

  return(sukan)

}


#' Check the maximum successive number for each CS condition per phase across all participants
#'
#' @param df A tibble that includes all individuals' trial structures.
#' @param which_phase A string or an integer (e.g., 1 or "avoid_conditioning").
#' @param CS_col Name (without ''/"") of the column that refer to CS conditions (e.g., CS_names)
#' @param phase_col Name (without ''/"") of the column that refer to specific experimental phase (e.g., phase/phase_name)
#'
#' @export
#'
#' @examples
#' # check_max_consequtive(inner_d2, phase_name, "avoid_conditioning", CS_names)
#'
check_max_consecutive <- function(df, phase_col, which_phase, CS_col) {

  sukan <- NULL

  for (i in unique(df$ppid)) {
    sukan.d <- summarize_max_consecutive(df %>%
                                           dplyr::filter({{phase_col}} == which_phase,
                                                  ppid == i) %>%
                                           dplyr::pull({{CS_col}}))

    sukan.d <-
      sukan.d %>%
      dplyr::mutate(ppid = i,
             .before = Condition)

    if (i == 1) {
      sukan <- sukan.d
    } else {
      sukan <- rbind(sukan, sukan.d)
    }

  }

  sukan %>%
    dplyr::group_by(Condition) %>%
    dplyr::summarise(max.value = max(MaxConsecutive)) %>%
    print()

}




















