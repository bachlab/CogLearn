#' Draw phase-level plots
#'
#' @description
#' For each outcome variable per condition (CS+/CS-), this function will plot
#' the average value and its error bars phase-by-phase
#'
#' @param df A trial results tibble
#' @param phases Experimental phases
#' @param DV Outcome variable (e.g., 'mean_dist_CS')
#' @param cs_types A string vector of needed CS types (e.g., 'CS1p', 'CS1m')
#' @param fill_colors A string vector of fill colors for different CS conditions (e.g., 'red', 'lightgreen')
#'
#' @export
#' @examples
#' # plot_by_phase(inner_d, c(1,2), "mean_dist_CS", cs_types = c('CS1p', 'CS1m'), fill_colors = c('red', 'lightgreen', 'blue', 'yellow'))
#'
#'
plot_by_phase <- function(df, phases, DV,
                          cs_types = c('CSp', 'CSm'), fill_colors = c('red', 'lightgreen', 'blue', 'yellow')) {
  ## Set the them at the first place
  ggplot2::theme_set(vrthreat::theme_vrthreat())
  df %>%
    ## select data for phases of interest
    dplyr::filter(phase %in% phases,
                  CS %in% cs_types) %>%
    ## remove trials with missing values for variables of interest
    dplyr::filter(!is.na(.data[[DV]])) %>%
    dplyr::mutate(phase = factor(phase)) %>%

    ## now begin making the plot
    ggplot2::ggplot(aes(x = phase,y = .data[[DV]], fill = CS)) +
    ggplot2::scale_fill_manual(values = fill_colors) +
    ggbeeswarm::geom_quasirandom(
      ggplot2::aes(color = CS),
      size = 0.2,
      dodge.width = .3,
      groupOnX = TRUE,
      alpha = 0.4
    ) +
    ## add error bars
    ggplot2::stat_summary(
      geom = "errorbar",
      fun.data = mean_se,
      width = .2,
      position = ggplot2::position_dodge(width = 0.3)
    ) +
    ## add the mean value point for each phase
    ggplot2::stat_summary(
      geom = "point",
      fun.data = mean_se,
      size = 0.5,
      position = ggplot2::position_dodge(width = 0.3),
      pch = 21
    ) +
    ggplot2::labs(y = DV) +
    ggplot2::theme(legend.position = "top",
          axis.title.x = ggplot2::element_text(size = 12, face = "bold"),
          axis.title.y = ggplot2::element_text(size = 12, face = "bold"),
          legend.title = ggplot2::element_blank()) +
    ggplot2::guides(color = guide_legend(override.aes = list(size = 2.5)))

}


#' Draw trial-level graphs
#' @description
#' At each trial, this function will plot the average value and its error bars
#' for both conditions (CS+/CS-) across all experimental phases
#'
#' @param df A trial results tibble
#' @param phases Experimental phases
#' @param DV Outcome variables (e.g., 'mean_dist_CS')
#' @param cs_types A string vector of needed CS types (e.g., 'CS1p', 'CS1m')
#' @param fill_colors A string vector of fill colors for different CS conditions (e.g., 'red', 'lightgreen')
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' library(ggplot2)
#'
#' create_sub_data <- function(seed) {
#' set.seed(seed)
#' d_sub <-
#'  tibble::tibble(CS = factor(rep(c('CSp', 'CSm', 'CS2p', 'CS2m'), each = 10))
#'         %>% sample) %>%
#'  dplyr::mutate(
#'    mean_dist_CS = dplyr::case_when(
#'      CS == 'CSp' ~ rnorm(1,2.5,.3),
#'      CS == 'CSm' ~ rnorm(1,1,.3),
#'      CS == 'CS2p' ~ rnorm(1,3,.3),
#'      CS == 'CS2m' ~ rnorm(1, 2, .3)
#'    ),
#'    trial_num_all = 1:n())

#'return(d_sub)
#'}
#'
#'d <- NULL
#'for (i in 1:20) {
#'  d_sub <- create_sub_data(seed = i)
#'  d <- rbind(d, d_sub)
#'}
#'d$pp <- as.character(rep(c(1:20), each = 40))
#'d <-
#'  d %>% dplyr::mutate(phase = rep(2, times = n()))
#'plot_by_trial(d, 2, "mean_dist_CS", cs_types = c('CSp', 'CSm', 'CS2p', 'CS2m'),
#'              fill_colors = c('red', 'lightgreen', 'blue', 'yellow'))
#'
plot_by_trial <- function(df, phases, DV,
                          cs_types = c('CSp', 'CSm'), fill_colors = c('red', 'lightgreen', 'blue', 'yellow')) {

  ## Set the them at the first place
  ggplot2::theme_set(vrthreat::theme_vrthreat())
  df %>%
    ## select data for phases of interest
    dplyr::filter(phase %in% phases,
                  CS %in% cs_types) %>%
    ## remove trials with missing values for variables of interest
    dplyr::filter(!is.na(.data[[DV]])) %>%
    dplyr::mutate(phase = factor(phase)) %>%

    ## now begin making the plot
    ggplot2::ggplot(ggplot2::aes(x = trial_num_all, y = .data[[DV]], fill = CS)) +
    ggplot2::scale_fill_manual(values = fill_colors) +
    ggplot2::stat_summary(
      geom = "errorbar",
      fun.data = mean_se,
      width = .5,
      size = .5,
      position = ggplot2::position_dodge(width = .5)
    ) +
    ## add the mean value point for each phase
    ggplot2::stat_summary(
      geom = "point",
      fun.data = mean_se,
      size = 1.5,
      pch = 21,
      position = ggplot2::position_dodge(width = .5)
    ) +
    ggplot2::labs(y = DV) +
    ggplot2::theme(legend.position = "top",
          axis.title.x = element_text(size = 12, face = "bold"),
          axis.title.y = element_text(size = 12, face = "bold"),
          legend.title = element_blank()) +
    ggplot2::guides(color = guide_legend(override.aes = list(size = 2.5))) +
    ggplot2::facet_grid(. ~ phase,
               scales = "free_x",
               space = "free_x")


}


#' Plot trial-by-trial condition differences
#'
#' @description i.e., on each trial the CS+/CS- difference, collapsed across
#' participants
#'
#' @param df A trial results tibble
#' @param phases A string vector (e.g., c(2:5)) that indicates experimental phases
#' @param DV_col Column name refers to the dependent variable
#' @param phase_col Column name refers to the specific experimental phase
#' @param ppid_col Column name refers to the participants' ID
#' @param trial_num_col Column name refers to the trial number per phase per subject
#' @param CS_col Column name refers to the CS stimuli
#' @param cs_types A string vector of needed CS types (e.g., 'CS1p', 'CS1m')
#' @param smooth A bolean value (TRUE or FALSE)
#' @export
#'
#' @examples
#' library(dplyr)
#' library(ggplot2)
#'
#' create_sub_data <- function(seed) {
#' set.seed(seed)
#' d_sub <-
#'  tibble::tibble(CS = factor(rep(c('CSp', 'CSm'), each = 10))
#'         %>% sample) %>%
#'  dplyr::mutate(
#'    mean_dist_CS = dplyr::case_when(
#'      CS == 'CSp' ~ rnorm(1,2.5,.3),
#'      CS == 'CSm' ~ rnorm(1,.5,.3)
#'    ),
#'    trial_num_all = 1:n())

#'return(d_sub)
#'}
#'
#'d <- NULL
#'for (i in 1:20) {
#'  d_sub <- create_sub_data(seed = i)
#'  d <- rbind(d, d_sub)
#'}
#'
#'d$pp <- as.character(rep(c(1:20), each = 20))
#'
#'d <-
#'  d %>% dplyr::mutate(phase = rep(2, times = n()))
#'
#'plot_by_trial_con_diff(d, 2, mean_dist_CS, phase,
#'                       pp, trial_num_all, CS, cs_types = c('CSp', 'CSm'), smooth = TRUE)
#'
plot_by_trial_con_diff <- function(df, phases, DV_col,
                                   phase_col, ppid_col, trial_num_col, CS_col,
                                   cs_types = c("CSp", "CSm"), smooth = FALSE) {

  # set the theme
  ggplot2::theme_set(vrthreat::theme_vrthreat())

  df_processed <- df %>%
    dplyr::filter({{phase_col}} %in% phases,
                  {{CS_col}} %in% cs_types) %>%
    dplyr::filter(!is.na({{DV_col}})) %>%
    dplyr::mutate(phase = factor({{phase_col}}),
                  trial_num = factor({{trial_num_col}})) %>%
    dplyr::group_by(phase, trial_num, {{CS_col}}) %>%
    dplyr::summarise(mean_value = mean({{DV_col}})) %>%
    tidyr::pivot_wider(names_from = {{CS_col}}, values_from = mean_value)

  # Extract the CS column names after pivot_wider
  new_cs_columns <- setdiff(names(df_processed), c("phase", "trial_num"))

  # Compute the difference
  df_processed <- df_processed %>%
    dplyr::mutate(diff_value = .data[[new_cs_columns[1]]] - .data[[new_cs_columns[2]]]) %>%
    dplyr::ungroup()

  p <- ggplot2::ggplot(data = df_processed, aes(x = trial_num, y = diff_value, group = 1)) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::facet_grid(. ~ phase,
                        scales = "free_x",
                        space = "free_x") +
    ggplot2::geom_hline(yintercept = 0,
                        linetype = "dashed")

  if (smooth) {
    p <- p + ggplot2::geom_smooth(method = "loess")
  }

  p  # Return the plot
}



#' Plot the changes in distances as time goes on at the trial level across all participants
#'
#' @description This function can be used for multiple/1 experimental phases
#'
#' @param head_data A head movement data frame/tibble
#' @param pp_col Column name refers to the participants' ID
#' @param trial_col Column name refers to the trial information
#' @param cs_col Column name refers to the CS stimuli
#' @param cs_level A string vector that refers to CS levels (e.g., 'CSp'/'CS2p', 'CSm'/'CS2m')
#' @param label_x A string (e.g., 'CS')
#' @param label_y A string (e.g., 'mean_dist_CS')
#' @param add_CI A bolean value (TRUE/FALSE), if TRUE then will add CI to the lines
#' @param ref_x An integer (e.g., 0)
#' @param ref_z An integer (e.g., 2.5)
#'
#' @export
#'
plot_by_trial_distance <- function(head_data,
                                pp_col, trial_col, cs_col,
                                cs_level = c('CSp', 'CSm'),
                                label_x = "", label_y = "",
                                add_CI = FALSE, ref_x = 0, ref_z = 2.5) {

  p <-
    head_data %>%
    dplyr::mutate(dist_from_ref = vrthreat::calculate_2d_dist(target_x = pos_x,
                                                    target_z = pos_z,
                                                    ref_x = ref_x,
                                                    ref_z = ref_z)) %>%
    # note that the variable 'new_time' is from the input head_data tibble
    dplyr::select({{pp_col}}, {{trial_col}}, {{cs_col}}, new_time, dist_from_ref) %>%
    dplyr::filter({{cs_col}} %in% cs_level) %>%
    dplyr::mutate(CS = factor({{cs_col}}, levels = cs_level)) %>%
    dplyr::group_by({{trial_col}}, CS, new_time) %>%
    dplyr::summarise(mean_dist = Rmisc::CI(dist_from_ref)[2],
                     dist_upperCI = Rmisc::CI(dist_from_ref)[1],
                     dist_lowerCI = Rmisc::CI(dist_from_ref)[3]) %>%

    ## plot
    ggplot2::ggplot(aes(x = new_time, y = mean_dist, colour = CS)) +
    ggplot2::scale_color_manual(values = c('red', 'lightgreen')) +
    ggplot2::geom_line(size = 1) +
    ggplot2::facet_wrap(vars({{trial_col}})) +
    ggplot2::theme_bw(base_size = 14) +
    ## remove grid lines to assist interpretation
    ggplot2::theme(
          panel.grid.minor = ggplot2::element_blank(),
          panel.grid.major = ggplot2::element_blank()) +
    ggplot2::labs(x = label_x,
         y = label_y,
         title = "")

  if (add_CI == TRUE) {
    p <- p +
      ggplot2::geom_ribbon(aes(ymin = dist_lowerCI, ymax = dist_upperCI,
                               fill = CS, color = CS),
                  alpha = .05)

  }

  print(p)


}


#' Plot participant's movement trajectories trial-by-trial
#'
#' @description This function will make 3 panels: the 1st panel is the changes in
#' distances as time goes on per trial; the 2nd panel is the movement trajectories
#' per trial; the 3rd panel is one specific participant's changes in distance averaged across
#' all trials of a specific experimental phase
#'
#' @param start_point A movement data tibble
#' @param dist_D A movement data tibble
#' @param which_exp A string indicates a specific experiment (e.g., 'exp6')
#' @param which_phase An integer indicates an experimental phase (e.g., 1)
#' @param pp An integer indicates one particular participant (e.g, 5)
#' @param phase_col Column name indicates the variable experimental phase (e.g., phase)
#' @param cs_col Column name indicates the CS stimuli (e.g., CS)
#' @param pp_col Column name indicates indiciates the participants' ID (e,g., ppid)
#' @param trial_col Column name indicates indicates the trial information (e.g., trial)
#' @param cs_level A string vector indicates CS stimuli levels (e.g., c('CSp', 'CSm'))
#' @param ref_x An integer indicates the reference point at the left-right direction (e.g., 0)
#' @param ref_z An integer indicates the reference point at the forwar-backward direction (e.g., 2.5)
#'              actually in this CogLearn package the z-axis is the y-axis
#'
#' @export
#'
plot_by_trial_movement <- function(start_point, dist_D, which_exp, which_phase, pp,
                                   phase_col, cs_col, pp_col, trial_col,
                                   cs_level = c('CSp', 'CSm'), ref_x = 0, ref_z = 2.5) {

  # tidy the input data first by calculate the moment-by-moent 2D distance from the location of CSs
  dist_D <-
    dist_D %>%
    # calculate the moment-by-moment 2D distance from the location of CSs
    dplyr::mutate(dist_from_ref = vrthreat::calculate_2d_dist(target_x = pos_x,
                                                    target_z = pos_z,
                                                    ref_x = ref_x,
                                                    ref_z = ref_z)) %>%
    # remove NAs
    dplyr::filter(!is.na(dist_from_ref))

  # Plot distance data
  p1 <- dist_D %>%
    # select specific phase of specific participant
    dplyr::filter({{phase_col}} == which_phase,
                  {{cs_col}} %in% cs_level,
                  {{pp_col}} == pp) %>%
    dplyr::mutate(CS = factor({{cs_col}}, levels = cs_level)) %>%
    ggplot2::ggplot(aes(x = new_time, y = dist_from_ref, colour = CS)) +
    ggplot2::scale_color_manual(values = c('red', 'lightgreen')) +
    ggplot2::geom_line(size = 1) +
    ggplot2::facet_wrap(vars({{trial_col}})) +
    ggplot2::theme_bw(base_size = 10) +
    # remove grid lines to assist interpretation
    theme(panel.grid.minor = ggplot2::element_blank(),
          panel.grid.major = ggplot2::element_blank()) +
    ggplot2::labs(x = "Time", y = "Distance (m)")

  # Plot tarjecotry
  p2 <- dist_D %>%
    # select specific phase of specific participant
    dplyr::filter({{phase_col}} == which_phase,
                  {{cs_col}} %in% cs_level,
                  {{pp_col}} == pp) %>%
    ggplot(aes(x = pos_x, y = pos_z,
               colour = speed)) +
    ggplot2::scale_colour_continuous(type = "viridis")+
    # plot the 2D trajectory
    ggplot2::geom_path(size = 1,
              alpha = 1) +
    # mark the location of the CSm/CSp on each trial
    ggplot2::geom_point(data = tibble(
                            x = ref_x,
                             y = ref_z),
               aes(x = x, y = y),
               colour = "lightgreen",
               size = 1.5,
               alpha = .8) +
    # mark the starting point for each trial
    ggplot2::geom_point(data = start_point %>%
                 dplyr::filter({{phase_col}} == which_phase,
                               {{cs_col}} %in% cs_level,
                               {{pp_col}} == pp),
               aes(x = pos_x, y = pos_z),
               colour = "#FF6666",
               alpha  = .8,
               size = 1)+
    ggplot2::facet_wrap(vars({{trial_col}}, {{cs_col}})) +

    ggplot2::theme_bw(base_size = 10) +
    # remove grid lines to assist interpretation
    theme(panel.grid.minor = ggplot2::element_blank(),
          panel.grid.major = ggplot2::element_blank()) +
    ggplot2::ylim(c(-4, 4)) +
    ggplot2::xlim(c(-4, 4)) +
    ggplot2::coord_fixed(ratio = 1) +
    ggplot2::labs(x = "Left-rignt (m)",
         y = "Forward-backward (m)")

  # For each subject, draw phase-level plot across all trials
  # Plot the distance data
  p3 <- dist_D %>%
    ## select specific phase of specific participant
    dplyr::filter({{phase_col}} == which_phase,
                  {{cs_col}} %in% c('CSp', 'CSm'),
                  {{pp_col}} == pp) %>%
    dplyr::mutate(CS = factor({{cs_col}}, levels = c('CSp', 'CSm'))) %>%
    dplyr::group_by(new_time, CS) %>%
    dplyr::summarise(mean_dist = mean(dist_from_ref),
                     se = plotrix::std.error(dist_from_ref)) %>%
    ## plot
    ggplot2::ggplot(aes(x = new_time, y = mean_dist, colour = CS)) +
    ggplot2::scale_color_manual(values = c('red', 'lightgreen')) +
    ggplot2::geom_line(size = 1) +
    ggplot2::theme_bw(base_size = 10) +
    ## remove grid lines to assist interpretation
    theme(panel.grid.minor = ggplot2::element_blank(),
          panel.grid.major = ggplot2::element_blank()) +
    ggplot2::labs(title = "") +
    ggplot2::geom_ribbon(aes(ymin = (mean_dist - se),
                             ymax = (mean_dist + se), color = CS),
                alpha = .05)

  # combine the above two plots
  p1 + p2 + p3 +
    patchwork::plot_layout(heights = c(3, 1),
                widths = c(1, 1.3),
                ncol = 2,
                nrow = 2) +
    patchwork::plot_annotation(title = paste(which_exp, "_phase",which_phase,"_pp",pp,
                                             sep = ""),
                               subtitle = "distance & trajectory")

}










