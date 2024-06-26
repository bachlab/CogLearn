% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_functions.R
\name{plot_by_trial_movement}
\alias{plot_by_trial_movement}
\title{Plot participant's movement trajectories trial-by-trial}
\usage{
plot_by_trial_movement(
  start_point,
  dist_D,
  which_exp,
  which_phase,
  pp,
  phase_col,
  cs_col,
  pp_col,
  trial_col,
  cs_level = c("CSp", "CSm"),
  ref_x = 0,
  ref_z = 2.5
)
}
\arguments{
\item{start_point}{A movement data tibble}

\item{dist_D}{A movement data tibble}

\item{which_exp}{A string indicates a specific experiment (e.g., 'exp6')}

\item{which_phase}{An integer indicates an experimental phase (e.g., 1)}

\item{pp}{An integer indicates one particular participant (e.g, 5)}

\item{phase_col}{Column name indicates the variable experimental phase (e.g., phase)}

\item{cs_col}{Column name indicates the CS stimuli (e.g., CS)}

\item{pp_col}{Column name indicates indiciates the participants' ID (e,g., ppid)}

\item{trial_col}{Column name indicates indicates the trial information (e.g., trial)}

\item{cs_level}{A string vector indicates CS stimuli levels (e.g., c('CSp', 'CSm'))}

\item{ref_x}{An integer indicates the reference point at the left-right direction (e.g., 0)}

\item{ref_z}{An integer indicates the reference point at the forwar-backward direction (e.g., 2.5)
actually in this CogLearn package the z-axis is the y-axis}
}
\description{
This function will make 3 panels: the 1st panel is the changes in
distances as time goes on per trial; the 2nd panel is the movement trajectories
per trial; the 3rd panel is one specific participant's changes in distance averaged across
all trials of a specific experimental phase
}
