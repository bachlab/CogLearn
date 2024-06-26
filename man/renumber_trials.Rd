% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/preprocess_functions.R
\name{renumber_trials}
\alias{renumber_trials}
\title{Re-number trials across separate VR sessions}
\usage{
renumber_trials(session, trial_num)
}
\arguments{
\item{session}{A trial-by-trial integer vector with session numbers}

\item{trial_num}{A trial-by-trial integer vector with trial numbers}
}
\value{
A trial_by_trial vector of successive trial number
}
\description{
If an experiment spans several VR sessions or ppids (e.g. days) then trial numbers start
from 1 for each session. This functions re-numbers to successive trial numbers
}
\examples{
d <- c(1:7)
t <- c(1:7)
renumber_trials(d, t)
}
