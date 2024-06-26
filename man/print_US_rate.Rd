% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/preprocess_functions.R
\name{print_US_rate}
\alias{print_US_rate}
\title{Verify US rate for each CS}
\usage{
print_US_rate(df)
}
\arguments{
\item{df}{A trial results tibble}
}
\description{
Extracts the US rate for each participant, phase, and CS, and prints them
on the screen. This provides a basic manipulation check.
}
\examples{
d <- tibble::tibble(ppid = rep("pp.1", times = 8),
            phase = rep(2, times = 8),
            CS = rep("CSp", times = 8),
            punishment_provided = c(1,0,1,1,1,0,1,1))

print_US_rate(d)
}
