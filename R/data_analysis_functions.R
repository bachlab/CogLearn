#' Summarise all outcome variables of a specific phase
#'
#' @description For each outcome variable per condition (CS+/CS-), this function
#' computes the condition means, using summarise
#'
#' @param df A trial results tibble
#' @param DVs A string vector of outcome variables (e.g., c('mean_distance', 'maximum_distance'))
#' @param i_phase An integer that indicates a specific experimental phase (e.g., 1)
#' @param cs_types A string vector of expected CS types (e.g., 'CS1p', 'CS1m')
#'
#' @return A summarised tibble
#' @export
#'
summarise_outcome_variables <-
  function(df, i_phase, DVs, cs_types = c('CSp', 'CSm')) {
    df %>%
      dplyr::ungroup() %>%
      dplyr::filter(CS %in% cs_types,
                    phase == i_phase) %>%
      # compute condition means and SD
      dplyr::group_by(CS) %>%
      dplyr::summarise(dplyr::across(
        .cols = tidyselect::any_of(DVs),
        .fns = ~ mean(.x, na.rm = T),
        .names = "{.col}"
      ))
}


#' Summarise numerical demographic information
#'
#' @param df A tibble with participant information
#' @param DVs A string vector of demographic variables (default "age")
#'
#' @return A summarised tibble
#'
#' @examples
#' d <- tibble::tibble(age = c(21:40))
#' summarise_demographics_num(d)
#'
#' @export
summarise_demographics_num <- function(df, DVs = "age") {
  df %>%
    # remove NAs
    dplyr::filter(!is.na(age)) %>%
    dplyr::summarise(dplyr::across(
      .cols = tidyselect::any_of(DVs),
      .fns = list(
        min = min,
        mean = mean,
        sd = sd,
        max = max
      ),
      .names = "{.col}.{.fn}"
    ))
}


#' Summary frequency demographic information
#'
#' @param df A tibble with participant information
#' @param DVs A string vector of demographic variables (default "sex")
#'
#' @return A summarised tibble
#'
#' @examples
#' d <- tibble::tibble(sex = c('male', 'male', 'male',
#'                     rep("female", times = 4),
#'                     rep("non-binary", times = 5)))
#'
#' summarise_demographics_freq(d)
#' @export
summarise_demographics_freq <- function(df, DVs = "sex") {
  df %>%
    dplyr::group_by(tidyselect::any_of(DVs)) %>%
    dplyr::summarise(n = dplyr::n())
}


#' Compute effect sizes for a given DV of a specific phase
#'
#' @description This function performs t-test and computes cohens_d and hedges_g
#'
#' @param df A trial results tibble
#' @param DV A string (e.g., "mean distance")
#' @param i_phase An integer or a string, which indicate a specific phase
#' @param cs_types A string vector of needed CS types (e.g., 'CS1p', 'CS1m')
#'
#' @return A list object
#' @export
summary_effect_size <- function(df, i_phase, DV, cs_types = c('CSp', 'CSm')) {

  # compute the condition difference per participant
  df <-
    df %>%
    dplyr::ungroup() %>%
    dplyr::filter(phase %in% c(i_phase),
                  CS %in% cs_types) %>%
    dplyr::group_by(ppid, CS) %>%
    dplyr::summarise(across(.cols = DV,
                            .fns = ~ mean(.x, na.rm = T),
                            .names = "{.col}_sub")
    ) %>%
    dplyr::ungroup()


  DV <- paste0(DV, "_sub")

  mdl_formula <- as.formula(paste0(DV, " ~ CS"))

  # return if not enough observations
  insufficient_obs <- any(unlist(lapply(cs_types, function(cs) {
    nrow(df %>% dplyr::filter(CS == cs & !is.na(.data[[DV]]))) < 2
  })))

  if (insufficient_obs) return(NA)


  # exclude subjects with missing observations
  missing_ppid <-
    df %>%
    dplyr::filter(!is.na(.data[[DV]])) %>%
    dplyr::group_by(ppid) %>%
    dplyr::summarise(nobs = n()) %>%
    dplyr::filter(nobs < 2) %>%
    dplyr::pull(ppid)

  if (length(missing_ppid) > 0)
    df <-
    df %>%
    dplyr::filter(!(ppid %in% missing_ppid))


  eff_size_list <- list(t_test = t.test(mdl_formula, data = df, paired = TRUE),
                        c_d = effectsize::cohens_d(x = df[[DV]], y = df[["CS"]], paired = TRUE),
                        h_g = effectsize::hedges_g(x = df[[DV]], y = df[["CS"]], paired = TRUE))

  return(eff_size_list)

}
































