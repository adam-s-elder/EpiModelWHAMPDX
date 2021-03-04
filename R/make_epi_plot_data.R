#' Create data for plotting from EpiModel simulation
#'
#' @param dat_obj The dat object from an EpiModel Simulation
#' @param plot_params A list containing information about the information
#' to be plotted and how to extract this information from the dat object.
#' See details for more information.
#'
#' @return
#' A list with the needed objects to be passed to the plot_epi function.
#'
#' @details
#' The `plot_params` object contains at least two elements.  The first is the
#' `plot_name` argument which is the formal name of the measure you wish to
#' plot.  The other `name` which is the name of the variable that will be
#' as it appears in the epi list object (dat$epi).
#'
#' If the epi measure you wish to plot must be constructed, two additional
#' arguments must be given, and the `name` argument can be chosen by the user.
#' While `name` can be set to any string, the user should make sure this name
#' matches any target they wish to match.
#'
#' The first additional argument that must be specified is the `vars`.
#' This argument gives the names of the epi measures currently stored inside
#' of epi that will be used to construct the epi measure of interest.  As an
#' example, if the measure of interest was prevalence, the `vars` argument would
#' be `c("num.", "num.i.")` because the number and number of individuals
#' infected could be used to construct prevalence.
#'
#' The second additional argument that must be specified is the `FUN` argument.
#' This argument gives the function that will calculate the epi measure of
#' interest from the `vars` arguments given.  Note that this function should
#' take a number of arguments that matches the length of `vars`.  The function
#' should also take arguments in the order that they appear in `vars`.
#' Continuing our example of prevalence, the function we would specify would be
#' `function(x, y) = y / x`.
#'
#' @export
#'
#' @examples
#' # see vignette
#'
#' @importFrom magrittr %>%
#'

make_epi_plot_data <- function(dat_obj, plot_params) {
  require(dplyr)
  require(tidyr)
  require(magrittr)
  if (any(grepl("num", names(dat_obj$epi)))) {
    dat_obj$epi <- list(dat_obj$epi)
  }
  if (!is.null(plot_params$vars)) {
    upd_dat <- make_new_epi(dat = dat_obj,
                            args = plot_params$vars,
                            FUN = plot_params$sum_fun,
                            new_name = paste0(plot_params$name, ".")
    )
  }else{
    upd_dat <- dat_obj
  }
  req_epi <- list()
  for (atr_idx in 1:4) {
    req_epi <- c(req_epi,
                 upd_dat$epi[[1]][paste0(
                   plot_params$name, ".",
                   names(EpiModelWHAMPDX::attr_names[[atr_idx]])
                 )])
  }
  attr_name_idx <-
    lapply(EpiModelWHAMPDX::attr_names, names) %>%
    lapply(FUN = function(x) paste0("\\.", x)) %>%
    sapply(FUN = function(x) any(grepl(paste(x, collapse = "|"),
                                   names(req_epi),
                                   ))) %>%
    which()
  req_epi <- req_epi[which(sapply(req_epi, length) > 1)]
  all_epi_inf <- do.call(data.frame, req_epi)
  all_epi_inf$at <- 1:nrow(all_epi_inf)
  which_all_na <- apply(all_epi_inf, 2, function(x) mean(is.na(x))) == 1
  all_epi_inf <- all_epi_inf[, !which_all_na]
  all_epi_inf <- all_epi_inf[stats::complete.cases(all_epi_inf), ]
  all_epi_inf$year <- (all_epi_inf$at - dat_obj$control$start + 1) / 52 +
    dat_obj$control$year_start
  tidy_epi_info <- all_epi_inf %>% dplyr::select(-at) %>%
    tidyr::pivot_longer(cols = -year)
  cmbd_names <- tidy_epi_info$name
  splt_names <- strsplit(cmbd_names, "\\.")
  tidy_epi_info$meas <- sapply(splt_names, FUN = function(x){
    xlen <- length(x)
    paste0(x[-xlen], collapse = ".")
  })
  if (!all(tidy_epi_info$meas == plot_params$name)) {
    warning("Some measures in the data frame are for an unexpected measure.")
  }
  sub_names <- sapply(splt_names, FUN = function(x){
    xlen <- length(x)
    paste0(x[xlen], collapse = ".")
  })
  sub_cat_names <- cat_names <- rep(NA, length(sub_names))
  for (atr_idx in attr_name_idx) {
    attr_vl <- EpiModelWHAMPDX::attr_names[[atr_idx]]
    attr_vl_name <- names(EpiModelWHAMPDX::attr_names)[atr_idx]
    for (sub_atr_idx in 1:length(attr_vl)) {
      w_match <- which(sub_names == names(attr_vl)[sub_atr_idx])
      sub_cat_names[w_match] <- attr_vl[sub_atr_idx]
      cat_names[w_match] <- attr_vl_name
    }
  }
  tidy_epi_info$sub_cat_name <- sub_cat_names
  tidy_epi_info$cat_name <- cat_names
  impt_yrs <- c(
    "mtch.demog" = max(dat_obj$param$demog_match_arrival_df$entry_year),
    "adap.start" = dat_obj$control$adap.year.start,
    "pdap.start" = dat_obj$control$pdap.year.start,
    "prep.start" = dat_obj$control$risk.hist.start.year,
    "cure.start" = dat_obj$param$jnt_prev_targ_yr - 2,
    "cure.end" = dat_obj$param$jnt_prev_targ_yr
  )
  impt_yrs <- data.frame(name = names(impt_yrs), year = impt_yrs)
  rownames(impt_yrs) <- NULL
  return(
    list(plot_title = plot_params$plot_name,
         epi_data = tidy_epi_info,
         impt_years = impt_yrs)
  )
}
