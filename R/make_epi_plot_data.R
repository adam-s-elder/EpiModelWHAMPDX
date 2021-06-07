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
  requireNamespace("magrittr")
  if (any(grepl("num", names(dat_obj$epi)))) {
    dat_obj$epi <- list(dat_obj$epi)
  }
  if (class(plot_params$vars) == "list") {
    expnd_plot_params <- .expand_plot_params(plot_params)
    mult_meas <- TRUE
  }else{
    expnd_plot_params <- list(plot_params)
    mult_meas <- FALSE
  }
  for (plt_idx in seq_along(expnd_plot_params)) {
    sub_params <- expnd_plot_params[[plt_idx]]
    if (!is.null(sub_params$vars)) {
      upd_dat <- EpiModelWHAMPDX::make_new_epi(
        dat = dat_obj, args = sub_params$vars,
        FUN = sub_params$sum_fun, new_name = paste0(sub_params$name, ".")
      )
    }else{
      upd_dat <- dat_obj
    }
    req_epi <- list()
    for (atr_idx in seq(EpiModelWHAMPDX::attr_names)) {
      req_epi <- c(req_epi,
                   upd_dat$epi[[1]][paste0(
                     sub_params$name, ".",
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
      dat_obj$control$year.start
    tidy_epi_info <- all_epi_inf %>% dplyr::select(-at) %>%
      tidyr::pivot_longer(cols = -year)
    cmbd_names <- tidy_epi_info$name
    splt_names <- strsplit(cmbd_names, "\\.")
    tidy_epi_info$meas <- sapply(splt_names, FUN = function(x){
      xlen <- length(x)
      paste0(x[-xlen], collapse = ".")
    })
    if (!all(tidy_epi_info$meas == sub_params$name)) {
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
    if (mult_meas) {
      tidy_epi_info$meta_measure <- sub_params$var_name
      if (exists("all_epi_info")){
        all_epi_info <- dplyr::bind_rows(all_epi_info, tidy_epi_info)
      }else{
        all_epi_info <- tidy_epi_info
      }
    }
  }
  if (mult_meas) tidy_epi_info <- all_epi_info
  impt_yrs <- c(
    "mtch.demog" = max(dat_obj$param$demog$match.arrival$entry_year),
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
         plot_cap = plot_params$plot_cap,
         plot_ylab = plot_params$plot_ylab,
         epi_data = tidy_epi_info,
         impt_years = impt_yrs)
  )
}

.expand_plot_params <- function(plot_params) {
  def_param <- plot_params
  def_param$var_names <- NULL
  fin_param <- list()
  for (pr_indx in seq_along(plot_params$var_names)) {
    sub_param <- def_param
    sub_param$vars <- plot_params$vars[[pr_indx]]
    sub_param$var_name <- plot_params$var_names[
      min(pr_indx, length(plot_params$var_names))
      ]
    fin_param <- c(fin_param, list(sub_param))
  }
  return(fin_param)
}
