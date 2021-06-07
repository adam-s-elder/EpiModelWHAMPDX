#' Plotting data from EpiModel simulation
#'
#' @param sim_obj The dat object from an EpiModel Simulation
#' @param plotting_params A list containing information a description of
#' what is to be plotted and what sections of these plots should be named.
#' The list will be a list of lists, in which each sub list contains
#' information for a desired set of plots. See details for more information.
#' @param num_hash The number of hashes for the top level section name
#' @param targ_df The dataframe with the desired targets. The value of the
#' `measure` column for the desired column should match that of the plot_name
#' inside of the plotting_params sublists. See details for more information.
#' @param other_args Experimental.  Pass ggplot commands to be added to the
#' base plot as a string, to be used with a parse eval command.
#' @param sub_atrs Which attributes to break out the measure by.  If no value
#' is given, all present attribute breakdowns will be shown.
#' @param print_sec Logical for if the section title should be printed.
#'
#' @return
#' Print the results of one or more sets of EpiModel diagnostic plots.
#'
#' @details
#' The `plotting_params` object contains
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
#'
#' @examples
#' # see vignette
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr filter
#'
#' @export

print_many_plots <- function(plotting_params, sim_obj,
                             num_hash = 3,
                             targ_df = EpiModelWHAMPDX::WHAMP.targs,
                             othr_args = NULL, sub_atrs = NULL,
                             print_sec = TRUE) {
  big_temp <- paste0(c(rep("#", num_hash),
  " %s {.tabset .tabset-fade .tabset-pills} ", "
", "
 "), collapse = "")
  sml_temp <- paste0(c("\n\n", rep("#", num_hash + 1), " %s ", "
", "
 "), collapse = "")
  for (ms_idx in 1:length(plotting_params)) {
    sub_plot_info <- plotting_params[[ms_idx]]
    if (is.null(sub_plot_info$plt_type)) {
      plt_type <- "line"
      }else{plt_type <- sub_plot_info$plt_type}
    meas_data <- EpiModelWHAMPDX::make_epi_plot_data(sim_obj, sub_plot_info)
    if (!is.null(sub_plot_info$sec_title)) {
      this_meas_name <- sub_plot_info$sec_title
    }else{
      this_meas_name <- sub_plot_info$plot_name
    }
    if (print_sec) {
      cat(sprintf(big_temp, this_meas_name))
    }
    this_targ <- targ_df %>%
      dplyr::filter(sub_plot_info$name == measure)
    attrs_present <- unique(meas_data$epi_data$cat_name)
    if (!is.null(sub_atrs)) {attrs_present <- sub_atrs}
    for (ct_idx in seq(attrs_present)) {
      this_cat <- attrs_present[ct_idx]
      cat(sprintf(sml_temp, this_cat))
      first_plt <- EpiModelWHAMPDX::plot_epi(
        meas_data, this_cat, this_targ,
        plot_type = plt_type, year_range = c(1980, 2030))
      if (!is.null(othr_args)) {
        final_p <- eval(parse(
          text = paste0(c("first_plt", othr_args), collapse = " + "))
        )
      }else {final_p <- first_plt}
      print(final_p)
      cat("\n\n")
    }
  }
}
