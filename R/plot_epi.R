#' Create a plot of epi measures from EpiModel simulation
#'
#' @param plot_data Data object that is output from
#' @param brk_across the attribute over which you want to break out the measure
#' (for example race).
#' @param targets A dataframe that contains the targets for the measure of
#' interest.  Note that plot_data should be specific to a singe measure.
#' @param plot_type type of geom to call.  Default is for a line plot
#' (geom_line) when `plot_type == "line"`.  Otherwise `geom_smooth` is called.
#' @param year_range Range of years over which plot should be created.
#'
#' @importFrom magrittr %>%
#'
#' @return
#' a ggplot object.
#'
#' @export
#'
#' @examples
#' # See vignette

plot_epi <- function(plot_data, brk_across = "ovr",
                     targets = NULL, plot_type = "line",
                     year_range = c(2000, 2030)) {
  requireNamespace("magrittr")
  this_dat <- plot_data$epi_data %>%
    dplyr::filter(cat_name == brk_across, year > year_range[1],
           year < year_range[2])
  meas <- unique(this_dat$meas)
  if (!is.null(targets)) {
    our_targ <- targets %>%
      dplyr::filter(cat == brk_across, measure %in% meas)
    if ((nrow(our_targ) == 0) & TRUE) our_targ <- NULL
    if (!is.null(our_targ) &
        any(our_targ$low == our_targ$high, na.rm = TRUE)) {
      which_match <- which(our_targ$low == our_targ$high)
      our_targ$low <- our_targ$low * 0.99
      our_targ$high <- our_targ$high * 1.01
    }
  }else{our_targ <- NULL}
  if (length(meas) > 1) {
    init_plot <- ggplot2::ggplot(
      this_dat, ggplot2::aes(x = year, y = value,
                             group = interaction(simno, sub_cat_name),
                    linetype = meas, color = sub_cat_name)) +
      ggplot2::scale_color_discrete(name = brk_across)
  }else if ("meta_measure" %in% colnames(this_dat)){
    init_plot <- ggplot2::ggplot(
      this_dat, ggplot2::aes(
        x = year, y = value,
        group = interaction(simno, meta_measure, sub_cat_name),
        linetype = meta_measure,
        color = sub_cat_name)) +
      ggplot2::scale_color_discrete(name = brk_across)
  }else{
    init_plot <- ggplot2::ggplot(
      this_dat, ggplot2::aes(x = year, y = value,
                             group = interaction(simno, sub_cat_name),
                             color = sub_cat_name)) +
      ggplot2::scale_color_discrete(name = brk_across)
  }
  if (!is.null(our_targ$low)) {
    targ_plt_df <- dplyr::bind_rows(
      our_targ %>% dplyr::mutate(year = year_range[1]),
      our_targ %>% dplyr::mutate(year = year_range[2])
    )
    rib_alpha <- ifelse(brk_across == "ovr", 0.3, 0)
    line_alpha <- 1
    targ_plt_df$sub_cat_name <- targ_plt_df$sub_cat
    targ_plt_df$meta_measure <- NA
    targ_plt_df$simno <- 1
    init_plot <- init_plot +
      ggplot2::geom_ribbon(data = targ_plt_df, y = NA, color = NA,
                  ggplot2::aes(fill = sub_cat_name,
                               group = sub_cat_name,
                               ymin = low, ymax = high),
                  alpha = rib_alpha) +
      ggplot2::geom_line(
        data = targ_plt_df,
        ggplot2::aes(y = targ,
                     color = sub_cat_name,

        ),linetype = 1, alpha = line_alpha
        ) +
      ggplot2::scale_fill_discrete(name = brk_across)
  }
  if (plot_type == "line") {
    upd_plot <- init_plot + ggplot2::geom_line()
  } else if (plot_type == "smooth") {
    upd_plot <- init_plot +
      ggplot2::geom_smooth(se = FALSE, method = "loess", formula = y ~ x)
  }
  if (!is.null(plot_data$plot_ylab)) {
    yaxis_inf <- ggplot2::element_text()
    upd_plot <- upd_plot + ylab(plot_data$plot_ylab)
  }else{
    yaxis_inf <- ggplot2::element_blank()
  }
  if (!is.null(plot_data$plot_cap)) {
    upd_plot <- upd_plot + labs(caption = plot_data$plot_cap)
  }
  if (brk_across != "ovr") {
    plt_title <- paste0(
      plot_data$plot_title, " by ", dplyr::recode(
        brk_across, "region" = "Region", "age.grp" = "Age Group",
        "race" = "Race"
      )
    )
  }else{ plt_title <- plot_data$plot_title }
  fin_plot <- upd_plot + cowplot::theme_cowplot() +
    ggplot2::theme(axis.title.y = yaxis_inf,
                   legend.position = "right",
          legend.box = "vertical", legend.margin = ggplot2::margin(),
          panel.background = ggplot2::element_rect(fill = "#E0E2E7"),
          panel.grid = ggplot2::element_line(color = "#FFFFFF", size = 0.4)) +
    ggplot2::xlab("Year") +
    ggplot2::ggtitle(plt_title)
  disp_imp_yrs <- TRUE
  if (disp_imp_yrs) {
    yr_dat <- plot_data$impt_years
    ylim <-
      ggplot2::ggplot_build(fin_plot)$layout$panel_scales_y[[1]]$range$range
    yr_dat$meas <- meas[1]
    yr_dat$sub_cat_name <- NA
    yr_dat$meta_measure <- NA
    yr_dat$simno <- NA
    fin_plot <- fin_plot +
      ggplot2::geom_vline(data = yr_dat, ggplot2::aes(xintercept = year),
                          alpha = 0.5) +
      ggrepel::geom_label_repel(data = yr_dat,
                                ggplot2::aes(x = year, label = name),
                                color = "black",
                                y = ylim[2], direction = "y",
                                alpha = 0.5,
                                min.segment.length = 10)
    fin_plot <- fin_plot + ggplot2::theme(legend.box = "vertical") +
      ggplot2::scale_linetype_discrete(name = "Measure")
  }
  if (brk_across %in% c("age.grp")) {
    cols <- c("#4575b4", "#0088db", "#4ad4f8",
              "#feb010", "#fc4a4a", "#d70000")
      # c("#b2182b", "#ef8a62", "#fddbc7",
      #         "#d1e5f0", "#67a9cf", "#2166ac")
    fin_plot <- fin_plot + ggplot2::scale_color_manual(
      name = "Age Group", values = cols) +
      ggplot2::guides(fill = FALSE)  #+
      # theme(
      #   panel.background = element_rect(fill = "#9B9DA0")
      # )
  }
  if (brk_across == "ovr") {
    fin_plot <- suppressMessages(
      fin_plot + ggplot2::scale_color_discrete(guide = "none") +
        ggplot2::scale_fill_discrete(guide = "none")
    )
  }
  fin_plot
}


