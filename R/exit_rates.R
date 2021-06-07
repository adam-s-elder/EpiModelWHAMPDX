#' Create a plot of the exiting rate of nodes in a population.
#'
#' This function calculates the mortality rates by comparing the population
#' distribution at different time steps.  For example if there are 50
#' individuals in the population age 30 at year 1990 and only 45 individuals
#' in the population age 31 at year 1991, we can determine that 5 individuals
#' in this age cohort died.
#'
#' @param sim The simulation for which you would like to plot the exit rates.
#'
#' @details Note that because of the incredibly small mortality rates for
#' younger individuals, the plots for the younger individuals will be noisy.
#' @importFrom WApopdata get_data
#'
#' @export

make_exit_plot <- function(sim) {
  ad <- sim$epi[[1]]$age_dist %>% as.data.frame()
  wks_in <- sim$control$start - min(ad$at)
  tr_sy <- sim$control$year.start - wks_in / 52
  ad$year <- round(tr_sy + ad$at /52)
  ad <- ad %>% group_by(year, race, age) %>%
    summarise(count = sum(count)) %>% ungroup()
  ad$obs_mort <- NA_real_
  for (yrs in sort(unique(ad$year))[-1]) {
    this_yr <- ad %>% filter(year == yrs) %>%
      mutate(cur_cnt = count)
    last_yr <- ad %>% filter(year == yrs - 1) %>%
      mutate(age = age + 1, lst_cnt = count)
    cmbr <- left_join(this_yr, last_yr,
                      by = c("age", "race")) %>%
      mutate(obs_mort = (lst_cnt - cur_cnt) / lst_cnt)
    ad[ad$year == yrs, "obs_mort"] <- cmbr$obs_mort
  }
  mean_obs_morts <- ad %>% group_by(race, age) %>%
    summarise(mort = mean(obs_mort, na.rm = TRUE)) %>%
    select(race, age, mort)
  trg_morts <- WApopdata::get_data("asmr_by_race") %>%
    filter(age > 15) %>%
    tidyr::pivot_longer(cols = 2:4, names_to = "race",
                        values_to = "mort") %>%
    mutate(mort = 1 - (1 - mort) ** 52) %>%
    mutate(race = gsub("vec.asmr.", "", race))
  all_morts <- bind_rows(
    bind_cols(mean_obs_morts, type = "observed"),
    bind_cols(trg_morts, type = "target")
  )

  young <- all_morts %>% filter(age < 40) %>% mutate(mort = pmax(mort, 0.001)) %>%
    ggplot(aes(x = age, y = mort, shape = type,
               lty = type, color = race)) +
    geom_point(size = 2) + geom_smooth(se = FALSE, method = "lm") +
    coord_cartesian(ylim = c(0.001, 0.01)) + scale_y_log10() +
    facet_wrap(~ race) + theme(legend.position = "none")
  old <- all_morts %>% filter(age >= 40) %>%
    mutate(mort = pmax(mort, 0.001)) %>%
    ggplot(aes(x = age, y = mort, shape = type,
               lty = type, color = race)) +
    geom_point(size = 2) + geom_smooth(se = FALSE, method = "lm") +
    coord_cartesian(ylim = c(0.001, 0.125)) + scale_y_log10() +
    facet_wrap(~ race) + theme(legend.position = "bottom")
  cowplot::plot_grid(
    young, old,
    labels = c("Younger", "Older"),
    rel_heights = c(1, 1.3),
    ncol = 1
  )
}
