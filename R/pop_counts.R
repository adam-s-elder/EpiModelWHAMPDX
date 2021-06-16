#' Population counts
#'
#' Population counts split by active (Age < 65) and inactive (Age >= 65)
#'
#' @param sim The simulation you wish to see.
#' @param race_spec Are race specific populaiton counts desired.
#' @param ofm_proj If desired the OFM based population projections.
#'
#' @export
make_split_num_plot <- function(sim, race_spec = FALSE, ofm_proj = NULL) {
  age_cnts <- map(sim$temp, function(x) {
    x$age_dist %>%
    mutate(Age = c("Active (< 65)",
                   "Inactive (>65)")[2 - as.numeric(age < 65)])
  })
  if (race_spec) {
    age_per_year <- age_cnts %>% group_by(at, Age, race) %>%
      summarise(count = sum(count)) %>%
      mutate(year = round((at - sim$control$start) / 52 +
                            sim$control$year.start))
    if (!is.null(ofm_proj)) {
      age_per_year$type <- "Simulated"
      age_per_year <- age_per_year %>% filter(year > 2010) %>% group_by(year) %>%
        mutate(prop = count / sum(count)) %>%
        select(year, prop, race, type, Age)
      age_per_year <- bind_rows(age_per_year, ofm_proj)
      swtch_yr <- sim$param$demog$match.arrival$entry_year
      fin_plot <- age_per_year %>%
        ggplot(aes(x = year, y = 100 * prop,
                   linetype = type, color = Age)) +
        geom_line(size = 2) +
        geom_vline(xintercept = max(swtch_yr)) +
        facet_wrap(~ race, scales = "free_y") +
        ylab("Percent") + xlab("Year") +
        theme(legend.position = "bottom",
              legend.key.width = unit(3, "line"))
      return(fin_plot)
    }
    age_per_year %>%
      ggplot(aes(x = year, y = count, color = Age)) +
      geom_line(size = 2) +
      geom_vline(xintercept = max(sim$param$demog$match.arrival$entry_year)) +
      facet_wrap(~ race) + theme(legend.position = "bottom")
  } else {
    age_per_year <- map(age_cnts, function(x) {x %>% group_by(at, Age) %>%
      summarise(count = sum(count)) %>%
      mutate(year = round((at - sim$control$start) / 52 +
                            sim$control$year.start)) %>%
      ungroup() %>% select(-at)
    })
    start_grwth <- max(sim$param$demog$match.arrival$entry_year)
    maxyear <- sim$control$year.start +
      (sim$control$nsteps - sim$control$start) / 52
    netsize_st <- map(age_per_year, function(x) x$count[
      which.min((x$year - start_grwth) ** 2)])
    grwt <- map(netsize_st, function(x) {sim$param$demog$pop.growth %>%
      filter((year) >= (start_grwth) & year <= (maxyear - 1)) %>%
      mutate(count = s_perc * x, Age = "Active (< 65)",
             year = year + 1, simno = 1) %>%
      select(count, year, Age)})
    grwt.fin <- do.call(bind_rows, grwt)
    grwt.fin$simno <- rep(1:length(grwt),
                          each = nrow(grwt[[1]]))
    apy <- do.call(bind_rows, age_per_year)
    apy$simno <- rep(1:length(age_per_year),
                     each = nrow(age_per_year[[1]]))
    fin_df <- bind_rows(
      bind_cols(apy, "Source" = "Simulation"),
      bind_cols(grwt.fin, "Source" = "Target"),
    )
    fin_df %>% ggplot(aes(x = year, y = count, color = Age,
                          group = interaction(simno, Age, Source),
                          linetype = Source)) +
      geom_line(size = 1.5, alpha = 0.85) +
      geom_vline(xintercept = max(sim$param$demog$match.arrival$entry_year)) +
      ylab("Population Size") + xlab("Year") +
      theme(legend.position = "bottom")
  }
}

#' Make plot of overall population counts.
#' @param sim Simulation which to plot
#' @export
make_num_plot <- function(sim) {
  nums <- sim$epi[[1]]$num
  nums <- nums[!is.na(nums)]
  start_year <- sim$control$year.start
  year_vals <- start_year - sim$control$start / 52 +
    seq_along(nums) * (7 / 365)
  fin_df <- data.frame("Network_Size" = nums,
                       "Year" = year_vals)
  start_grwth <- max(sim$param$demog$match.arrival$entry_year)
  maxyear <- sim$control$year.start +
    (sim$control$nsteps - sim$control$start) / 52
  netsize_st <- fin_df$Network_Size[which.min((fin_df$Year - start_grwth) ** 2)]
  grwt <- sim$param$pop_growth_post_targ %>%
    mutate(Year = year) %>%
    filter((Year) >= (start_grwth) & Year <= (maxyear - 1)) %>%
    mutate(Network_Size = s_perc * netsize_st, Year = Year + 1) %>%
    select(Network_Size, Year)
  fin_df <- bind_rows(
    bind_cols(fin_df, "Source" = "Simulation"),
    bind_cols(grwt, "Source" = "Target"),
  )

  fin_df %>% ggplot(aes(x = Year, y = Network_Size, color = Source)) +
    geom_line(size = 1.4) +
    geom_vline(xintercept = max(sim$param$demog$match.arrival$entry_year)) +
    ylab("Network size") +
    theme(legend.position = "bottom")
}
