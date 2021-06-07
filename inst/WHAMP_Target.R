## Create Target data for EpiModel WHAMP DX
### Read in attr names:
source("./sup_dat.R")
suppressPackageStartupMessages(library(tidyverse))

# ART Outcomes
## Start ART within 30 days
### Overall
art_within30 <- data.frame(
  subcat = "ovr", targ = 0.8, measure = "art.at.30", cat = "ovr"
)

## Proportion engaged in care
### Overall
on_art <- data.frame(
  subcat = "ovr", targ = 0.895, measure = "prop.art", cat = "ovr"
)
## Proportion Suppressed
### Overall
prop_suprs <- data.frame(
  subcat = "ovr", targ = 0.825, measure = "prop.supr", cat = "ovr"
)

## Prevalence
### Race
popdat <- WApopdata::msm.pop.totals_2019
race_df <- popdat$pop.race.pos %>%
  select(subcat = race, low = prev_lb, high = prev_ub) %>%
  mutate(measure = "prev", cat = "race")
### Age Group
agrp_df <- popdat$pop.age.pos %>%
  mutate(tot_num = num_lb / prev_lb, subcat = substr(age.grp10, 1, 1),
         subcat2 = pmin(as.numeric(subcat), 6)) %>% group_by(subcat2) %>%
  mutate(prev_lb = sum(num_lb)/sum(tot_num),
         prev_ub = sum(num_ub)/sum(tot_num)) %>%
  ungroup() %>% filter(subcat < 7) %>%
  select(subcat, low = prev_lb, high = prev_ub) %>%
  mutate(measure = "prev", cat = "age.grp")

### Region
region_df <- popdat$pop.region.pos %>%
  select(subcat = region, low = prev_lb, high = prev_ub) %>%
  mutate(measure = "prev", cat = "region")

### Combine
df_names <- setdiff(ls(), "attr_names")
df_lst <- list()
for (df_name in df_names) {
  sub_df <- eval(parse(text = df_name))
  if ("targ" %in% colnames(sub_df)) {
    sub_df <- sub_df %>%
      mutate(low = targ, high = targ) %>%
      select(-targ)
  }
  if (length(setdiff(
    c("subcat", "low", "high", "measure", "cat"),
              colnames(sub_df))) == 0) {
    df_lst[[df_name]] <- sub_df
  }
}

WHAMP.targs <- do.call(dplyr::bind_rows, df_lst)

### Rename attr values to be more readable
all_names <- sapply(purrr::flatten(EpiModelWHAMPDX::attr_names), "[[", 1)
WHAMP.targs$sub_cat <- recode(WHAMP.targs$subcat, !!!all_names)

usethis::use_data(WHAMP.targs, overwrite = "TRUE")



