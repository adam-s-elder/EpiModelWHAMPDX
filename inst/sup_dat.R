attr_names <- list(
  "ovr" = c(ovr = "Overall"),
  "race" = c(H = "Hispanic", B = "Black", O = "Other"),
  "region" = c(EasternWA = "Eastern Washington",
               WesternWA = "Western Washington",
               King = "King County"),
  "age.grp" = c("1" = "15-24", "2" = "25-34", "3" = "35-44",
                "4" = "45-54", "5" = "55-65", "6" = "66+"),
  "snap5" = c("sn0" = "SNAP 0", "sn1" = "SNAP 1", "sn2" = "SNAP 2",
              "sn3" = "SNAP 3", "sn4" = "SNAP 4", "sn5" = "SNAP 5")
)

usethis::use_data(attr_names, overwrite = TRUE)
