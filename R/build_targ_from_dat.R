#' Build Targets from dat objec
#'
#' @param dat the netsim object from an EpiModel Simulation
#'
#' @return
#' A dataframe with the targets contained in the dat object matching the
#' structure of the WHAMP.targ dataframe.
#'
#' @export
#'

build_targ_from_dat <- function(dat){
  plt_targ <- data.frame(sub_cat = "ovr",
                         low = dat$param$hiv.test.late.prob,
                         high = dat$param$hiv.test.late.prob,
                         measure = "prop.late.tester", cat = "ovr")
  ltr_targ <- data.frame(sub_cat = "ovr",
                         low = dat$param$hiv.test.late.rate,
                         high = dat$param$hiv.test.late.rate,
                         measure = "late.test.rate", cat = "ovr")
  all_targs <- dplyr::bind_rows(plt_targ, ltr_targ)
  return(all_targs)
}

.extract.one.targ <- function(dat.name, targ.name) {
  dat.val <- eval(parse(text = paste0("dat$", dat.name)))
  return(data.frame(
    sub_cat = "ovr", low = dat.val,
    high = dat.val, measure = targ.name, cat = "ovr")
    )
}
