#' Create a new Epi diagnostic from already existing Epi diagnostics.
#'
#' In EpiModel simulations, current states of the network are often stored.
#' As an example, the number of individuals in the network and the number of
#' infected individuals are stored in the epi object of dat.  If you
#' wish to calculate the prevalence (number infected / number of individuals)
#' from these two values, this can be done using the make_new_epi function.
#'
#' **Why not just do this manually?** While this calculation can be done
#' manually, it can become cumbersom if you wish to calculate a new epi
#' measure among many different subgroups.  This function automatically
#' calculates the new epi measure for each subgroup for which all the required
#' epi measures (in the above example number infected and number of individuals)
#' are defined.
#'
#' @param dat The dat object that contains the epi information
#' @param args The names of the epi measure used to calculate the new epi measure.
#' @param FUN The function used to calculate the new epi measures.  The number
#' of arguments should be the same as the length of args list.
#' @param new_name Name of the new epi measure should end with ".".
#'
#' @return
#' a dat object with the addition of the epi measures with names given by
#' new_name (along with the attribute name).
#'
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' ## NOT RUN
#' dat <- list("epi" = list(
#'   "num.B" = c(1, 3, 4, 10, 20),
#'   "num.O" = c(20, 25, 50, 55, 60),
#'   "num.new.i.B" = c(0, 0, 1, 0, 1),
#'   "num.new.i.O" = c(2, 0, 2, 0, 2))
#'   )
#' dat <- make_new_epi(dat, c("num.new.i."), function(x){cumsum(x)}, "num.i.")
#' dat$epi$num.new.i.O
#' dat <- make_new_epi(dat, c("num.", "num.i."), function(x, y){y / x}, "prev.")
#' dat$epi$prev.O

make_new_epi <- function(epi, args, FUN, new_name) {
  if (any(grepl("num", names(epi)))) {
    epi <- epi
    episim <- FALSE
  }else{
    epi <- epi[[1]]
    episim <- TRUE
  }
  lens <- sapply(epi, length)
  mis_epi <- max(lens) - lens
  for (epi_idx in 1:length(epi)) {
    epi[[epi_idx]] <- c(epi[[epi_idx]], rep(NA, mis_epi[epi_idx]))
  }
  ep_names <- names(epi)
  match_args <- lapply(args, FUN = function(str) {
    all_match <- ep_names[grep(str, ep_names)]
    num_dots_arg <- length(strsplit(str, "\\.")[[1]]) + 1
    num_dots <- sapply(strsplit(all_match, "\\."), length)
    return(all_match[num_dots == num_dots_arg])
  })
  extra_vals <- lapply(args, FUN = function(str) {
    which_match <- which(
      sapply(match_args, FUN = function(x) {
        num.dots <- length(strsplit(str, "\\.")[[1]]) + 1
        num.dots.x <- sapply(strsplit(x, "\\."), length)
        all(grepl(str, x) & num.dots.x == num.dots)
      }
      )
    )
    return(gsub(str, "", match_args[[which_match]]))
  })
  if (length(args) > 1){
    match_vals <- Reduce(base::intersect, extra_vals)
  }else{
    match_vals <- extra_vals[[1]]
  }
  for (mtch_idx in 1:length(match_vals)) {
    sub_val <- match_vals[mtch_idx]
    sub_lst <- epi[paste0(args, sub_val)]
    names(sub_lst) <- NULL
    if (episim) {
      epi[[1]][[paste0(new_name, sub_val)]] <-
        do.call(FUN, sub_lst)
    }else{
      epi[[paste0(new_name, sub_val)]] <-
        do.call(FUN, sub_lst)
    }
  }
  return(epi)
}
