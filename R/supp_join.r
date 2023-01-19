#' @title Supplementarily join the two data sets

#' @description if dat_1 and dat-2 have the same variable, like gender. If gender in dat_1 is NA, then extract the gender value from dat_2

#' @param dat_1 primary data table 1
#' @param dat_2 supplement data table 2
#' @param full TREU keep all the cases, full_join, else left_join
#' @param by unique id in dat_1 and dat_2

#' @author Shanquan CHEN \email{shanquan0301@gmial.com}

#' @keywords data description
#' @examples
##' dat_all <- supp_join(dat_fs, dat_hl,full = FALSE, by = "unique_hh_ln")  #merge fs and hl

#' @import dplyr stringr

#' @export supp_join


supp_join <- function(dat_1, dat_2, full = TRUE, by = "unique_hh_ln"){
  if(full){mdat <- full_join(dat_1, dat_2, by = by, suffix = c("", ".supp"))}
  if(!full){mdat <- left_join(dat_1, dat_2, by = by, suffix = c("", ".supp"))}
  same_var <- intersect(names(dat_1), names(dat_2))
  for(i in same_var) {
    eval(parse(text = str_glue("mdat <- mdat %>% mutate({i} = ifelse(is.na({i}), {i}.supp, {i}))")))
  }
  mdat <- mdat %>% select(-ends_with("supp"))
  return(mdat)
}
