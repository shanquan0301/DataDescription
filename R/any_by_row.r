#' @title Process by row or group

#' @description if all NA then return NA, else run the aimed function

#' @author Shanquan CHEN \email{shanquan0301@gmial.com}

#' @keywords data description
#' @examples
##' # judge by row, need use mapply
##' dat_hl <- dat_hl %>% mutate(
##'   hearing = mapply(any_by_row, hearing_1, hearing_2),
##'   seeing = mapply(any_by_row, seeing_1, seeing_2),
##'   dis_comm_sup = mapply(mean_by_row, dis_comm_1, dis_comm_2)
##'   )
##'
##' # judge by group, can use directly
##' mdat <- dat_all %>% group_by(unique_hh) %>% summarise(
##'   disability = any_by_row(disability_0_4, disability_5_7_without_mental, disability_adult))
#'
#' @import dplyr tidyr

#' @export any_by_row
#' @export mean_by_row
#' @export sum_by_row
#' @export all_by_row


any_by_row <- function(...){
  dots <- unlist(list(...))
  na_num <- sum(is.na(dots))
  if(length(dots) == na_num){return(NA)}
  if(length(dots) != na_num){
    return(any(dots, na.rm = TRUE))
  }
}

all_by_row <- function(...){
  dots <- unlist(list(...))
  na_num <- sum(is.na(dots))
  if(length(dots) == na_num){return(NA)}
  if(length(dots) != na_num){
    return(all(dots, na.rm = TRUE))
  }
}

mean_by_row <- function(...){
  dots <- unlist(list(...))
  na_num <- sum(is.na(dots))
  if(length(dots) == na_num){return(NA)}
  if(length(dots) != na_num){
    return(mean(dots, na.rm = TRUE))
  }
}
sum_by_row <- function(...){
  dots <- unlist(list(...))
  na_num <- sum(is.na(dots))
  if(length(dots) == na_num){return(NA)}
  if(length(dots) != na_num){
    return(sum(dots, na.rm = TRUE))
  }
}





