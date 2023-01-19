#' @title Detect the existence of multiple strings

#' @description Detect the existence of multiple strings

#' @param x provide the target variable
#' @param str provide the strings need to be detected

#' @author Shanquan CHEN \email{shanquan0301@gmial.com}

#' @keywords data description
#' @examples
##' x <- c("counts", "outcome", "treatment")
##' str_fun(x, str = c("cou", "ment"))
#'
#' @import stringr
#' @export str_fun

str_fun <- function(x, str = str){
  str <- str_c(str, collapse = "|")
  res <- str_detect(x, regex(str, ignore_case = TRUE))
  return(res)
}
