#' @title Structure the Data as Table 1 Used in Publication.

#' @description Category date will be presented as N(percentage). continuous data will be presented in a customized form.

#' @param data  data table.
#' @param row_var The variable shown on the row, can't be NUll.
#' @param col_var The variable shown on the column, can be NULL.
#' @param col_perc When presented the category variable, the percentage should be calculated based on row or column. Default is column.
#' @param fun  The function used on continuous variable.
#' @param comb_sym  It determines how the continuous variable will be presented.
#' @param round_cate  Decimal of category variable. Default is 1.
#' @param round_cont  Decimal of continuous variable. Default is 2.
#' @param ... Arguments given to \code{"fun"}

#' @author Shanquan CHEN \email{shanquan0301@gmial.com}

#' @keywords data description
#' @examples
##' data(mtcars)
##' mtcars$judge <- TRUE
##' mtcars$judge[3:7] <- FALSE
##' mtcars$judge2 <- "TRUE"
##' mtcars$judge2[3:7] <- "FALSE"
##'
##' # continuous varialbe was shown in mean(sd)
##' res_tab <- data_des(data = mtcars,
##'          row_var = c("vs", "mpg", "wt", "gear", "judge", "judge2"),
##'          fun = c("mean", "sd"),
##'          comb_sym = c("(", ")"),
##'          round_cate = 2,
##'          round_cont = 3)
##'
##' res_tab
##'
##' # continuous varialbe was shown in median[25% quantile, 75% quantile]
##' res_tab <- data_des(data = mtcars,
##'          row_var = c("vs", "mpg", "wt", "gear", "judge", "judge2"),
##'          fun = c("median", "quantile"),
##'          comb_sym = c("[", "]"),
##'          probs = c(0.25, 0.75))
##'
##' res_tab
##'
##' # continuous varialbe was shown in mean[25% quantile, 75% quantile]
##' res_tab <- data_des(data = mtcars,
##'          row_var = c("vs", "mpg", "wt", "gear", "judge", "judge2"),
##'          col_var = "am",
##'          fun = c("mean", "quantile"),
##'          comb_sym = c("[", "]"),
##'          probs = c(0.25, 0.75),
##'          round_cate = 2,
##'          round_cont = 3)
##'
##' res_tab
##'

#' @import dplyr stringr
#' @importFrom stats sd quantile
#' @importFrom magrittr %$%
#' @importFrom tidyr spread

#' @export data_des
#function for all-------
data_des <- function(data,
                     row_var = c("vs", "mpg"),
                     col_var = NULL,
                     col_perc = TRUE,
                     fun = c("mean", "sd"),
                     comb_sym = c(" (", ")"),
                     round_cate = 1,
                     round_cont = 2,
                     ...){
  class(data) <- class(data)[which(class(data) != "rowwise_df")]
  if (is.null(col_var)){
    res_f <- data.frame(Variable = character(0), comb = character(0))
  } else {
    res_f <- data.frame(Variable = character(0), col_var = character(0), comb = character(0))
  }

  for (var in row_var){
    print(var)
    var <- as.name(var)
    m_len <- data %$% length(unique(eval(var)))
    m_class <- data %$% class(eval(var))
    if(m_len == 2 | m_class %in% c("character", "logical", "factor")){
      res <- cate_des(data = data,
                      row_var = as.character(var),
                      col_var = col_var,
                      col_perc = col_perc,
                      round_cate = round_cate)
    } else {
      res <- cont_des(data = data,
                      row_var = as.character(var),
                      col_var = col_var,
                      fun = fun,
                      comb_sym = comb_sym,
                      round_cont = round_cont,
                      ...)
    }
    if(is.null(col_var)){
      res <- res %>% select(Variable, comb)
    } else {
      res <- res %>% select(Variable, as.character(col_var), comb)
      names(res)[2] <- "col_var"
    }

    res_f <- rbind(res_f, res)
  }

  if(is.null(col_var)){
    return(res_f)
  } else {
    u_Variable <- unique(res_f$Variable)
    res_f <- res_f %>% spread(key = col_var, value = comb)
    #res_f <- res_f %>% filter (!is.na(Variable), !is.na(age_g)) %>% spread(key = col_var, value = comb)
    res_f <- res_f[match(u_Variable, res_f$Variable), ]
    names(res_f)[which(names(res_f) == "<NA>")] <- "All"
    names(res_f)[-1] <- str_c(as.character(col_var), " (= ", names(res_f)[-1], ")")
    return(res_f)
  }


}


#function for category variable------
cate_des <- function(data,
                     row_var = "vs",
                     col_var = NULL,
                     col_perc = TRUE,
                     round_cate = 1){
  class(data) <- class(data)[which(class(data) != "rowwise_df")]
  row_var <- as.name(row_var)
  #if col_var == NULL, it means no column
  if(is.null(col_var)){
    res_2 <- data %>%
      filter(!is.na(!!row_var)) %>%
      group_by(!!row_var) %>%
      summarise(n = n()) %>%
      mutate(N = sum(n),
             `Percentage(%)` = sprintf(str_glue("%.{round_cate}f"), round(n/N * 100, round_cate)))
    res <- res_2

  } else {
    col_var <- as.name(col_var)
    if (col_perc){
      res_1 <- data %>%
        filter(!is.na(!!row_var), !is.na(!!col_var)) %>%
        group_by(!!row_var, !!col_var) %>%
        summarise(n = n()) %>%
        group_by(!!col_var) %>%
        mutate(N = sum(n),
               `Percentage(%)` = sprintf(str_glue("%.{round_cate}f"), round(n/N * 100, round_cate)))
    } else {
      res_1 <- data %>%
        filter(!is.na(!!row_var), !is.na(!!col_var)) %>%
        group_by(!!row_var, !!col_var) %>%
        summarise(n = n()) %>%
        mutate(N = sum(n),
               `Percentage(%)` = sprintf(str_glue("%.{round_cate}f"), round(n/N * 100, round_cate)))
    }

    res_2 <- data %>%
      filter(!is.na(!!row_var)) %>%
      group_by(!!row_var) %>%
      summarise(n = n()) %>%
      mutate(N = sum(n),
             `Percentage(%)` = sprintf(str_glue("%.{round_cate}f"), round(n/N * 100, round_cate)))

    res <- bind_rows(res_1, res_2)
  }
  #combine the row
  res <- res %>% rowwise() %>%
    mutate(comb = str_c(n, " (", `Percentage(%)`, ")"),
           Variable = str_c(as.character(row_var), " (= ", !!row_var, ")")
    )
  names(res)[1] <- "cate"
  return(res)
}

#function for continuous variable-------
cont_des <- function(data,
                     row_var = "mpg",
                     col_var = NULL,
                     fun = c("mean", "sd"),
                     comb_sym = c(" (", ")"),
                     round_cont = 2,
                     ...){
  class(data) <- class(data)[which(class(data) != "rowwise_df")]
  row_var <- as.name(row_var)
  if(is.null(col_var)){
    res_2 <- data %>%
      summarise(mean = do.call(fun[1], args = list(!!row_var, na.rm = TRUE)),
                sd = str_c(sprintf(str_glue("%.{round_cont}f"),
                                   round(do.call(fun[2],
                                                 args = list(!!row_var,
                                                             na.rm = TRUE,
                                                             ...)),
                                         round_cont)),
                           collapse = ", "))

    res <- res_2
  } else {
    col_var <- as.name(col_var)
    res_1 <- data %>%
      filter(!is.na(!!col_var)) %>%
      group_by(!!col_var) %>%
      summarise(mean = do.call(fun[1], args = list(!!row_var, na.rm = TRUE)),
                sd = str_c(sprintf(str_glue("%.{round_cont}f"),
                                   round(do.call(fun[2],
                                                 args = list(!!row_var,
                                                             na.rm = TRUE,
                                                             ...)),
                                         round_cont)),
                           collapse = ", "))
    res_2 <- data %>%
      summarise(mean = do.call(fun[1], args = list(!!row_var, na.rm = TRUE)),
                sd = str_c(sprintf(str_glue("%.{round_cont}f"),
                                   round(do.call(fun[2],
                                                 args = list(!!row_var,
                                                             na.rm = TRUE,
                                                             ...)),
                                         round_cont)),
                           collapse = ", "))

    res <- bind_rows(res_1, res_2)
  }

  if (class(res$sd) == "numeric"){
    res <- res %>% rowwise() %>%
      mutate(Variable = as.character(row_var),
             comb = str_c(sprintf(str_glue("%.{round_cont}f"), round(mean, round_cont)),
                          comb_sym[1],
                          sprintf(str_glue("%.{round_cont}f"), round(sd, round_cont)),
                          comb_sym[2]))
  } else {
    res <- res %>% rowwise() %>%
      mutate(Variable = as.character(row_var),
             comb = str_c(sprintf(str_glue("%.{round_cont}f"), round(mean, round_cont)),
                          comb_sym[1],
                          sd,
                          comb_sym[2]))
  }
  n_loc <- which(names(res) == "mean")
  names(res)[n_loc:(n_loc + 1)] <- fun
  return(res)

}

utils::globalVariables(c("N", "Percentage(%)", "Variable", "comb"))
