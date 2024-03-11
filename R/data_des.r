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
#' @param p_value If calculate the p value
#' @param test_name_cont test on continuous variable
#' @param test_name_cate test on category variable
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

#' @import dplyr stringr smd
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
                     p_value = TRUE,
                     weight = NULL,
                     test_name_cont = "t.test",
                     test_name_cate = "chisq.test",
                     ...){
  class(data) <- class(data)[which(class(data) != "rowwise_df")]
  if (is.null(col_var)){
    res_f <- data.frame(Variable = character(0), comb = character(0))
  } else {
    if(p_value == TRUE){res_f <- data.frame(Variable = character(0),
                                            col_var = character(0),
                                            comb = character(0),
                                            test_name = character(0),
                                            test_value = numeric(0),
                                            p_value = numeric(0))}
    if(p_value != TRUE){res_f <- data.frame(Variable = character(0),
                                            col_var = character(0),
                                            comb = character(0))}

  }

  for (var in row_var){
    print(var)
    var <- as.name(var)
    m_len <- data %$% length(unique(eval(var)))
    m_class <- data %$% class(eval(var))
    if(m_len == 2 | m_class[1] %in% c("character", "logical", "factor", "ordered")){
      res <- cate_des(data = data,
                      row_var = as.character(var),
                      col_var = col_var,
                      col_perc = col_perc,
                      p_value = p_value,
                      round_cate = round_cate,
                      weight = weight,
                      test_name = test_name_cate)
    } else {
      res <- cont_des(data = data,
                      row_var = as.character(var),
                      col_var = col_var,
                      fun = fun,
                      p_value = p_value,
                      weight = weight,
                      comb_sym = comb_sym,
                      round_cont = round_cont,
                      test_name = test_name_cont,
                      ...)
    }
    if(is.null(col_var)){
      res <- res %>% select(Variable, comb)
    } else {
      if(p_value == TRUE){res <- res %>% select(Variable, as.character(col_var), comb, test_name, test_value, p_value)}
      if(p_value != TRUE){res <- res %>% select(Variable, as.character(col_var), comb)}
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
    names(res_f)[str_detect(names(res_f), "test_value")] <- "test_value"
    names(res_f)[str_detect(names(res_f), "test_name")] <- "test_name"
    names(res_f)[str_detect(names(res_f), "p_value")] <- "p_value"
    if("p_value" %in% names(res_f)){
      res_f <- res_f %>% select(Variable, starts_with(as.character(col_var)), test_name, test_value, p_value)
    }
    return(res_f)
  }


}


#function for category variable------
cate_des <- function(data,
                     row_var = "gear",
                     col_var = "vs",
                     col_perc = TRUE,
                     round_cate = 1,
                     p_value = TRUE,
                     weight = NUll,
                     test_name = "chisq.test"){
  class(data) <- class(data)[which(class(data) != "rowwise_df")]
  row_var <- as.name(row_var)
  m_class <- data %$% class(eval(row_var))
  if("ordered" %in% m_class) {test_name <- "prop.trend.test"}

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
    mutate(comb = str_c(n, " (", `Percentage(%)`, "%)"),
           Variable = str_c(as.character(row_var), " (= ", !!row_var, ")")
    )

  #add p_value
  if(!is.null(col_var) & p_value == TRUE){
    m_tab <- eval(parse(text =str_glue("data %$% table(as.character({row_var}), as.character({col_var}))")))
    events <- m_tab[, 1]
    trials <- rowSums(m_tab)

    if(test_name %in% c("smd")){
      if (is.null(weight)) {
        mdat_test <- eval(parse(text = str_glue("data %$% smd(x = {row_var}, g = {col_var}, std.error = TRUE)")))
      }

      if (!is.null(weight)) {
        mdat_test <- eval(parse(text = str_glue("data %$% smd(x = {row_var}, g = {col_var}, w = {weight}, std.error = TRUE)")))
      }

    }

    if (test_name == "chisq.test"){
      mdat_test <- eval(parse(text = str_glue("{test_name}(m_tab)")))
    }

    if (test_name == "prop.trend.test"){
      mdat_test <- prop.trend.test(events, trials)
    }

    if(test_name == "smd") {
      res$test_name <- test_name
      #mdat_test <- unlist(summary(mdat_test))
      res$test_value <- mdat_test$estimate
      res$p_value <- mdat_test$std.error

    }


    if(test_name %in% c("chisq.test", "prop.trend.test")){
      res$test_name <- test_name
      res$test_value <- mdat_test$statistic
      res$p_value <- mdat_test$p.value
    }

  }

  names(res)[1] <- "cate"
  return(res)
}

#function for continuous variable-------
cont_des <- function(data,
                     row_var = "mpg",
                     col_var = "vs",
                     fun = c("mean", "sd"),
                     comb_sym = c(" (", ")"),
                     round_cont = 2,
                     p_value = TRUE,
                     weight = NULL,
                     test_name = "t.test",
                     ...){
  class(data) <- class(data)[which(!class(data) %in% c("rowwise_df", "grouped_df"))]
  row_var <- as.name(row_var)
  n_col <- eval(parse(text = str_glue("length(na.omit(unique(data${col_var})))")))
  if(n_col > 2) {test_name <- "aov"}


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


  #add p_value
  if(!is.null(col_var) & p_value == TRUE){

    if(test_name %in% c("t.test", "aov", "wilcox.test")){
      mdat_test <- eval(parse(text = str_glue("{test_name}({row_var} ~ {col_var}, data = data)")))
    }

    if(test_name %in% "smd"){
      if (is.null(weight)){
        mdat_test <- eval(parse(text = str_glue("data %$% smd(x = {row_var}, g = {col_var}, std.error = TRUE)")))
      }

      if (!is.null(weight)){
        mdat_test <- eval(parse(text = str_glue("data %$% smd(x = {row_var}, g = {col_var}, w = {weight}, std.error = TRUE)")))
      }
    }

    if(test_name == "aov") {
      res$test_name <- test_name
      mdat_test <- unlist(summary(mdat_test))
      res$test_value <- mdat_test["F value1"]
      res$p_value <- mdat_test["Pr(>F)1"]

    }

    if(test_name == "smd") {
      res$test_name <- test_name
      #mdat_test <- unlist(summary(mdat_test))
      res$test_value <- mdat_test$estimate
      res$p_value <- mdat_test$std.error

    }


    if(!(test_name %in% c("aov", "smd"))) {
      res$test_name <- test_name
      res$test_value <- mdat_test$statistic
      res$p_value <- mdat_test$p.value
    }

  }

  n_loc <- which(names(res) == "mean")
  names(res)[n_loc:(n_loc + 1)] <- fun
  return(res)

}

utils::globalVariables(c("N", "Percentage(%)", "Variable", "comb"))
