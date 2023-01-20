#' @title Forest the results from regression

#' @description Forest the results from regression

#' @param reg  the regression result, like the result of lm or glm
#' @param reg_res result form reg_comb
#' @param log if log x axis for plot 3
#' @param var_label list. full name of the variables
#' @param value_label list. labels of values of variables. Primary use for logical. Could also use for character and factor
#' @param comb_title title of plot 2 or combined result.
#' @param layout_matrix matrix of the final combined plot
#' @param shadow shadow of lines. still under test.
#' @param plot_out output plot or list of plot_1, plot_2, plot_3, plot_4
#'
#' @author Shanquan CHEN \email{shanquan0301@gmial.com}

#' @keywords data description
#' @examples
#' #cox regression-----------
#' reg <- coxph(Surv(follow_up, detention) ~ group + cluster(rid) +
#'                age + gender + mari_stat + ethnic + imd_cat + mmse + charlson + smi,
#'                data = dat_mha %>% filter(group != "PDD"))

#' reg_res <- reg_comb(reg = reg,
#'                     exp_transfer = TRUE,
#'                     coef = "coef",
#'                     round_ci = 2,
#'                     round_p = 4,
#'                     comb_ci = "coef(ci_low-ci_high)")

#' plot_forest(reg = reg,
#'             reg_res = reg_res,
#'             var_label = list(group = "Dementia",
#'                              age = "Age at baseline (years)",
#'                              gender = "Gender",
#'                              mari_stat = "Marital status",
#'                              ethnic = "Ethnicity",
#'                              imd_cat = "Social-economic status",
#'                              mmse = "Dementia severity",
#'                              charlson = "Physical comordity",
#'                              smi = "Severe mental illness (= yes)"),
#'             value_label = list(mari_stat = c("Single/divorced/widowed", "Cohabiting/married"),
#'                                imd_cat = c("1(Most deprived)", "2", "3", "4", "5(Most affluent)"),
#'                                smi = c(TRUE, FALSE)))


#' #cox regression with interaction-----------------------
#' reg <- coxph(Surv(follow_up, death) ~ detention*group +  cluster(id) +
#'               age + gender + mari_stat + ethnic + imd_cat  + mmse + charlson + smi,
#'               data = dat_reg)

#' reg_res <- reg_comb(reg = reg,
#'                     exp_transfer = TRUE,
#'                     coef = "coef",
#'                     round_ci = 2,
#'                     round_p = 4,
#'                     comb_ci = "coef(ci_low-ci_high)")

#' plot_forest(reg = reg,
#'             reg_res = reg_res,
#'             var_label = list(detention = "Compulsory admission (= yes)",
#'                              group = "Dementia",
#'                              `detention:group` = "Compulsory admission (= yes)xDLB",
#'                              age = "Age at baseline (years)",
#'                              gender = "Gender",
#'                              mari_stat = "Marital status",
#'                              ethnic = "Ethnicity",
#'                              imd_cat = "Social-economic status",
#'                              mmse = "Dementia severity",
#'                              charlson = "Physical comordity",
#'                              smi = "Severe mental illness (= yes)"),
#'             value_label = list(mari_stat = c("Single/divorced/widowed", "Cohabiting/married"),
#'                                detention = c(TRUE, FALSE),
#'                                `detention:group` = c(TRUE, FALSE),
#'                                imd_cat = c("1(Most deprived)", "2", "3", "4", "5(Most affluent)"),
#'                                smi = c(TRUE, FALSE)))

#logistic regression ------------
#' reg <- glm(detention ~ group +
#'            age + gender*smi + mari_stat + ethnic + imd_cat + mmse + charlson + smi,
#'            family = binomial(),
#'            data = dat_mha)

#' reg_res <- reg_comb(reg = reg,
#'                     exp_transfer = TRUE,
#'                     coef = "Estimate",
#'                     round_ci = 2,
#'                     round_p = 4,
#'                     comb_ci = "coef(ci_low-ci_high)")

#' plot_forest(reg = reg,
#'             reg_res = reg_res,
#'             var_label = list(group = "Dementia",
#'                              age = "Age at baseline (years)",
#'                              gender = "Gender",
#'                              `gender:smi` = "GenderxSMI",
#'                              mari_stat = "Marital status",
#'                              ethnic = "Ethnicity",
#'                              imd_cat = "Social-economic status",
#'                              mmse = "Dementia severity",
#'                              charlson = "Physical comordity",
#'                              smi = "Severe mental illness (= yes)"),
#'             value_label = list(mari_stat = c("Single/divorced/widowed", "Cohabiting/married"),
#'                                `gender:smi` = c(TRUE, FALSE),
#'                                imd_cat = c("1(Most deprived)", "2", "3", "4", "5(Most affluent)"),
#'                                smi = c(TRUE, FALSE)),
#'             shadow = pal_nejm("default", alpha = 0.6)(8)[1:2])




#'
#' @import dplyr stringr tidyr
#' @importFrom gridExtra grid.arrange
#' @export plot_forest



plot_forest <- function(reg,
                        reg_res,
                        log = TRUE,
                        var_label = list(group = "Dementia",
                                         age = "Age at baseline (years)",
                                         gender = "Gender",
                                         smi = "Severe mental illness (= yes)",
                                         `gender:smi` = "Gender(= male)xSMI",
                                         mari_stat = "Marital status",
                                         imd_cat = "Social-economic status",
                                         ethnic = "Ethnicity",
                                         mmse = "Dementia severity",
                                         charlson = "Physical comordity"
                        ),
                        value_label = list(mari_stat = c("Single/divorced/widowed", "Cohabiting/married"),
                                           detention = c(TRUE, FALSE),
                                           `gender:smi` = c(TRUE, FALSE),
                                           imd_cat = c("1(Most deprived)", "2", "3", "4", "5(Most affluent)"),
                                           smi = c(TRUE, FALSE)),
                        comb_title = "Hazard Ratio (95%CI)",
                        layout_matrix = matrix(c(1, 1, 2,2, 3, 3,3, 4), nrow = 1),
                        shadow = c("white", "white"),
                        plot_out = FALSE,
                        num_space = 1,
                        ...){
  #get the variables---------
  var <- attr(reg$terms, "dataClasses")[names(attr(reg$terms, "dataClasses")) %in% attr(reg$terms, "term.labels")]
  #get the value labels---------
  value_label <- c(value_label, reg$xlevels[setdiff(names(reg$xlevels), names(value_label))])

  dat_plot_1 <- data.frame(var = names(var_label),
                           label = unlist(var_label))
  dat_plot_2 <- data.frame(var = names(var),
                           type = var)
  dat_plot <- full_join(dat_plot_1, dat_plot_2, by = "var")
  row.names(dat_plot) <- dat_plot$var
  #add the frequency of repeated---------
  dat_plot$frequency <- NA
  for(i in 1:nrow(dat_plot)){
    if(!dat_plot$type[i] %in% c("character", "factor", NA)){
      dat_plot$frequency[i] <- 1
    }
  }

  dat_plot_3 <- data.frame(var = names(reg$xlevels),
                           frequency = unlist(lapply(reg$xlevels, length)) -1)

  dat_plot <- supp_join(dat_plot, dat_plot_3, by = "var")

  dat_plot$frequency[dat_plot$var == "gender"] * dat_plot$frequency[dat_plot$var == "smi"]

  num_na <- which(is.na(dat_plot$frequency))
  str_var <- unlist(str_split(dat_plot$var[num_na], ":"))
  dat_plot$frequency[num_na] <-
    eval(parse(text = str_c(str_glue("dat_plot$frequency[dat_plot$var == '{str_var}']"), collapse = "*")))

  row.names(dat_plot) <- dat_plot$var








  #exclue the value_label may not exist----
  str_nam <- names(value_label)[names(value_label) %in% dat_plot$var]
  value_label <- value_label[str_nam]
  #transfer the regression into data table----------------------
  res <- reg_res %>% select(variable, comb_ci, coef, ci_low, ci_high, p_value) %>%
    filter(!str_fun(variable, "intercept"))
  res$variable <- rep(attr(reg$terms, "term.labels"),
                      times = dat_plot[attr(reg$terms, "term.labels"), ]$frequency)
  res$fontface <- NA


  #put the sub groups of variables, their labels, regression into the data table------
  for(i in 1:nrow(dat_plot)){
    if(dat_plot$type[i] %in% c("character", "factor", "logical", NA)){
      if (value_label[[dat_plot$var[i]]][1] != TRUE){
        m_mdat_1 <- matrix(c(dat_plot$label[i], NA, NA, NA, NA, NA, "bold"), nrow = 1)
        m_mdat_2 <-  matrix(c(str_c(str_c(rep(" ", time = num_space), collapse = ""), value_label[[dat_plot$var[i]]][1], collapse = ""),
                              "Reference", NA, NA, NA, "Reference", NA), nrow = 1)
        m_mdat_3 <- res %>% filter(variable == dat_plot$var[i])
        m_mdat_3$variable <- str_c(str_c(rep(" ", time = num_space), collapse = ""), value_label[[dat_plot$var[i]]][-1], sep = "")
        m_mdat_3 <- m_mdat_3 %>% as.matrix()
        m_mdat <- rbind(m_mdat_1, m_mdat_2, m_mdat_3)
      }

      if (value_label[[dat_plot$var[i]]][1] == TRUE){
        m_mdat <- res %>% filter(variable == dat_plot$var[i])
        m_mdat$variable <- dat_plot$label[i]
        m_mdat$fontface <- "bold"
        m_mdat <- m_mdat %>% as.matrix()
      }
    }

    if(!dat_plot$type[i] %in% c("character", "factor", "logical")){
      m_mdat <- res %>% filter(variable == dat_plot$var[i])
      m_mdat$variable <- dat_plot$label[i]
      m_mdat$fontface <- "bold"
      m_mdat <- m_mdat %>% as.matrix()
    }

    if(i == 1){mdat_f <- m_mdat}
    if(i != 1)(mdat_f <- rbind(mdat_f, m_mdat))
  }

  dat_plot <- as_tibble(mdat_f)

  dat_plot <- dat_plot %>% mutate(
    coef = as.numeric(coef),
    ci_low = as.numeric(ci_low),
    ci_high = as.numeric(ci_high),
  )

  # mdat <- mdat %>% rowwise() %>% mutate(
  #   comb_ci = str_glue("{sprintf('%.2f', round(coef, 2))}({sprintf('%.2f', round(ci_low, 2))}, {sprintf('%.2f', round(ci_high, 2))})"),
  #   comb_ci = ifelse(p_value == "Reference", "Reference", comb_ci)
  # )

  dat_plot <- dat_plot %>% rowwise() %>% mutate(
    #p_value = if_else(coef == 0 & !is.na(coef), "<0.01(<0.01, <0.01)", p_value),
    #comb_ci = if_else(coef == 0 & !is.na(coef), "<0.01(<0.01, <0.01)", comb_ci)
    p_value = if_else(coef == 0 & !is.na(coef), "-", p_value),
    comb_ci = if_else(coef == 0 & !is.na(coef), "-", comb_ci)
  )

  dat_plot[which(dat_plot$coef == 0), c("coef", "ci_low", "ci_high")] <- NA

  dat_plot$order <-  nrow(dat_plot):1
  dat_plot <- dat_plot %>% mutate(
    fontface = ifelse(is.na(fontface), "plain", fontface)
  )
  dat_plot$shadow <- rep(shadow, 100)[1:nrow(dat_plot)]
  #plot-----
  theme_com <- function(...){
    theme_classic() +
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.line = element_blank(),
            axis.title.x = element_blank(),
            plot.title = element_text(hjust = 0.5, face = "bold"))
  }

  theme_uni <- function(...){
    theme(axis.ticks.x = element_line(colour = "white"),
          axis.text.x = element_text(colour = "white"))
  }


  plot_1 <-  ggplot(data = dat_plot, aes(x = -0.5, y = order))  +
    geom_rect(aes(xmin = -Inf, xmax = Inf,
                  ymin =  order - 0.5, ymax = order + 0.5), fill = dat_plot$shadow) +
    geom_text(aes(label = variable), hjust = 0,
              fontface = dat_plot$fontface) +
    scale_x_continuous(name = "", limits = c(-0.5, 0.5), breaks = -0.5) +
    scale_y_discrete(name = "")  +
    labs(title = "Varialbe") +
    theme_com() +
    theme_uni()


  plot_2 <-  ggplot(data = dat_plot, aes(x = 0, y = order))  +
    geom_rect(aes(xmin = -Inf, xmax = Inf,
                  ymin =  order - 0.5, ymax = order + 0.5), fill = dat_plot$shadow) +
    geom_text(aes(label = comb_ci)) +
    scale_x_continuous(name = "", breaks = 0) +
    scale_y_discrete(name = "")  +
    labs(title = comb_title) +
    theme_com() +
    theme_uni()

  plot_3 <- ggplot(data = dat_plot, aes(x  = coef, y = order))  +
    geom_rect(aes(xmin = -Inf, xmax = Inf,
                  ymin =  order - 0.5, ymax = order + 0.5), fill = dat_plot$shadow) +
    geom_point() +
    geom_errorbarh(aes(xmin = ci_low, xmax = ci_high), height = .2) +
    geom_vline(xintercept = 1, linetype = 2, colour = "grey75") +
    scale_x_log10() +
    #scale_x_continuous(limits = c(0, max(mdat$ci_high, na.rm = TRUE))) +
    scale_y_discrete(name = "")  +
    labs(title = " ") +
    theme_com() +
    theme(axis.line.x = element_line(colour = "black"))

  if(log == FALSE){
    plot_3 <- ggplot(data = dat_plot, aes(x  = coef, y = order))  +
      geom_rect(aes(xmin = -Inf, xmax = Inf,
                    ymin =  order - 0.5, ymax = order + 0.5), fill = dat_plot$shadow) +
      geom_point() +
      geom_errorbarh(aes(xmin = ci_low, xmax = ci_high), height = .2) +
      geom_vline(xintercept = 0, linetype = 2, colour = "grey75") +
      scale_x_continuous(limits = c(0, max(dat_plot$ci_high, na.rm = TRUE))) +
      scale_y_discrete(name = "")  +
      labs(title = " ") +
      theme_com() +
      theme(axis.line.x = element_line(colour = "black"))
  }

  plot_4 <-  ggplot(data = dat_plot, aes(x  = 0, y = order))  +
    geom_rect(aes(xmin = -Inf, xmax = Inf,
                  ymin =  order - 0.5, ymax = order + 0.5), fill = dat_plot$shadow) +
    geom_text(aes(label = p_value))  +
    scale_x_continuous(name = "", breaks = 0) +
    scale_y_discrete(name = "") +
    labs(title = "P value") +
    theme_com() +
    theme_uni()


  if(plot_out == TRUE){
    grid.arrange(plot_1, plot_2, plot_3, plot_4, layout_matrix = layout_matrix)
  } else{
    plot_out <- list(plot_1, plot_2, plot_3, plot_4)
    return(plot_out)
  }


}


