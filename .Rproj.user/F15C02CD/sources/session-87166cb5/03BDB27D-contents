#' @title Structure the result of regression

#' @description Structure the result of regression

#' @param reg  the regression result, like the result of lm or glm
#' @param data provide the data to process, only used when reg is missing
#' @param round_p digital for p value
#' @param round_ci digital for coefficient and ci
#' @param exp_transfer if show the exp form.
#' @param coef column name of coefficient
#' @param p_value  column name of p value
#' @param ci_low  column name of ci low
#' @param ci_high  column name of ci high
#' @param comb_ci  form the result to be shown

#' @author Shanquan CHEN \email{shanquan0301@gmial.com}

#' @keywords data description
#' @examples
#' counts <- c(18,17,15,20,10,20,25,13,12)
#' outcome <- gl(3,1,9)
#' treatment <- gl(3,3)
#' glm.D93 <- glm(counts ~ outcome + treatment, family = poisson())
#' reg_comb(reg = glm.D93, exp_transfer = TRUE)
#'
#' @import dplyr stringr tidyr
#' @importFrom stats glm confint
#' @export reg_comb


reg_comb <- function (reg = reg,
                      data = data,
                      round_p = 4,
                      round_ci = 4,
                      exp_transfer = FALSE,
                      coef = "Estimate",
                      p_value = "`Pr(>|z|)`",
                      ci_low = "`2.5 %`",
                      ci_high = "`97.5 %`",
                      comb_ci = "coef(ci_low-ci_high)star, p_value"){
  if (!is.null(reg)){
    mdat_coef <- summary(reg)$coefficients %>% as.data.frame()
    mdat_coef$variable <- rownames(mdat_coef)
    mdat_ci <- confint(reg) %>% as.data.frame()
    mdat_ci$variable <- rownames(mdat_ci)
    mdat <- full_join(mdat_coef, mdat_ci, by = "variable")
  } else {
    mdat <- data
  }

  mdat_express <- str_glue("mdat %>% mutate(
                              coef = {coef},
                              p_value = {p_value},
                              ci_low = {ci_low},
                              ci_high = {ci_high})")

  mdat <- eval(parse(text = mdat_express))

  if (exp_transfer) {
    mdat <- mdat %>% mutate(
      coef = sprintf(str_glue("%.{round_ci}f"), round(exp(coef), round_ci)),
      ci_low = sprintf(str_glue("%.{round_ci}f"), round(exp(ci_low), round_ci)),
      ci_high = sprintf(str_glue("%.{round_ci}f"), round(exp(ci_high), round_ci)),
      p_value = round(p_value, round_p))
  } else {
    mdat <- mdat %>% mutate(
      coef = sprintf(str_glue("%.{round_ci}f"), round(coef, round_ci)),
      ci_low = sprintf(str_glue("%.{round_ci}f"), round(ci_low, round_ci)),
      ci_high = sprintf(str_glue("%.{round_ci}f"), round(ci_high, round_ci)),
      p_value = round(p_value, round_p))
  }

  mdat <- mdat %>% mutate(
    star = case_when(
      p_value <= 0.1 &  p_value > 0.05 ~ ".",
      p_value <= 0.05 & p_value > 0.01 ~ "*",
      p_value <= 0.01 & p_value > 0.001 ~ "**",
      p_value <= 0.001 ~ "***", TRUE ~ ""),
    p_value = case_when(
      p_value == 0 ~ "<0.0001",
      p_value == 1 ~ "1.0000",
      TRUE ~ sprintf(str_glue("%.{round_p}f"), round(p_value, round_p))))

  # mdat <- mdat %>% mutate(
  #   coef = str_trim(as.character(format(coef, scientific = FALSE))),
  #   ci_low = str_trim(as.character(format(ci_low, scientific = FALSE))),
  #   ci_high = str_trim(as.character(format(ci_high, scientific = FALSE))))

  for (i in c("coef", "ci_low", "ci_high", "p_value", "star")) {
    if (str_detect(comb_ci, i)) {
      comb_ci <- str_replace(comb_ci, i, str_c("{",i, "}"))
    }
  }
  mdat_express <- str_glue("mdat %>% rowwise() %>%
                               mutate(comb_ci = str_glue('{comb_ci}'))")
  mdat <- eval(parse(text = mdat_express))
  return(mdat)
}
