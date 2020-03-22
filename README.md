# DataDescription
Structure the Data as Table 1 Used in Publication

# install the package
```{r}
devtools::install_github("shanquan0301/DataDescription")
```

# Example
```{r}
data(mtcars)
mtcars$judge <- TRUE
mtcars$judge[3:7] <- FALSE
mtcars$judge2 <- "TRUE"
mtcars$judge2[3:7] <- "FALSE"
```
## continuous varialbe was shown in mean(sd)
```{r}
res_tab <- data_des(data = mtcars,
         row_var = c("vs", "mpg", "wt", "gear", "judge", "judge2"),
         fun = c("mean", "sd"),
         comb_sym = c("(", ")"),
         round_cate = 2,
         round_cont = 3)

res_tab
```

## continuous varialbe was shown in median[25% quantile, 75% quantile]
```{r}
res_tab <- data_des(data = mtcars,
         row_var = c("vs", "mpg", "wt", "gear", "judge", "judge2"),
         fun = c("median", "quantile"),
         comb_sym = c("[", "]"),
         probs = c(0.25, 0.75))

res_tab
```

## continuous varialbe was shown in mean[25% quantile, 75% quantile]
```{r}
res_tab <- data_des(data = mtcars,
         row_var = c("vs", "mpg", "wt", "gear", "judge", "judge2"),
         col_var = "am",
         fun = c("mean", "quantile"),
         comb_sym = c("[", "]"),
         probs = c(0.25, 0.75),
         round_cate = 2,
         round_cont = 3)

res_tab
```

