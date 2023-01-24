#' @title Import the data from MICS

#' @description Only import data from MICS. Not for genderal use

#' @param country which country's data to be imported
#' @param dataset which data set need to be imported. Options include "hl", "ch", "fs", "wm", "mn", "hh", and others
#' @param var_id  which variables use to generate unique id. hh is c("HH1", "HH2"), others more likely is c("HH1", "HH2", "HL1")
#' @param var_interested_identify identify names of variables. For example, for variable "gender", you need input "gender"
#' @param var_interested_match match the variables by start_with. for example, for series of variables "FCF1", "FCF2", "FCF3", FCF4", you can easily input "FCF". Then the function will extract all the variables start with "FCF"
#' @param var_sampling_weight sampling weight

#' @author Shanquan CHEN \email{shanquan0301@gmial.com}

#' @keywords data description
#' @examples
#' # import from hl
#' dat_hl <- lapply(country_name, FUN = fun_data_import,
#'                  dataset = "hl",
#'                  var_id = c("HH1", "HH2", "HL1"),
#'                  var_interested_identify = c("HL4", "HL6", "HH6", "ED2A", "melevel", "caretakerdis"),
#'                  var_interested_match = c("HL5", "HT12", "windex5", "windex10", "disability"),
#'                  var_sampling_weight = "hhweight")
#'
#' dat_hl <- do.call("bind_rows", dat_hl)
#'
#' @import magrittr dplyr stringr tidyr haven glue crayon

#' @export fun_data_import


fun_data_import <- function(country = "Pakistan",
                            dataset = "hl",
                            var_id = c("HH1", "HH2", "HL1"),
                            var_interested_identify = c("disability", "wscore", "windex5r"),
                            var_interested_match = c("ST3$", "TS5B", "TS6"),
                            var_sampling_weight = "hhweight",
                            level = 1){
  print(country)
  cat(str_c(rep("-", times = level), "Start to import the data\n"))

  #define the path of data
  path_spss <- str_glue('{path_data}{country}/spss/')
  spss_file <- list.files(path = path_spss, recursive = TRUE)
  spss_file <- spss_file[str_fun(spss_file, str = str_c(dataset, ".sav"))]
  final_path <- str_glue('{path_spss}{spss_file}')

  if (length(final_path) == 0) {
    cat(str_c(str_c(rep("-", times = level + 1), collapse = ""),
              glue_col("{yellow No {dataset}}\n\n")))
    mdat <- NULL
    return(mdat)
    next
  }
  #read the available variable name
  if(country %in% "Mongolia"){
    dat_names <- lapply(final_path, read_sav, n_max = 0, encoding="latin1")
  } else {
    dat_names <- lapply(final_path, read_sav, n_max = 0)
  }

  dat_names <- do.call("bind_rows", dat_names)
  #not all country have the needed variables
  #judge if LN exists, or else only read HH1 and HH2
  #var_id <- var_id[var_id %in% names(dat_names)]

  #only read the var_interested exist, and report which one not exist
  res <- var_interested_identify[!var_interested_identify %in% names(dat_names)]
  cat(str_c(str_c(rep("-", times = level + 1), collapse = ""),
            glue_col("{blue '{res}' not in the {dataset}}\n\n")))

  res <- sapply(var_interested_match,
                FUN = function(x, prefix){any(startsWith(x, prefix))},
                x = names(dat_names)) %>% unlist()

  cat(str_c(str_c(rep("-", times = level + 1), collapse = ""),
            glue_col("{blue '{var_interested_match[!res]}' not in the {dataset}}\n\n")))

  var_interested_match <- lapply(var_interested_match[res],
                                 FUN = function(x, prefix){names(dat_names)[startsWith(x, prefix)]},
                                 x = names(dat_names)) %>% unlist()

  col_select <- c(var_id,
                  var_interested_identify[var_interested_identify %in% names(dat_names)],
                  var_interested_match,
                  var_sampling_weight)
  col_select <- unique(col_select)

  #read the data
  if (length(final_path) == 1) {
    if(country %in% "Mongolia") {mdat <- read_sav(final_path, col_select = all_of(col_select), encoding="latin1")}
    if(!country %in% "Mongolia") {mdat <- read_sav(final_path, col_select = all_of(col_select))}
    }

  if (length(final_path) > 1) {
    for (i in 1:length(final_path)){
      if(country %in% "Mongolia"){dat_names <- read_sav(final_path[i], n_max = 0, encoding="latin1")}
      if(!country %in% "Mongolia"){dat_names <- read_sav(final_path[i], n_max = 0)}

      res <- col_select[!col_select %in% names(dat_names)]
      if(length(res) > 0){
        cat(str_c(str_c(rep("-", times = level + 1), collapse = ""),
                  glue_col("{blue '{res}' not in the {final_path[i]}}\n\n")))
      }
      country_sub <- str_replace(final_path[i],  path_spss, "")
      country_sub <- str_split(country_sub, "/")[[1]][1]
      if(country %in% "Mongolia"){mdat_m <- read_sav(final_path[i],
                              col_select = all_of(col_select[col_select %in% names(dat_names)]), encoding="latin1")}
      if(!country %in% "Mongolia"){mdat_m <- read_sav(final_path[i],
                              col_select = all_of(col_select[col_select %in% names(dat_names)]))}

      mdat_m$country_sub <- country_sub
      if (i == 1) {mdat <- mdat_m}
      if (i != 1) {mdat <- bind_rows(mdat, mdat_m)}
    }
  }


  cat(str_c(rep("-", times = level), "Done\n"))

  #generate unique_id and variable "sampling_weight"
  iso3 <- regions$iso3[which(regions$country == country)]
  mdat <- mdat %>% mutate(
    country_full = country,
    country_iso3 = iso3
  )

  if(length(var_id) == 2){
    var_id <- str_c("{", var_id, "}")
    str_unique_id <- str_c(c(iso3, var_id), collapse = "_")
    mdat <- mdat %>% mutate(
      unique_hh = str_glue(str_unique_id),
    )

    # for countries like Pakistan has sub country datasets, the HH1 and HH2 were repeated used, so
    # need to add sub country to generate unique id
    if (length(final_path) > 1) {
      mdat <- mdat %>% mutate(
        unique_hh = str_c(unique_hh, country_sub, sep = "_"),
      )
    }

  }

  if(length(var_id) == 3){
    mdat <- mdat %>% mutate(
      unique_hh = str_glue(str_c(c(iso3, str_c("{", var_id[1:2], "}")), collapse = "_")),
      unique_hh_ln = str_glue(str_c(c(iso3, str_c("{", var_id, "}")), collapse = "_")),
    )

    if (length(final_path) > 1) {
      mdat <- mdat %>% mutate(
        unique_hh = str_c(unique_hh, country_sub, sep = "_"),
        unique_hh_ln = str_c(unique_hh_ln, country_sub, sep = "_")
      )
    }

  }

  mdat <- mdat %>% select(country_full, country_iso3, starts_with("unique_hh"), all_of(col_select))

  return(mdat)
}
