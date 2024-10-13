#' Read, Clean, and Combine Family Resources Survey data
#'
#' A wrapper function for applying all of the reading and cleaning functions for the
#' Family Resources Survey (FRS) end user licence data, selecting the
#' desired variables/observations for the analysis, and specifying complete case
#' restrictions.
#'
#' @param root Character - the root directory.
#' @param file Character - the file path and name.
#' @param years Integer vector - the years of the FRS to read, clean, and combine (defaults to 2020).
#' @param ages Integer vector - the ages in single years to retain. If NULL (default), retain all ages.
#' @param keep_vars Character vector - the names of the variables to keep (defaults to NULL - retaining all variables).
#' @param complete_vars Character vector - the names of the variables on which the selection of complete cases will be based.
#' If NULL (default) no complete-case filtering is applied.
#' @param inflate Logical - TRUE if adjusting monetary values to real-terms, FALSE otherwise.
#' @param price_year Numeric integer - year to use as the base year for inflation adjustment (default = 2022).
#' @param index Character - inflation index to use for real terms adjustment, "cpih" (default) or "rpi"
#'
#' @return Returns a new set of variables
#' @export

frsclean <- function(root = "X:/",
                     file = "HAR_PR/PR/IPS_beyond_SMI_NIHR202996/General/R/data-family-resources-survey/data/raw",
                     years = 2020,
                     ages = NULL,
                     keep_vars = NULL,
                     complete_vars = NULL,
                     inflate = TRUE,
                     price_year = 2022,
                     index = "cpih"){

  cat(crayon::green("\n\nCleaning the Family Resources Survey Data\n\n"))

  start_time <- Sys.time()

  ###############################################################################
  #### For each wave, wrap the reading function in the global cleaning function
  #### and update a data list with cleaned data tables

  data_list <- list()

  ### 2018/2019 tax year (April 2018 - March 2019)

  if (2018 %in% years){

    data <- read_frs_2018_19(root = root, file = file)
    wave <- frs_clean_global(data,
                             ages = ages,
                             keep_vars = keep_vars,
                             complete_vars = complete_vars,
                             year = 2018,
                             inflate = inflate,
                             price_year = price_year,
                             index = index)

    wave[, year := 2018]
    wave[, fiscal_year := "2018/2019"]

    data_list <- append(data_list, list(wave)) ; rm(wave)
  }

  ### 2019/2020 tax year (April 2019 - March 2020)

  if (2019 %in% years){

    data <- read_frs_2019_20(root = root, file = file)
    wave <- frs_clean_global(data,
                             ages = ages,
                             keep_vars = keep_vars,
                             complete_vars = complete_vars,
                             year = 2019,
                             inflate = inflate,
                             price_year = price_year,
                             index = index)

    wave[, year := 2019]
    wave[, fiscal_year := "2019/2020"]

    data_list <- append(data_list, list(wave)) ; rm(wave)
  }

  ### 2020/2021 tax year (April 2020 - March 2021)

  if (2020 %in% years){

    data <- read_frs_2020_21(root = root, file = file)
    wave <- frs_clean_global(data,
                             ages = ages,
                             keep_vars = keep_vars,
                             complete_vars = complete_vars,
                             year = 2020,
                             inflate = inflate,
                             price_year = price_year,
                             index = index)

    wave[, year := 2020]
    wave[, fiscal_year := "2020/2021"]

    data_list <- append(data_list, list(wave)) ; rm(wave)
  }

  ### 2021/2022 tax year (April 2021 - March 2022)

  if (2021 %in% years){

    data <- read_frs_2021_22(root = root, file = file)
    wave <- frs_clean_global(data,
                             ages = ages,
                             keep_vars = keep_vars,
                             complete_vars = complete_vars,
                             year = 2021,
                             inflate = inflate,
                             price_year = price_year,
                             index = index)

    wave[, year := 2021]
    wave[, fiscal_year := "2021/2022"]

    data_list <- append(data_list, list(wave)) ; rm(wave)
  }

  ### 2022/2023 tax year (April 2022 - March 2023)

  if (2022 %in% years){

    data <- read_frs_2022_23(root = root, file = file)
    wave <- frs_clean_global(data,
                             ages = ages,
                             keep_vars = keep_vars,
                             complete_vars = complete_vars,
                             year = 2022,
                             inflate = inflate,
                             price_year = price_year,
                             index = index)

    wave[, year := 2022]
    wave[, fiscal_year := "2022/2023"]

    data_list <- append(data_list, list(wave)) ; rm(wave)
  }


  #############################################################
  ### Combine all waves in the list into a single dataset

  data <- data.table::rbindlist(data_list, use.names = T, fill = T)

  ## UKMOD requires data sorting by household
  if (length(years) > 1){
  data <- data %>%
    mutate(idhh = idhh*100000 + year) %>%
    arrange(idhh)
  }
  ###################################################################
  ## Rescale the weights so they sum up to the price year population

  pop_size <- sum(data$dwt)
  scaled_pop_size <- sum(data[year == price_year]$dwt)

  scaling_factor <- scaled_pop_size / pop_size

  data <- mutate(data, dwt = dwt * scaling_factor)

  #######################
  ## Record time taken

  end_time <- Sys.time()

  tdiff <- difftime(end_time, start_time, units = "mins")

  time <- paste0("Total Data reading and cleaning time: ", round(tdiff,2), " minutes")

  cat(crayon::yellow(time))

  return(data)
}
