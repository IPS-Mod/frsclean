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
#' @return Returns a new set of variables
#' @export

frsclean <- function(root,
                     file,
                     years = 2020,
                     ages = NULL,
                     keep_vars = NULL,
                     complete_vars = NULL){

  cat(crayon::bgGreen("Cleaning the Family Resources Survey Data\n"))

  start_time <- Sys.time()

  ###############################################################################
  #### For each wave, wrap the reading function in the global cleaning function
  #### and update a data list with cleaned data tables

  data_list <- list()

  ### 2020/2021 tax year (April 2020 - March 2021)

  if (2020 %in% years){

    wave <- frs_clean_global(read_frs_2020_21(root = root, file = file),
                             ages = ages, keep_vars = keep_vars, complete_vars = complete_vars)

    wave[, year := 2020]
    wave[, fiscal_year := "2020/2021"]

    data_list <- append(data_list, list(wave)) ; rm(wave)
  }


  #############################################################
  ### Combine all waves in the list into a single dataset

  data <- data.table::rbindlist(data_list, use.names = T, fill = T)

  #######################
  ## Record time taken

  end_time <- Sys.time()

  tdiff <- difftime(end_time, start_time, units = "mins")

  time <- paste0("Total Data reading and cleaning time: ", round(tdiff,2), " minutes")

  cat(crayon::bgGreen(time))

  return(data)
}
