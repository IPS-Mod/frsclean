#' Clean Assets data
#'
#' Generate clean variables for assets from the raw
#' Family Resources Survey data sets.
#'
#' @param data Data table. The combined adult, child, and household raw FRS data files.
#' @param main_data  Data table. The main data file "frsxxyy"
#' @param benunit_data Data table. The raw FRS benefit unit data file.
#'
#' @return Data table
#' @export
#'
#' @examples
#'
#'
#' \dontrun{
#'
#' }
clean_assets <- function(data,
                         main_data,
                         benunit_data) {

  clean_data <- copy(data[order(sernum, person)])

  clean_main_data <- copy(main_data[, c("sernum","benunit","ptentyp2","tenure","landlord","accjob")])

  clean_data <- merge(clean_data, clean_main_data, by = c("sernum","benunit"), all.x = T)

  ######################################
  #### Create clean asset variables ####

  ## Number of bedrooms (capped at 6)
  clean_data[, amrrm := bedroom6]

  ## Housing costs (rent) (household responsible person only)
  clean_data[, amrtn := 0]
  clean_data[ptentyp2 == 6, amrtn := 1]
  clean_data[ptentyp2 == 5, amrtn := 2]
  clean_data[ptentyp2 %in% 1:2, amrtn := 5]
  clean_data[tenure == 5, amrtn := 6]
  clean_data[tenure == 6, amrtn := 7]
  clean_data[(landlord %in% 3:6 | accjob == 1) & amrtn == 0, amrtn := 4]
  clean_data[amrts == 0, amrtn := 3]

  ## Financial capital
  benunit_data[, afc := totcapb3]

  clean_data <- merge(clean_data, benunit_data, by = c("sernum", "benunit"), all.x = TRUE)
  clean_data[is.na(afc), afc := 0]

  ############################
  #### Retain variables ######

  asset_vars <- Hmisc::Cs(sernum, benunit, person,
                          amrrm, amrtn, afc)


  clean_data <- clean_data[ , asset_vars, with = F]

  return(clean_data)

}
