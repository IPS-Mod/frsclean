#' Clean Expenditures data
#'
#' Generate clean variables for individual expenditures from the raw
#' Family Resources Survey data sets.
#'
#' @param data Data table. The combined adult, child, and household raw FRS data files.
#' @param maint_data Data table. The raw FRS maintenance data file.
#' @param penprov_data Data table. The raw FRS pension provision data file.
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
clean_expenditure <- function(data,
                              maint_data,
                              penprov_data) {

  clean_data <- copy(data[order(sernum, person)])

  ############################################
  #### Create clean expenditure variables ####

  ## Maintenance payments
  maint_data[, xmp := 0]
  maint_data[, xmp := mramt*(52/12)]
  maint_data[mrus == 2, xmp := mruamt*(52/12)]

  clean_data <- merge(clean_data, maint_data, by = c("sernum", "benunit", "person"), all.x = TRUE)

  clean_data[is.na(xmp), xmp := 0]

  ## Housing costs (rent) (household responsible person only)
  clean_data[, xhcrt := 0]
  clean_data[!is.na(hhrent) & hhrent > 0, xhcrt := hhrent*(52/12)]
  clean_data[hrpid != 1, xhcrt := 0]

  ## Mortgage interest
  clean_data[, xhcmomi := 0]
  clean_data[!is.na(mortint) & mortint > 0, xhcmomi := mortint*(52/12)]
  clean_data[hrpid != 1, xhcmomi := 0]

  ## Housing costs (other) - water and sewage costs
  clean_data[, xhcot := 0]
  clean_data[gvtregno == 12 & cwatamtd >= 0, xhcot := xhcot + cwatamtd] ## Scot
  clean_data[gvtregno == 12 & csewamt >= 0, xhcot := xhcot + csewamt]     ## Scot
  clean_data[gvtregno < 12 & watsewrt >= 0, xhcot := xhcot + watsewrt]  ## Eng/Wales
  clean_data[, xhcot := xhcot*(52/12)]
  clean_data[hrpid != 1, xhcot := 0]

  ## Total housing costs
  clean_data[, xhc := 0]
  clean_data[gvtregno != 13 & gbhscost != -1, xhc := gbhscost] ## GB
  clean_data[gvtregno == 13 & nihscost != -1, xhc := nihscost] ## Northern Ireland
  clean_data[, xhc := xhc*(52/12)]
  clean_data[hrpid != 1, xhc := 0]

  ## Private pension provision
  penprov_data[, xpp := 0]
  penprov_data[stemppen %in% 5:6, xpp := penamt*(52/12)]
  penprov_data <- penprov_data[, .(xpp = sum(xpp)), by = c("sernum", "person", "benunit")]

  clean_data <- merge(clean_data, penprov_data, by = c("sernum", "benunit", "person"), all.x = TRUE)
  clean_data[is.na(xpp), xpp := 0]

  ############################
  #### Retain variables ######

  exp_vars <- Hmisc::Cs(sernum, benunit, person,
                        xmp, xhc, xhcrt, xhcmomi, xhcot, xpp)


  clean_data <- clean_data[ , exp_vars, with = F]

  return(clean_data)

}
