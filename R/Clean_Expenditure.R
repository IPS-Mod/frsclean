#' Clean Expenditures data
#'
#' Generate clean variables for individual expenditures from the raw
#' Family Resources Survey data sets that are needed for the microsimulation model.
#'
#' @param data Data table. The combined adult, child, and household raw FRS data files.
#' @param maint_data Data table. The raw FRS maintenance data file.
#' @param penprov_data Data table. The raw FRS pension provision data file.
#'
#' @return
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
  maint_data[, x_mp := 0]
  maint_data[, x_mp := mramt*(52/12)]
  maint_data[mrus == 2, x_mp := mruamt*(52/12)]

  clean_data <- merge(clean_data, maint_data, by = c("sernum", "benunit", "person"), all.x = TRUE)

  clean_data[is.na(x_mp), x_mp := 0]

  ## Housing costs (rent) (household responsible person only)
  clean_data[, x_hcost_rent := 0]
  clean_data[!is.na(hhrent) & hhrent > 0, x_hcost_rent := hhrent*(52/12)]
  clean_data[hrpid != 1, x_hcost_rent := 0]

  ## Mortgage interest
  clean_data[, x_hcost_mint := 0]
  clean_data[!is.na(mortint) & mortint > 0, x_hcost_mint := mortint*(52/12)]
  clean_data[hrpid != 1, x_hcost_mint := 0]

  ## Housing costs (other) - water and sewage costs
  clean_data[, x_hcost_oth := 0]
  clean_data[gvtregno == 12 & cwatamtd >= 0, x_hcost_oth := x_hcost_oth + cwatamtd] ## Scot
  clean_data[gvtregno == 12 & csewamt >= 0, x_hcost_oth := x_hcost_oth + csewamt]     ## Scot
  clean_data[gvtregno < 12 & watsewrt >= 0, x_hcost_oth := x_hcost_oth + watsewrt]  ## Eng/Wales
  clean_data[, x_hcost_oth := x_hcost_oth*(52/12)]
  clean_data[hrpid != 1, x_hcost_oth := 0]

  ## Total housing costs
  clean_data[, x_hcost := 0]
  clean_data[gvtregno != 13 & gbhscost != -1, x_hcost := gbhscost]
  clean_data[gvtregno != 13 & nihscost != -1, x_hcost := nihscost]
  clean_data[, x_hcost := x_hcost*(52/12)]
  clean_data[hrpid != 1, x_hcost := 0]

  ## Private pension provision
  penprov_data[, x_ppen := 0]
  penprov_data[stemppen %in% 5:6, x_ppen := penamt*(52/12)]
  penprov_data <- penprov_data[, .(x_ppen = sum(x_ppen)), by = c("sernum", "person", "benunit")]

  clean_data <- merge(clean_data, penprov_data, by = c("sernum", "benunit", "person"), all.x = TRUE)
  clean_data[is.na(x_ppen), x_ppen := 0]

  ############################
  #### Retain variables ######

  exp_vars <- Hmisc::Cs(sernum, benunit, person,
                        x_mp, x_hcost, x_hcost_rent, x_hcost_mint, x_ppen, x_hcost_oth)


  clean_data <- clean_data[ , exp_vars, with = F]

  return(clean_data)

}
