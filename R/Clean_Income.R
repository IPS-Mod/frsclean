#' Clean Income and Tax data
#'
#' Generate clean variables for individual market income (excluding benefit income) and taxes from the raw
#' Family Resources Survey data sets.
#'
#' @param data Data table. The combined adult, child, and household raw FRS data files.
#' @param pension_data Data table. The raw FRS pension data file.
#' @param job_data Data table. The raw FRS job file.
#' @param oddjob_data Data table. The raw FRS odd-job file.
#' @param accounts_data Data table. the raw FRS accounts file.
#' @param benefits_data Data table. The raw FRS benefits file.
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
clean_income <- function(data,
                         pension_data,
                         job_data,
                         oddjob_data,
                         accounts_data,
                         benefits_data) {

  clean_data <- copy(data[order(sernum, person)])

  ####################################
  #### Clean income variables ########

  ### Employment Income
  clean_data[, yem := inearns * (52/12)]
  clean_data[yem == 0, yem := NA]
  clean_data[is.na(yem), yem := 0]

  ## Private pension income
  pension_data[, ypp := 0]
  pension_data[!is.na(penpay) & penpay > 0, ypp := ypp + penpay]
  pension_data[ptinc == 2 & !is.na(ptamt) & ptamt > 0, ypp := ypp + ptamt]
  pension_data[(poinc == 2 | penoth == 1) & poamt > 0 & !is.na(poamt), ypp := ypp + poamt]
  pension_data[, ypp := ypp * (52/12)]

  pension_data <- pension_data[, .(ypp = sum(ypp, na.rm = TRUE)), by = c("sernum","benunit","person")]

  clean_data <- merge(clean_data, pension_data, by = c("sernum","benunit","person"), all.x = TRUE)
  clean_data[is.na(ypp), ypp := 0]

  ## Self-employment Income
  clean_data[, yse := seincam2 * (52/12)]

  ## number of years self-employed (using longest work history if > 1 semp job)
  job_data <- job_data[order(sernum, benunit, person, -sejblong, -sejbmths)]

  job_data <- job_data[!duplicated(sernum, benunit, person),]

  job_data[, yseny := as.numeric(sejblong)]
  job_data[sejblong == 1 & sejbmths > 0 & !(is.na(sejbmths)), yseny := 1 + (sejbmths/12)]
  job_data[sejblong == 0 & sejbmths > 0 & !(is.na(sejbmths)), yseny := sejbmths/12]
  job_data[sejblong <= 0 & sejbmths < 0, yseny := 0]

  job_data <- job_data[, c("sernum", "person", "benunit", "yseny")]

  clean_data <- merge(clean_data, job_data, by = c("sernum", "benunit", "person"), all.x = TRUE)
  clean_data[yseny > age, yseny := age]

  clean_data[is.na(yse), yse := 0]
  clean_data[is.na(yseny), yseny := 0]

  ## Investment income

  ## investment (not taxable)
    accounts_data[, yiynt := 0]
    accounts_data[account != 2, yiynt := accint]
    accounts_data[account == 2 & accint > (70/52), yiynt := (70/52)]
    accounts_data[account == 2 & accint <= (70/52), yiynt := accint]
    accounts_data[, yiynt := yiynt*(52/12)]

  ## investment (taxable)
    accounts_data[, yiytx := 0]
    accounts_data[acctax != 1 & (account %in% c(1,3,5,27,28)), yiytx := accint]
    accounts_data[acctax == 1 & (account %in% c(1,3,5,27,28)), yiytx := accint*1.25] ## convert to gross. 20% taken at source
    accounts_data[account == 2, yiytx := ifelse(accint > (70/52), accint - (70/52), 0)]

    accounts_data[account == 6 & invtax == 1, yiytx := accint*1.25]
    accounts_data[(account == 6 & invtax == 1) | account == 7 | account == 8, yiytx := accint]

    accounts_data[, yiytx := yiytx*(52/12)]

    accounts_data <- accounts_data[, .(yiynt = sum(yiynt, na.rm = TRUE),
                                       yiytx  = sum(yiytx, na.rm = TRUE)), by = c("sernum", "person")]

  clean_data <- merge(clean_data, accounts_data, by = c("sernum", "person"), all.x = TRUE)
  clean_data[is.na(yiynt), yiynt := 0]
  clean_data[is.na(yiytx), yiytx := 0]
  clean_data[, yiy := yiytx + yiynt]

  ## Property income

      ## income from rent (non-taxable)
  clean_data[, y_prop_in := 0]
  clean_data[tentyp2 %in% 5:6 & subrent > 0 & !(is.na(subrent)), y_prop_in := subrent*(52/12)]
  clean_data[cvpay < 0 | is.na(cvpay), cvpay := 0]
  clean_data[, cvpayhh := max(cvpay), by = c("sernum")]
  clean_data[, y_prop_in := y_prop_in + cvpayhh*(52/12)] ## from boarders/lodgers net of housing benefit
  clean_data[hrpid != 1, y_prop_in := 0] ## set to zero if not person responsibel for hhold costs
  clean_data[y_prop_in*12 <= 7500, yprnt := y_prop_in]

      ## income from rent (taxable)
  clean_data[, y_prop_out := 0]
  clean_data[royyr1 > 0 & !(is.na(royyr1)), y_prop_out := royyr1]
  clean_data[, y_prop_out := y_prop_out*(52/12)]
  clean_data[, yprtx := 0]
  clean_data[, yprtx := yprtx + y_prop_out]
  clean_data[y_prop_in*12 > 7500, yprtx := yprtx + y_prop_in]

  clean_data[, ypr := yprtx + yprnt]

      ## Maintenance income (private transfers)
  clean_data[mntus1 == 2 & mntusam1 > 0 & !is.na(mntusam1), mntamt1 := mntusam1]
  clean_data[mntamt1 < 0 | is.na(mntamt1), mntamt1 := 0]
  clean_data[mntus2 == 2 & mntusam2 > 0 & !is.na(mntusam2), mntamt2 := mntusam2]
  clean_data[mntamt2 < 0 | is.na(mntamt2), mntamt2 := 0]
  clean_data[, yptmp := (mntamt1 + mntamt2)*(52/12)]

  ## Private transfers (other than maintenance)
      benefits_data <- benefits_data[benefit %in% 31:35 & pres == 1,]
      benefits_data[, yptot := benamt]
      benefits_data[is.na(yptot), yptot := 0]
      benefits_data <- benefits_data[, .(yptot = sum(yptot, na.rm = TRUE)), by = c("sernum", "person")]

  clean_data <- merge(clean_data, benefits_data, by = c("sernum", "person"), all.x = TRUE)
  clean_data[apamt > 0 & !(is.na(apamt)), yptot := yptot + apamt*(52/12)]
  clean_data[apdamt > 0 & !(is.na(apdamt)), yptot := yptot + apdamt*(52/12)]
  clean_data[pareamt > 0 & !(is.na(pareamt)), yptot := yptot + pareamt*(52/12)]
  clean_data[allow3 == 1, yptot := yptot + allpay3*(52/12)]
  clean_data[allow4 == 1, yptot := yptot + allpay4*(52/12)]
  clean_data[allow1 == 1, yptot := yptot + allpay1*(52/12)]

  ## Other income
      oddjob_data[, y_odd := 0]
      oddjob_data[ojnow == 1, y_odd := ojamt*(52/12)]
      oddjob_data <- oddjob_data[, .(y_odd = sum(y_odd)), by = c("sernum", "person")]

  clean_data <- merge(clean_data, oddjob_data, by = c("sernum", "person"), all.x = TRUE)
  clean_data[is.na(y_odd), y_odd := 0]

  clean_data[, yot01 := 0]
  clean_data[, yot01 := yot01 + y_odd]
  clean_data[chamtern > 0 & !(is.na(chamtern)), yot01 := yot01 + (chamtern*52/12)]
  clean_data[allow2 == 1, yot01 := yot01 + allpay2*(52/12)]
  clean_data[royal2 == 1, yot01 := yot01 + royyr2*(52/12)]
  clean_data[royal3 == 1, yot01 := yot01 + royyr3*(52/12)]
  clean_data[royal4 == 1, yot01 := yot01 + royyr4*(52/12)]
  clean_data[chamttst > 0 & !(is.na(chamttst)), yot01 := yot01 + (chamttst*52/12)]

  ## Disposable income
  clean_data[is.na(nindinc) | nindinc %in% -9:-1, nindinc := 0]
  clean_data[adult == 1, yds := as.numeric(nindinc)]
  clean_data[adult == 0 & !is.na(chincdv), yds := chincdv]
  clean_data[, yds := yds*(52/12)]

  ############################
  #### Retain variables ######

  inc_vars <- Hmisc::Cs(sernum, benunit, person,
                        yem, ypp, yse, yseny, yiynt, yiyntx, yiy,
                        ypr, yprtx, yprnt, yptmp, yptot, yot01, yds)


  clean_data <- clean_data[ , inc_vars, with = F]



  return(clean_data)

}
