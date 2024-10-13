#' Read Family Resources Survey 2022/2023
#'
#' Read in the Family Resources Survey data for the 2022-23 fiscal year
#'
#' @param root Character. The root directory
#' @param file Character. The file path and name
#'
#' @return Data table
#' @export
#'
#' @examples
#'
#'
#' \dontrun{
#'
#'
#' }
read_frs_2022_23 <- function(root = "X:/",
                             file = "HAR_PR/PR/IPS_beyond_SMI_NIHR202996/General/R/data-family-resources-survey/data/raw") {

  cat(crayon::yellow("\tReading Family Resources Survey 2022/2023:\n\n"))

  ###################################
  ####### Read in the data ##########

  ###############
  ### main data

  cat(crayon::green("\tMain\n"))

  main <- data.table::fread(
    paste0(root, file, "/2022/frs2223.tab"), showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  data.table::setnames(main, names(main), tolower(names(main)))

  main[, intmonth := mnthcode]
  main[, intyear := ifelse(mnthcode %in% 4:12, 2022, 2023)]

  main_vars <- Hmisc::Cs(sernum, benunit, intdate, intmonth, intyear,
                         subrent, tentyp2,
                         ptentyp2, tenure, landlord, accjob, hbenamt)

  main_data <- main[ , main_vars, with=F]

  ###############
  ### adult data

  cat(crayon::green("\tAdult\n"))

  adult <- data.table::fread(
    paste0(root, file, "/2022/adult.tab"), showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  data.table::setnames(adult, names(adult), tolower(names(adult)))

  data.table::setnames(adult, c("educqual","educany","educleft","educt97"), c("dvhiqual","fted","tea","tea9697"))
  adult[, tea := NA] ## age left ft education, not in this year

  adult_vars <- Hmisc::Cs(sernum, benunit, person, hrpid, age80, sex, marital, empstatb, fted, empstati, typeed2,
                          allpay1, allpay2, allpay3, allpay4, allow1, allow2, allow3, allow4,
                          royal2, royal3, royal4, royyr1, royyr2, royyr3, royyr4, cvpay,
                          tea, tea9697, dvhiqual, sic, ftwk, ptwk, soc2020, lkwork, lktrain, nindinc,
                          mntamt1, mntamt2, mntusam1, mntusam2, mntus1, mntus2, apamt, apdamt, pareamt,
                          r01, r02, r03, r04, r05, r06, r07, r08, r09, r10, r11, r12, r13, r14,
                          disd01, disd02, disd03, disd04, disd05, disd06, disd07, disd08, disd09, disd10,
                          inearns, seincam2, redamt, redany)

  adult <- adult[ , adult_vars, with=F]

  setnames(adult, "age80", "age")

  adult[, adult := 1]

  ###############
  ### child data

  cat(crayon::green("\tChild\n"))

  child <- data.table::fread(
    paste0(root, file, "/2022/child.tab"), showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  data.table::setnames(child, names(child), tolower(names(child)))
  data.table::setnames(child, c("educft"), c("fted"))

  child_vars <- Hmisc::Cs(sernum, benunit, person, age, sex, fted, chincdv, chamtern, chamttst,
                          r01, r02, r03, r04, r05, r06, r07, r08, r09, r10, r11, r12, r13, r14,
                          cdisd01, cdisd02, cdisd03, cdisd04, cdisd05, cdisd06, cdisd07, cdisd08, cdisd09, cdisd10,
                          chealth1, chcond)

  child <- child[ , child_vars, with=F]

  child[, adult := 0]

  ### combine adult and child data-sets by row-bind

  data <- rbindlist(list(adult, child), use.names = TRUE, fill = TRUE)

  ######################
  ### Household data

  cat(crayon::green("\tHousehold\n"))

  hhold <- data.table::fread(
    paste0(root, file, "/2022/househol.tab"), showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  data.table::setnames(hhold, names(hhold), tolower(names(hhold)))

  data.table::setnames(hhold, "bedroom", "bedroom6")


  hhold_vars <- Hmisc::Cs(sernum, gross4, gvtregno, bedroom6, ptentyp2,
                          hhrent, tenure, tentyp2, subrent, mortint,
                          cwatamtd, csewamt, watsewrt,
                          gbhscost, nihscost, ctband,
                          chrgamt1, chrgamt3, chrgamt4, chrgamt5,
                          chrgamt6, chrgamt7, chrgamt8, chrgamt9)

  hhold <- hhold[ , hhold_vars, with=F]

  ## no of bedrooms data variable is empty in this year - set all equal to 0
  hhold[,bedroom6 := as.numeric(bedroom6)]
  hhold[is.na(bedroom6), bedroom6 := 0]

  data <- merge(data, hhold, by = c("sernum"))

  #######################################################################
  ##### Other data to process separately and aggregate before merging

  ##############
  ### benefit unit data

  cat(crayon::green("\tBenefit unit\n"))

  benunit_data <- data.table::fread(
    paste0(root, file, "/2022/benunit.tab"), showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  data.table::setnames(benunit_data, names(benunit_data), tolower(names(benunit_data)))


  benunit_vars <- Hmisc::Cs(sernum, benunit,
                            totcapb3, hbothamt, hbothbu)

  benunit_data <- benunit_data[ , benunit_vars, with=F]

  ##############
  ### pension data

  cat(crayon::green("\tPension\n"))

  pension_data <- data.table::fread(
    paste0(root, file, "/2022/pension.tab"), showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  data.table::setnames(pension_data, names(pension_data), tolower(names(pension_data)))


  pension_vars <- Hmisc::Cs(sernum, benunit, person,
                            penpay, ptamt, poamt, ptinc, poinc, penoth)

  pension_data <- pension_data[ , pension_vars, with=F]

  ##############
  ### pension provision data

  cat(crayon::green("\tPension provision\n"))

  penprov_data <- data.table::fread(
    paste0(root, file, "/2022/penprov.tab"), showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  data.table::setnames(penprov_data, names(penprov_data), tolower(names(penprov_data)))


  penprov_vars <- Hmisc::Cs(sernum, benunit, person,
                            penamt, stemppen)

  penprov_data <- penprov_data[ , penprov_vars, with=F]

  ##############
  ### maintenance data

  cat(crayon::green("\tMaintenance\n"))

  maint_data <- data.table::fread(
    paste0(root, file, "/2022/maint.tab"), showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  data.table::setnames(maint_data, names(maint_data), tolower(names(maint_data)))


  maint_vars <- Hmisc::Cs(sernum, benunit, person, mramt, mruamt, mrus)

  maint_data <- maint_data[ , maint_vars, with=F]

  ##############
  ### Job data

  cat(crayon::green("\tJob\n"))

  job_data <- data.table::fread(
    paste0(root, file, "/2022/job.tab"), showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  data.table::setnames(job_data, names(job_data), tolower(names(job_data)))


  job_vars <- Hmisc::Cs(sernum, benunit, person,
                        sejblong, sejbmths, jobtype, emplany, numemp,
                        totus1, everot, usuhr, pothr,
                        workyr, workmth, wrkprev)

  job_data <- job_data[ , job_vars, with=F]

  ##############
  ### Odd-Job data

  cat(crayon::green("\tOdd-job\n"))

  oddjob_data <- data.table::fread(
    paste0(root, file, "/2022/oddjob.tab"), showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  data.table::setnames(oddjob_data, names(oddjob_data), tolower(names(oddjob_data)))


  oddjob_vars <- Hmisc::Cs(sernum, person, benunit,
                           ojamt, ojnow)

  oddjob_data <- oddjob_data[ , oddjob_vars, with=F]

  ######################
  ### Accounts data

  cat(crayon::green("\tAccounts\n"))

  accounts_data <- data.table::fread(
    paste0(root, file, "/2022/accounts.tab"), showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  data.table::setnames(accounts_data, names(accounts_data), tolower(names(accounts_data)))


  accounts_vars <- Hmisc::Cs(sernum, benunit, person,
                             accint, account, acctax, invtax)

  accounts_data <- accounts_data[ , accounts_vars, with=F]

  ##########################
  ### Benefit data

  cat(crayon::green("\tBenefits\n"))

  benefits_data <- data.table::fread(
    paste0(root, file, "/2022/benefits.tab"), showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  data.table::setnames(benefits_data, names(benefits_data), tolower(names(benefits_data)))


  benefits_vars <- Hmisc::Cs(sernum, benunit, person,
                             benefit, benamt, pres, var1, var2, var3)

  benefits_data <- benefits_data[ , benefits_vars, with=F]

  ##################################################
  ### RETURN DATA SETS AS A LIST OF DATA TABLES ####

  return(list(data = data,
              main_data = main_data,
              benunit_data = benunit_data,
              pension_data = pension_data,
              penprov_data = penprov_data,
              maint_data = maint_data,
              job_data = job_data,
              oddjob_data = oddjob_data,
              accounts_data = accounts_data,
              benefits_data = benefits_data
  ))
}
