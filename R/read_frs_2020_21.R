#' Read Family Resources Survey 2020/2021
#'
#' Read in the Family Resources Survey data for the 2020-21 fiscal year, including the
#' two years prior if there is a need for a larger sample size
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
#' }
read_frs_2020_21 <- function(root = "C:/",
                             file = "Users/damon/OneDrive/Documents/Datasets/Family Resources Survey/tab") {

  cat(crayon::yellow("\tReading Family Resources Survey 2020/2021:\n"))

  ###################################
  ####### Read in the data ##########

  ###############
  ### main data

  cat(crayon::green("\tMain\n"))

  main <- data.table::fread(
    paste0(root, file, "/2020/frs2021.tab"), showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  data.table::setnames(main, names(main), tolower(names(main)))

  main_vars <- Hmisc::Cs(sernum, benunit, intdate,
                         subrent, tentyp2)

  main_data <- main[ , main_vars, with=F]

  ###############
  ### adult data

  cat(crayon::green("\tAdult\n"))

  adult <- data.table::fread(
    paste0(root, file, "/2020/adult.tab"), showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  data.table::setnames(adult, names(adult), tolower(names(adult)))

  adult_vars <- Hmisc::Cs(sernum, benunit, person, hrpid, age80, sex, marital, empstatb, fted, typeed2,
                          allpay1, allpay2, allpay3, allpay4, allow1, allow2, allow3, allow4,
                          royal2, royal3, royal4, royyr1, royyr2, royyr3, royyr4, cvpay,
                          tea, tea9697, dvhiqual, sic, ftwk, ptwk, soc2010, lkwork, lktrain, nindinc,
                          mntamt1, mntamt2, mntusam1, mntusam2, mntus1, mntus2, apamt, apdamt, pareamt,
                          r01, r02, r03, r04, r05, r06, r07, r08, r09, r10, r11, r12, r13, r14,
                          inearns, seincam2)

  adult <- adult[ , adult_vars, with=F]

  setnames(adult, "age80", "age")

  adult[, adult := 1]

  ###############
  ### child data

  cat(crayon::green("\tChild\n"))

  child <- data.table::fread(
    paste0(root, file, "/2020/child.tab"), showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  data.table::setnames(child, names(child), tolower(names(child)))

  child_vars <- Hmisc::Cs(sernum, benunit, person, age, sex, fted, chincdv, chamtern, chamttst,
                          r01, r02, r03, r04, r05, r06, r07, r08, r09, r10, r11, r12, r13, r14)

  child <- child[ , child_vars, with=F]

  child[, adult := 0]

  ### combine adult and child data-sets by row-bind

  data <- rbindlist(list(adult, child), use.names = TRUE, fill = TRUE)

  ######################
  ### Household data

  cat(crayon::green("\tHousehold\n"))

  hhold <- data.table::fread(
    paste0(root, file, "/2020/househol.tab"), showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  data.table::setnames(hhold, names(hhold), tolower(names(hhold)))

  hhold_vars <- Hmisc::Cs(sernum, gross4, gvtregno, bedroom6, ptentyp2,
                          hhrent, tenure, tentyp2, subrent, mortint,
                          cwatamtd, csewamt, watsewrt,
                          gbhscost, nihscost)

  hhold <- hhold[ , hhold_vars, with=F]

  data <- merge(data, hhold, by = c("sernum"))

  #######################################################################
  ##### Other data to process separately and aggregate before merging

  ##############
  ### pension data

  cat(crayon::green("\tPension\n"))

  pension_data <- data.table::fread(
    paste0(root, file, "/2020/pension.tab"), showProgress = FALSE,
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
    paste0(root, file, "/2020/penprov.tab"), showProgress = FALSE,
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
    paste0(root, file, "/2020/maint.tab"), showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  data.table::setnames(maint_data, names(maint_data), tolower(names(maint_data)))


  maint_vars <- Hmisc::Cs(sernum, benunit, person, mramt, mruamt, mrus)

  maint_data <- maint_data[ , maint_vars, with=F]

  ##############
  ### Job data

  cat(crayon::green("\tJob\n"))

  job_data <- data.table::fread(
    paste0(root, file, "/2020/job.tab"), showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  data.table::setnames(job_data, names(job_data), tolower(names(job_data)))


  job_vars <- Hmisc::Cs(sernum, benunit, person,
                        sejblong, sejbmths, jobtype, emplany, numemp,
                        totus1, everot, usuhr, pothr)

  job_data <- job_data[ , job_vars, with=F]

  ##############
  ### Odd-Job data

  cat(crayon::green("\tOdd-job\n"))

  oddjob_data <- data.table::fread(
    paste0(root, file, "/2020/oddjob.tab"), showProgress = FALSE,
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
    paste0(root, file, "/2020/accounts.tab"), showProgress = FALSE,
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
    paste0(root, file, "/2020/benefits.tab"), showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  data.table::setnames(benefits_data, names(benefits_data), tolower(names(benefits_data)))


  benefits_vars <- Hmisc::Cs(sernum, person,
                             benefit, benamt, pres)

  benefits_data <- benefits_data[ , benefits_vars, with=F]

  ##################################################
  ### RETURN DATA SETS AS A LIST OF DATA TABLES ####

  return(list(data = data,
              main_data = main_data,
              pension_data = pension_data,
              penprov_data = penprov_data,
              maint_data = maint_data,
              job_data = job_data,
              oddjob_data = oddjob_data,
              accounts_data = accounts_data,
              benefits_data = benefits_data
              ))
}
