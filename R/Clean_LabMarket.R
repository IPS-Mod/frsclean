#' Clean Labour Market data
#'
#' Generate clean variables for labour market information from the raw
#' Family Resources Survey data sets.
#'
#' Note that this function requires data generated from the `clean_income` function using the
#' same input data
#'
#' @param data Data table. The combined adult, child, and household raw FRS data files.
#' @param main_data  Data table. The main data file "frsxxyy"
#' @param job_data Data table. The raw FRS job file.
#' @param income_data Data table. The income data cleaned by the `clean_income` function.
#' @param year Numeric integer - year corresponding to the start of the financial year i.e. 2020/21 data is indexed as 2020
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
clean_labmarket <- function(data,
                            main_data,
                            job_data,
                            income_data,
                            year){

  clean_main_data <- copy(main_data[, c("sernum","benunit","intmonth","intyear")])
  clean_data <- copy(data)
  clean_job_data <- copy(job_data)
  lab_income <- copy(income_data[, c("sernum", "benunit", "person", "yem", "yse")])

  job_duration_data <- merge(clean_job_data[jobtype == 1,], clean_main_data, by = c("sernum","benunit"), all.x = TRUE)

  ######################
  #### Clean variables

  #### Economic status

  clean_data[adult == 0, les := 6]
  clean_data[(fted == 2 | fted == -1) & age <5 , les := 0]
  clean_data[empstatb == 1, les := 2]
  clean_data[empstatb %in% c(2,3,4,5,6,8), les := 3]
  clean_data[empstatb == 9, les := 4]
  clean_data[empstatb == 7, les := 5]
  clean_data[empstatb == 13, les := 6]
  clean_data[empstatb == 10, les := 7]
  clean_data[empstatb %in% c(11,12), les := 8]
  clean_data[empstatb == 14, les := 9]

  clean_data[is.na(les), les := 6]

  #### Civil service

  clean_data[sic == 84, lcs := ifelse(sic == 84, 1, 0)]
  clean_data[is.na(lcs), lcs := 0]

  ### firm size

  clean_job_data <- clean_job_data[jobtype == 1,]

  clean_job_data[emplany %in% 1:4, empany := 1]
  clean_job_data[emplany %in% 5:8, empany := 2]
  clean_job_data[emplany == 9, empany := 3]

  clean_job_data[numemp %in% 1:4, numempl := 1]
  clean_job_data[numemp %in% 5:8, numempl := 2]
  clean_job_data[numemp == 9, numempl := 3]

  clean_job_data[, lfs := 0]
  clean_job_data[!(is.na(emplany)) & emplany > 0 , lfs := empany]
  clean_job_data[!(is.na(numempl)) & numempl > 0 & lfs == 0, lfs := numempl]

  clean_job_data[lfs == 1, lfs := 12]
  clean_job_data[lfs == 2, lfs := 262]
  clean_job_data[lfs == 3, lfs := 500]

  clean_data <- merge(clean_data, clean_job_data, by = c("sernum","benunit","person"), all.x = TRUE)

  clean_data[is.na(lfs), lfs := 0]

  ### work history (time in months)

  clean_data[ptwk < 0 | is.na(ptwk), ptwk := 0]
  clean_data[ftwk < 0 | is.na(ftwk), ftwk := 0]

  clean_data[, liwwh := ptwk + ftwk]
  clean_data[adult == 0, liwwh := 0]
  clean_data[, liwwh := liwwh*12]

  ### occupation (soc2010 up to 2020/21, soc2020 after)

  if (year <= 2020) {
  clean_data[, loc := -1]
  clean_data[!(is.na(soc2010)) & soc2010 != -1, loc := soc2010/1000]
  clean_data[loc == 0, loc := -1]

  } else if (year > 2020){
  clean_data[, loc := -1]
  clean_data[!(is.na(soc2020)) & soc2020 != -1, loc := soc2020/1000]
  clean_data[loc == 0, loc := -1]

  }

  ### looking for work

  clean_data[empstatb == 7 & (lktrain == 1 | lkwork == 1), lowas := 1]
  clean_data[is.na(lowas), lowas := 0]

  ### hours

  clean_data <- merge(clean_data, lab_income, by = c("sernum","benunit","person"), all.x = TRUE)

        ### employed working hours
  clean_data[everot == 2 & inearns > 0, lhw00 := totus1]
  clean_data[everot == 1 & inearns > 0, lhw00 := usuhr + pothr]
  clean_data[yem > 0 & lhw00 > 0, temp := yem/lhw00] ## impute hours if reported earnings by 0 hours
  clean_data[yem > 0 & lhw00 == 0, lhw00 := yem/mean(temp)]
  clean_data[lhw00 > 0 & lhw00 < 1, lhw00 := 1] ## if less than 1 hour, set equal to 1
  clean_data[, lhw00 := as.integer(lhw00)]
  #clean_data[lm_status != "employed", lhw00 := 0]

        ### self-employed working hours
  clean_data[everot == 2, lhw01 := totus1]
  clean_data[everot == 1, lhw01 := usuhr + pothr]
  clean_data[yse > 0 & lhw01 > 0, temp := yse/lhw01] ## impute hours if reported earnings by 0 hours
  clean_data[yse > 0 & lhw01 == 0, lhw01 := yse/mean(temp)]
  clean_data[, lhw01 := as.integer(lhw01)]
  #clean_data[lm_status != "self_employed", lhw01 := 0]

       ### total hours
  clean_data[, lhw := lhw00 + lhw01]

  clean_data[is.na(lhw), lhw := 0]
  clean_data[is.na(lhw01), lhw01 := 0]
  clean_data[is.na(lhw00), lhw00 := 0]

  ### industry
  clean_data[, lindi := -1]
  clean_data[sic >= 1 & sic <= 3, lindi := 1]
  clean_data[sic >= 5 & sic <= 9, lindi := 2]
  clean_data[sic >= 10 & sic <= 33, lindi := 3]
  clean_data[sic == 35, lindi := 4]
  clean_data[sic >= 36 & sic <= 39, lindi := 5]
  clean_data[sic >= 41 & sic <= 43, lindi := 6]
  clean_data[sic >= 45 & sic <= 47, lindi := 7]
  clean_data[sic >= 49 & sic <= 53, lindi := 8]
  clean_data[sic >= 55 & sic <= 56, lindi := 9]
  clean_data[sic >= 58 & sic <= 63, lindi := 10]
  clean_data[sic >= 64 & sic <= 66, lindi := 11]
  clean_data[sic == 68, lindi := 12]
  clean_data[sic >= 69 & sic <= 75, lindi := 13]
  clean_data[sic >= 77 & sic <= 82, lindi := 14]
  clean_data[sic == 84, lindi := 15]
  clean_data[sic == 85, lindi := 16]
  clean_data[sic >= 86 & sic <= 88, lindi := 17]
  clean_data[sic >= 90 & sic <= 93, lindi := 18]
  clean_data[sic >= 94 & sic <= 96, lindi := 19]
  clean_data[sic >= 97 & sic <= 98, lindi := 20]
  clean_data[sic >= 99 & !is.na(sic), lindi := 21]

  #### Job duration data

  job_duration_data[, lwrkyr := workyr]
  job_duration_data[, lwrkmth := workmth]
  job_duration_data[, lprev := ifelse(wrkprev %in% 1:2, 1, 2)]

  suppressWarnings(
  job_duration_data[, strtwrk := my(paste0(lwrkmth,"-",lwrkyr))]
  )
  suppressWarnings(
  job_duration_data[, intdate := my(paste0(intmonth,"-",intyear))]
  )

  job_duration_data[, ldayswrk := as.numeric(intdate - strtwrk)]
  job_duration_data[ldayswrk < 0, ldayswrk := NA]


  job_duration_data <- job_duration_data[, c("sernum", "benunit", "person",
                                             "lwrkyr", "lwrkmth", "ldayswrk", "lprev")]

  clean_data <- merge(clean_data, job_duration_data, by = c("sernum", "benunit", "person"), all.x = TRUE)

  ######################
  #### Retain variables

  clean_data <- clean_data[, c("sernum", "benunit", "person",
                               "les", "lcs", "lfs",
                               "liwwh", "loc", "lowas",
                               "lhw", "lhw00", "lhw01", "lindi",
                               "lwrkyr", "lwrkmth", "ldayswrk", "lprev")]


  return(clean_data)

}
