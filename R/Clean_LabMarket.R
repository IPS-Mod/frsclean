#' Clean Labour Market data
#'
#' Generate clean variables for labour market information from the raw
#' Family Resources Survey data sets that are needed for the microsimulation model.
#'
#' Note that this function requires data generated from the `clean_income` function using the
#' same input data
#'
#' @param data Data table. The combined adult, child, and household raw FRS data files.
#' @param job_data Data table. The raw FRS job file.
#' @param income_data Data table. The income data cleaned by the `clean_income` function.
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
                            job_data,
                            income_data){

  clean_data <- copy(data)
  clean_job_data <- copy(job_data)
  lab_income <- copy(income_data[, c("sernum", "benunit", "person", "y_empl", "y_semp")])

  ######################
  #### Clean variables

  #### Economic status

  clean_data[adult == 0, lm_status := 6]
  clean_data[(fted == 2 | fted == -1) & age <5 , lm_status := 0]
  clean_data[empstatb == 1, lm_status := 2]
  clean_data[empstatb %in% c(2,3,4,5,6,8), lm_status := 3]
  clean_data[empstatb == 9, lm_status := 4]
  clean_data[empstatb == 7, lm_status := 5]
  clean_data[empstatb == 13, lm_status := 6]
  clean_data[empstatb == 10, lm_status := 7]
  clean_data[empstatb %in% c(11,12), lm_status := 8]
  clean_data[empstatb == 14, lm_status := 9]

  clean_data[is.na(lm_status), lm_status := 6]

  clean_data[, lm_status := factor(lm_status,
                                   levels = c(0,2:9),
                                   labels = c("pre_school","self_employed", "employed",
                                              "pensioner", "unemployed", "student",
                                              "inactive", "sick_disabled", "family_worker"))]

  #### Civil service

  clean_data[sic == 84, lm_civserv := ifelse(sic == 84, 1, 0)]
  clean_data[is.na(lm_civserv), lm_civserv := 0]

  ### firm size

  clean_job_data <- clean_job_data[jobtype == 1,]

  clean_job_data[emplany %in% 1:4, empany := 1]
  clean_job_data[emplany %in% 5:8, empany := 2]
  clean_job_data[emplany == 9, empany := 3]

  clean_job_data[numemp %in% 1:4, numempl := 1]
  clean_job_data[numemp %in% 5:8, numempl := 2]
  clean_job_data[numemp == 9, numempl := 3]

  clean_job_data[, lm_firmsize := 0]
  clean_job_data[!(is.na(emplany)) & emplany > 0 , lm_firmsize := empany]
  clean_job_data[!(is.na(numempl)) & numempl > 0 & lm_firmsize == 0, lm_firmsize := numempl]

  clean_job_data[lm_firmsize == 1, lm_firmsize := 12]
  clean_job_data[lm_firmsize == 2, lm_firmsize := 262]
  clean_job_data[lm_firmsize == 3, lm_firmsize := 500]

  clean_data <- merge(clean_data, clean_job_data, by = c("sernum","benunit","person"), all.x = TRUE)

  clean_data[is.na(lm_firmsize), lm_firmsize := 0]

  ### work history (time in months)

  clean_data[ptwk < 0 | is.na(ptwk), ptwk := 0]
  clean_data[ftwk < 0 | is.na(ftwk), ftwk := 0]

  clean_data[, lm_wrkhist := ptwk + ftwk]
  clean_data[adult == 0, lm_wrkhist := 0]
  clean_data[, lm_wrkhist := lm_wrkhist*12]

  ### occupation (soc2010)

  clean_data[, lm_occ := -1]
  clean_data[!(is.na(soc2010)) & soc2010 != -1, lm_occ := soc2010/1000]
  clean_data[lm_occ == 0, lm_occ := -1]

  ### looking for work

  clean_data[empstatb == 7 & (lktrain == 1 | lkwork == 1), lm_jobseek := 1]
  clean_data[is.na(lm_jobseek), lm_jobseek := 0]

  ### hours

  clean_data <- merge(clean_data, lab_income, by = c("sernum","benunit","person"), all.x = TRUE)

        ### employed working hours
  clean_data[everot == 2 & inearns > 0, lm_hours_empl := totus1]
  clean_data[everot == 1 & inearns > 0, lm_hours_empl := usuhr + pothr]
  clean_data[y_empl > 0 & lm_hours_empl > 0, temp := y_empl/lm_hours_empl] ## impute hours if reported earnings by 0 hours
  clean_data[y_empl > 0 & lm_hours_empl == 0, lm_hours_empl := y_empl/mean(temp)]
  clean_data[lm_hours_empl > 0 & lm_hours_empl < 1, lm_hours_empl := 1] ## if less than 1 hour, set equal to 1
  clean_data[, lm_hours_empl := as.integer(lm_hours_empl)]
  clean_data[lm_status != "employed", lm_hours_empl := 0]

        ### self-employed working hours
  clean_data[everot == 2, lm_hours_semp := totus1]
  clean_data[everot == 1, lm_hours_semp := usuhr + pothr]
  clean_data[y_semp > 0 & lm_hours_semp > 0, temp := y_semp/lm_hours_semp] ## impute hours if reported earnings by 0 hours
  clean_data[y_semp > 0 & lm_hours_semp == 0, lm_hours_semp := y_semp/mean(temp)]
  clean_data[, lm_hours_semp := as.integer(lm_hours_semp)]
  clean_data[lm_status != "self_employed", lm_hours_semp := 0]

       ### total hours
  clean_data[, lm_hours := lm_hours_empl + lm_hours_semp]

  clean_data[is.na(lm_hours), lm_hours := 0]
  clean_data[is.na(lm_hours_semp), lm_hours_semp := 0]
  clean_data[is.na(lm_hours_empl), lm_hours_empl := 0]

  ### industry
  clean_data[, lm_ind := -1]
  clean_data[sic >= 1 & sic <= 3, lm_ind := 1]
  clean_data[sic >= 5 & sic <= 9, lm_ind := 2]
  clean_data[sic >= 10 & sic <= 33, lm_ind := 3]
  clean_data[sic == 35, lm_ind := 4]
  clean_data[sic >= 36 & sic <= 39, lm_ind := 5]
  clean_data[sic >= 41 & sic <= 43, lm_ind := 6]
  clean_data[sic >= 45 & sic <= 47, lm_ind := 7]
  clean_data[sic >= 49 & sic <= 53, lm_ind := 8]
  clean_data[sic >= 55 & sic <= 56, lm_ind := 9]
  clean_data[sic >= 58 & sic <= 63, lm_ind := 10]
  clean_data[sic >= 64 & sic <= 66, lm_ind := 11]
  clean_data[sic == 68, lm_ind := 12]
  clean_data[sic >= 69 & sic <= 75, lm_ind := 13]
  clean_data[sic >= 77 & sic <= 82, lm_ind := 14]
  clean_data[sic == 84, lm_ind := 15]
  clean_data[sic == 85, lm_ind := 16]
  clean_data[sic >= 86 & sic <= 88, lm_ind := 17]
  clean_data[sic >= 90 & sic <= 93, lm_ind := 18]
  clean_data[sic >= 94 & sic <= 96, lm_ind := 19]
  clean_data[sic >= 97 & sic <= 98, lm_ind := 20]
  clean_data[sic >= 99 & !is.na(sic), lm_ind := 21]


  ######################
  #### Retain variables

  clean_data <- clean_data[, c("sernum", "benunit", "person",
                               "lm_status", "lm_civserv", "lm_firmsize",
                               "lm_wrkhist", "lm_occ", "lm_jobseek",
                               "lm_hours", "lm_hours_empl", "lm_hours_semp", "lm_ind")]


  return(clean_data)

}
