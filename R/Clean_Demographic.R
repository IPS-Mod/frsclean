#' Clean Demographic data
#'
#' Generate clean variables for individual demographics and generate identifiers from the raw
#' Family Resources Survey data sets.
#'
#' @param data Data table. The combined adult, child, and household raw FRS data files.
#' @param main_data  Data table. The main data file "frsxxyy"
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
clean_demographic <- function(data,
                              main_data){

  clean_main_data <- copy(main_data[, c("sernum","benunit","intdate")])

  clean_data <- copy(data)

  clean_data <- merge(clean_data, clean_main_data, by = c("sernum","benunit"), all.x = T)

  #################################################
  #### Identifiers for being a mother / father

  clean_data[, idhh := sernum]
  clean_data[, idorighh := sernum]

  clean_data[, idperson := sernum*100 + person]
  clean_data[, idorigperson := person]

  clean_data[, idmother := ifelse(sex == 2 & (r01 == 7 | r02 == 7 | r03 == 7 | r04 == 7 |
                                               r05 == 7 | r06 == 7 | r07 == 7 | r08 == 7 |
                                               r09 == 7 | r10 == 7 | r11 == 7 | r12 == 7 |
                                               r13 == 7 | r14 == 7), 1, 0)]

  clean_data[, idfather := ifelse(sex == 1 & (r01 == 7 | r02 == 7 | r03 == 7 | r04 == 7 |
                                               r05 == 7 | r06 == 7 | r07 == 7 | r08 == 7 |
                                               r09 == 7 | r10 == 7 | r11 == 7 | r12 == 7 |
                                               r13 == 7 | r14 == 7), 1, 0)]

  clean_data[, idpartner := ifelse((r01 %in% 1:2 | r02 %in% 1:2 | r03 %in% 1:2 | r04 %in% 1:2 |
                                    r05 %in% 1:2 | r06 %in% 1:2 | r07 %in% 1:2 | r08 %in% 1:2 |
                                    r09 %in% 1:2 | r10 %in% 1:2 | r11 %in% 1:2 | r12 %in% 1:2 |
                                    r13 %in% 1:2 | r14 %in% 1:2), 1, 0)]

  clean_data[is.na(idmother), idmother := 0]
  clean_data[is.na(idfather), idfather := 0]

  clean_data[, c("r01","r02","r03","r04","r05","r06","r07",
                 "r08","r09","r10","r11","r12","r13","r14") := NULL]

  #### region

  clean_data[, dct := 15]

  clean_data[, drgn1 := gvtregno]

  #### age

  setnames(clean_data, c("age"), c("dag"))

  #### sex

  clean_data[, dgn := ifelse(sex == 1, 1, 0)]

  ### citizenship (some missings in country of origin so UKMOD assumes 1 for all)

  clean_data[, dcz := 1]

  ### marital status

  clean_data[, dms := 0]
  clean_data[marital == 2 | marital == 3, dms := 1]
  clean_data[marital == 1, dms := 2]
  clean_data[marital == 5, dms := 3]
  clean_data[marital == 6, dms := 4]
  clean_data[marital == 4, dms := 5]

  ### disabled

  clean_data[, ddi := 0]
  clean_data[adult == 1 & empstati == 9, ddi := 1]
  clean_data[adult == 0 & chealth1 == 1 & chcond == 1, ddi := 1]

  #####################
  #### education

        ### current education status

  clean_data[fted %in% c(-1,2,NA), dec := 0]
  clean_data[typeed2 == 1, dec := 1]
  clean_data[((typeed2 == 2 | typeed2 == 4)|((typeed2 == 3 | typeed2 ==8 ) & dag < 11) |
             (is.na(typeed2) & fted == 1 & dag < 11)), dec := 2]
  clean_data[((typeed2 == 5 | typeed2 == 6) |
             ((typeed2 == 3 | typeed2 == 8) & dag >=11 & dag <= 16) |
             (is.na(typeed2) & fted == 1 & dag >=11 & dag <=16)), dec := 3]
  clean_data[(typeed2 == 7 | ((typeed2==3|typeed2==8) & dag >16) |
             (is.na(typeed2) & fted == 1 & dag > 16)), dec := 4]
  clean_data[((typeed2 == 7 | typeed2 == 8) & dag >= 19), dec := 5]
  clean_data[typeed2 == 9 |
             (is.na(typeed2) & fted == 1 & dag >= 19), dec := 6]

        ### Highest education status
        ### children
        ### adults in education (assume the level below current status)
        ### adults not in education (age completed fte, highest qual achieved if missing)

  clean_data[dec > 1 & adult == 0, deh := dec - 2]
  clean_data[(dec < 2 | is.na(dec)) & adult == 0, deh := 0 ]

  clean_data[dec > 1 & adult == 1, deh := dec - 2]
  clean_data[dec == 6 & adult == 1, deh := 3] ## lower secondary -> tertiary

  clean_data[tea == -1 & tea9697 == 97, deh := 0]
  clean_data[tea < 11, deh := 0]
  clean_data[tea %in% 11:15, deh := 1]
  clean_data[(tea %in% 16:17 | (is.na(tea) & dvhiqual >= 29 & !is.na(dvhiqual))), deh := 2]
  clean_data[(tea %in% 18:19 | (is.na(tea) & dvhiqual %in% 18:28)), deh := 3]
  clean_data[(tea == 20 | (is.na(tea) & dvhiqual %in% 8:17)), deh := 4]
  clean_data[(tea %in% 21:64 | (is.na(tea) & dvhiqual %in% 1:7)), deh := 5]

              # impute missing values (take the median education by age group)

        clean_data[dag %in% 0:19, age_gr := 1]
        clean_data[dag %in% 20:29, age_gr := 2]
        clean_data[dag %in% 30:39, age_gr := 3]
        clean_data[dag %in% 40:49, age_gr := 4]
        clean_data[dag %in% 50:59, age_gr := 5]
        clean_data[dag %in% 60:99, age_gr := 6]

        clean_data[, med_eduhigh := median(deh, na.rm = TRUE), by = "age_gr"]

        clean_data[is.na(deh), deh := med_eduhigh]

        ### when achieved highest education status
        ### tea2 = age when completed highest education qualification. if tea is missing
        ### then impute based on highest qualification received

  clean_data[deh == 0, dew := -1]
  clean_data[dec == 0 & deh != 0, tea2 := tea]

  clean_data[deh == 1, tea2 := ifelse(dag < 10, dag, 10)]
  clean_data[deh == 2, tea2 := ifelse(dag < 16, dag, 16)]
  clean_data[deh == 3, tea2 := ifelse(dag < 19, dag, 19)]
  clean_data[deh == 4, tea2 := ifelse(dag < 20, dag, 20)]
  clean_data[deh == 5, tea2 := ifelse(dag < 22, dag, 22)]

  clean_data[, yearnow := as.numeric( stringr::str_sub(intdate, -4, -1) )]

  clean_data[, cohort := yearnow - dag]

  clean_data[is.na(tea2) & tea9697 == 97, tea2 := 0 ]
  clean_data[is.na(tea2), tea2 := tea ]

  clean_data[, dew := cohort + tea2]
  clean_data[is.na(dew), dew := -1]


        ### number of years

  clean_data[dec == 0 & deh != 0, dey := ((dew - cohort) - 5)]
  clean_data[dec > 0 , dey := ifelse(dag - 5 > 0, dag - 5, 0)]
  clean_data[dag < 5 | deh == 0 , dey := 0]

  #### weight variable

  setnames(clean_data, c("gross4"), c("dwt"))

  #### interview data
  clean_data[, ddt := intdate]
  clean_data[substr(intdate,2,2) == "/" , ddt := paste0("0",intdate)] ## add leading zero to single digit months


  clean_data[, month := substr(ddt,1,2)]
  clean_data[, year := stringr::str_sub(ddt,-4)]
  clean_data[, day := 15]

  clean_data[, ddt := as.numeric(paste0(year,month,day))]


  ######################
  #### Retain variables

  clean_data <- clean_data[, c("sernum", "benunit", "person", "ddt",
                               "idperson", "idhh", "idmother", "idfather",
                               "idorighh", "idorigperson", "idpartner",
                               "dwt", "dgn", "dag", "dms", "dcz", "ddi",
                               "dec", "deh", "dew", "dey",
                               "drgn1", "dct")]


  return(clean_data)

}
