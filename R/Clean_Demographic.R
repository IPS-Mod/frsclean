#' Clean Demographic data
#'
#' Generate clean variables for individual demographics and generate identifiers from the raw
#' Family Resources Survey data sets that are needed for the microsimulation model.
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

  #### Identifiers for being a mother / father

  clean_data[, id_hh := sernum]

  clean_data[, id_person := sernum*100 + person]

  clean_data[, id_mother := ifelse(sex == 2 & (r01 == 7 | r02 == 7 | r03 == 7 | r04 == 7 |
                                                r05 == 7 | r06 == 7 | r07 == 7 | r08 == 7 |
                                                r09 == 7 | r10 == 7 | r11 == 7 | r12 == 7 |
                                                r13 == 7 | r14 == 7), 1, 0)]

  clean_data[, id_father := ifelse(sex == 1 & (r01 == 7 | r02 == 7 | r03 == 7 | r04 == 7 |
                                                r05 == 7 | r06 == 7 | r07 == 7 | r08 == 7 |
                                                r09 == 7 | r10 == 7 | r11 == 7 | r12 == 7 |
                                                r13 == 7 | r14 == 7), 1, 0)]

  clean_data[is.na(id_mother), id_mother := 0]
  clean_data[is.na(id_father), id_father := 0]

  clean_data[, c("r01","r02","r03","r04","r05","r06","r07",
                 "r08","r09","r10","r11","r12","r13","r14") := NULL]

  #### government region

  clean_data[, d_region1 := factor(gvtregno,
                                  levels = c(1,2,4:13),
                                  labels = c("North East","North West", "Yorks and Humber",
                                             "East Midlands", "West Midlands", "East of England",
                                             "London", "South East", "South West", "Wales",
                                             "Scotland", "Northern Ireland"))]

  clean_data[gvtregno %in% 1:10, d_country := 1 ]
  clean_data[gvtregno == 11, d_country := 2 ]
  clean_data[gvtregno == 12, d_country := 3 ]
  clean_data[gvtregno == 13, d_country := 4 ]

  clean_data[, d_country := factor(d_country,
                                  levels = 1:4,
                                  labels = c("England","Wales","Scotland","Northern Ireland"))]

  #### age

  setnames(clean_data, c("age"), c("d_age"))

  #### sex

  clean_data[, d_sex := factor(sex, levels = 1:2, labels = c("male","female"))]

  ### marital status

  clean_data[, d_marstat := 0]
  clean_data[marital == 2 | marital == 3, d_marstat := 1]
  clean_data[marital == 1, d_marstat := 2]
  clean_data[marital == 5, d_marstat := 3]
  clean_data[marital == 6, d_marstat := 4]
  clean_data[marital == 4, d_marstat := 5]

  clean_data[, d_marstat := factor(d_marstat,
                            levels = 0:5,
                            labels = c("child_data","single","married","separated",
                                       "divorced","widowed"))]

  #### education

        ### current education status

  clean_data[fted %in% c(-1,2,NA), d_educur := 0]
  clean_data[typeed2 == 1, d_educur := 1]
  clean_data[((typeed2 == 2 | typeed2 == 4)|((typeed2 == 3 | typeed2 ==8 ) & d_age < 11) |
             (is.na(typeed2) & fted == 1 & d_age < 11)), d_educur := 2]
  clean_data[((typeed2 == 5 | typeed2 == 6) |
             ((typeed2 == 3 | typeed2 == 8) & d_age >=11 & d_age <= 16) |
             (is.na(typeed2) & fted == 1 & d_age >=11 & d_age <=16)), d_educur := 3]
  clean_data[(typeed2 == 7 | ((typeed2==3|typeed2==8) & d_age >16) |
             (is.na(typeed2) & fted == 1 & d_age > 16)), d_educur := 4]
  clean_data[((typeed2 == 7 | typeed2 == 8) & d_age >= 19), d_educur := 5]
  clean_data[typeed2 == 9 |
             (is.na(typeed2) & fted == 1 & d_age >= 19), d_educur := 6]

        ### Highest education status
        ### children
        ### adults in education (assume the level below current status)
        ### adults not in education (age completed fte, highest qual achieved if missing)

  clean_data[d_educur > 1 & adult == 0, d_eduhigh := d_educur - 2]
  clean_data[(d_educur < 2 | is.na(d_educur)) & adult == 0, d_eduhigh := 0 ]

  clean_data[d_educur > 1 & adult == 1, d_eduhigh := d_educur - 2]
  clean_data[d_educur == 6 & adult == 1, d_eduhigh := 3] ## lower secondary -> tertiary

  clean_data[tea == -1 & tea9697 == 97, d_eduhigh := 0]
  clean_data[tea < 11, d_eduhigh := 0]
  clean_data[tea %in% 11:15, d_eduhigh := 1]
  clean_data[(tea %in% 16:17 | (is.na(tea) & dvhiqual >= 29 & !is.na(dvhiqual))), d_eduhigh := 2]
  clean_data[(tea %in% 18:19 | (is.na(tea) & dvhiqual %in% 18:28)), d_eduhigh := 3]
  clean_data[(tea == 20 | (is.na(tea) & dvhiqual %in% 8:17)), d_eduhigh := 4]
  clean_data[(tea %in% 21:64 | (is.na(tea) & dvhiqual %in% 1:7)), d_eduhigh := 5]

              # impute missing values (take the median education by age group)

        clean_data[d_age %in% 0:19, age_gr := 1]
        clean_data[d_age %in% 20:29, age_gr := 2]
        clean_data[d_age %in% 30:39, age_gr := 3]
        clean_data[d_age %in% 40:49, age_gr := 4]
        clean_data[d_age %in% 50:59, age_gr := 5]
        clean_data[d_age %in% 60:99, age_gr := 6]

        clean_data[, med_eduhigh := median(d_eduhigh, na.rm = TRUE), by = "age_gr"]

        clean_data[is.na(d_eduhigh), d_eduhigh := med_eduhigh]

        ### when achieved highest education status
        ### tea2 = age when completed highest education qualification. if tea is missing
        ### then impute based on highest qualification received

  clean_data[d_eduhigh == 0, d_eduwhen := -1]
  clean_data[d_educur == 0 & d_eduhigh != 0, tea2 := tea]

  clean_data[d_eduhigh == 1, tea2 := ifelse(d_age < 10, d_age, 10)]
  clean_data[d_eduhigh == 2, tea2 := ifelse(d_age < 16, d_age, 16)]
  clean_data[d_eduhigh == 3, tea2 := ifelse(d_age < 19, d_age, 19)]
  clean_data[d_eduhigh == 4, tea2 := ifelse(d_age < 20, d_age, 20)]
  clean_data[d_eduhigh == 5, tea2 := ifelse(d_age < 22, d_age, 22)]

  clean_data[, yearnow := as.numeric( stringr::str_sub(intdate, -4, -1) )]

  clean_data[, cohort := yearnow - d_age]

  clean_data[is.na(tea2) & tea9697 == 97, tea2 := 0 ]
  clean_data[is.na(tea2), tea2 := tea ]

  clean_data[, d_eduwhen := cohort + tea2]


        ### number of years

  clean_data[d_educur == 0 & d_eduhigh != 0, d_eduyears := ((d_eduwhen - cohort) - 5)]
  clean_data[d_educur > 0 , d_eduyears := ifelse(d_age - 5 > 0, d_age - 5, 0)]
  clean_data[d_age < 5 | d_eduhigh == 0 , d_eduyears := 0]

        ### education factors

  clean_data[, d_educur := factor(d_educur,
                                   levels = 0:6,
                                   labels = c("not_in_education","pre_primary","primary","lower_secondary",
                                              "upper_secondary","post_secondary","tertiary"))]

  clean_data[, d_eduhigh := factor(d_eduhigh,
                                  levels = 0:5,
                                  labels = c("none","primary","lower_secondary",
                                             "upper_secondary","post_secondary","tertiary"))]

  #### weight variable

  setnames(clean_data, c("gross4"), c("d_wght"))

  #### interview data
  setnames(clean_data, c("intdate"), c("d_intdate"))


  ######################
  #### Retain variables

  clean_data <- clean_data[, c("sernum", "benunit", "person", "d_intdate",
                               "id_person", "id_hh", "id_mother", "id_father",
                               "d_wght", "d_sex", "d_age", "d_marstat",
                               "d_educur", "d_eduhigh", "d_eduwhen", "d_eduyears",
                               "d_region1", "d_country")]


  return(clean_data)

}
