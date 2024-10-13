#' Clean Income and Tax data
#'
#' Generate clean variables for individual market income (excluding benefit income) and taxes from the raw
#' Family Resources Survey data sets that are needed for the microsimulation model.
#'
#' @param data Data table. The combined adult, child, and household raw FRS data files.
#' @param benefits_data Data table. The raw FRS benefits file.
#' @param benunit_data Data table. The raw FRS benefit unit file.
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
clean_benefits <- function(data,
                           benefits_data,
                           benunit_data) {

  clean_data <- copy(data[order(sernum, person)])
  clean_benefits_data <- copy(benefits_data)
  clean_benunit_data <- copy(benunit_data)

  #####################
  ### HOUSING #########

  clean_benunit_data[, bho := 0]
  clean_benunit_data[hbothbu == 1, bho := hbothamt]

  clean_benunit_data_col <- clean_benunit_data[, .(bho = sum(bho)), by = c("benunit")]

  #####################
  ### CHILD BENEFIT ###

  #### Child Benefit

  clean_benefits_data[benefit == 3, bch := benamt*(52/12)]
  clean_benefits_data[is.na(bch), bch := 0]

  #########################
  ### SOCIAL ASSISTANCE ###

  #### Social assistance - income support and JSA IB

  clean_benefits_data[benefit == 19 | (benefit == 14 & var2 %in% c(2,4)), bsa := benamt*(52/12) ]
  clean_benefits_data[is.na(bsa), bsa := 0]

  #### Social assistance - employment and support allowance income based

  clean_benefits_data[benefit == 16 & var2 %in% c(2,4), bsa01 := benamt*(52/12) ]
  clean_benefits_data[is.na(bsa01), bsa01 := 0]

  #####################
  ### DISABILITY ######

  ### employment and support allowance

  clean_benefits_data[benefit == 16 & var2 %in% c(1,3), bdict02 := benamt*(52/12) ]
  clean_benefits_data[is.na(bdict02), bdict02 := 0]

  ### attendance allowance

  clean_benefits_data[benefit == 12, bdioa := benamt*(52/12)]
  clean_benefits_data[is.na(bdioa), bdioa := 0]

  ### disability living allowance (self care)

  clean_benefits_data[benefit == 1, bdisc := benamt*(52/12)]
  clean_benefits_data[is.na(bdisc), bdisc := 0]

  ### disability living allowance (mobility)

  clean_benefits_data[benefit == 2, bdimb := benamt*(52/12)]
  clean_benefits_data[is.na(bdimb), bdimb := 0]

  ### industrial injuries pension / industrial injuries disablement benefit (IIDB)

  clean_benefits_data[benefit == 15, bdiwi := benamt*(52/12)]
  clean_benefits_data[is.na(bdiwi), bdiwi := 0]

  ### carer allowance

  clean_benefits_data[benefit == 13, bcrdi := benamt*(52/12)]
  clean_benefits_data[is.na(bcrdi), bcrdi := 0]

  ### severe disablement allowance

  clean_benefits_data[benefit == 10, bdisv := benamt*(52/12)]
  clean_benefits_data[is.na(bdisv), bdisv := 0]

  #####################
  ### HEALTH ##########

  ### statutory sick pay

  #####################
  ### UNEMPLOYMENT ####

  #### training allowance

  clean_benefits_data[benefit == 36, buntr := benamt*(52/12)]
  clean_benefits_data[is.na(buntr), buntr := 0]

  #### unemployment benefit - JSA contribution based

  clean_benefits_data[benefit == 14 & var2 %in% c(1,3), bunct := benamt*(52/12)]
  clean_benefits_data[is.na(bunct), bunct := 0]

  #####################
  ### OLD AGE #########

  #### war pension / armed forces compensation scheme

  clean_benefits_data[benefit == 8, boawr := benamt*(52/12)]
  clean_benefits_data[is.na(boawr), boawr := 0]

  #### pension credit

  clean_benefits_data[benefit == 4, boamt := benamt*(52/12)]
  clean_benefits_data[is.na(boamt), boamt := 0]

  #### winter fuel allowance

  clean_benefits_data[benefit == 62, boaht := benamt/12]
  clean_benefits_data[is.na(boaht), boaht := 0]

  #####################
  ### TAX CREDITS #####

  #### child tax credit

  clean_benefits_data[benefit == 91, bfamt := benamt*(52/12)]
  clean_benefits_data[is.na(bfamt), bfamt := 0]

  #### working tax credit

  clean_benefits_data[benefit == 90, bwkmt := benamt*(52/12)]
  clean_benefits_data[is.na(bwkmt), bwkmt := 0]

  ################
  #### OTHER #####

  clean_benefits_data[benefit == 30, bot := benamt*(52/12)]
  clean_benefits_data[is.na(bot), bot := 0]

  #####################
  ### MATERNITY #######

  #### maternity allowance

  clean_benefits_data[benefit == 21, bmana := benamt*(52/12)]
  clean_benefits_data[is.na(bmana), bmana := 0]

  #### statutory maternity benefit

  #####################
  ### Other Benefits ##

  #### universal credit

  clean_benefits_data[benefit == 95, bsauc := benamt*(52/12)]
  clean_benefits_data[is.na(bsauc), bsauc := 0]

  #### personal independence payment (mobility part)

  clean_benefits_data[benefit == 97, bdimbwa := benamt*(52/12)]
  clean_benefits_data[is.na(bdimbwa), bdimbwa := 0]

  #### personal independence payment (daily living part)

  clean_benefits_data[benefit == 98, bdiscwa := benamt*(52/12)]
  clean_benefits_data[is.na(bdiscwa), bdiscwa := 0]

  ######################################
  #### collapse to individual level ####

  clean_benefits_data_col <- clean_benefits_data %>%
    group_by(sernum, benunit, person) %>%
    summarise(bch = sum(bch),
              bsa = sum(bsa),
              bsa01 = sum(bsa01),
              bdict02 = sum(bdict02),
              bdioa = sum(bdioa),
              bdisc = sum(bdisc),
              bdimb = sum(bdimb),
              bdiwi = sum(bdiwi),
              bcrdi = sum(bcrdi),
              bdisv = sum(bdisv),
              buntr = sum(buntr),
              bunct = sum(bunct),
              boawr = sum(boawr),
              boamt = sum(boamt),
              boaht = sum(boaht),
              bfamt = sum(bfamt),
              bwkmt = sum(bwkmt),
              bot = sum(bot),
              bmana = sum(bmana),
              bsauc = sum(bsauc),
              bdimbwa = sum(bdimbwa),
              bdiscwa = sum(bdiscwa)) %>%
    ungroup() %>% setDT

  ############################
  #### Merge to the data #####

  clean_data <- merge(clean_data, clean_benefits_data_col, by = c("sernum","benunit","person"), all.x = TRUE)
  clean_data <- merge(clean_data, clean_benunit_data_col, by = "benunit", all.x = TRUE)

  clean_data[is.na(bho), bho := 0]
  clean_data[hrpid != 1, bho := 0]

  ############################
  #### Retain variables ######

  ben_vars <- Hmisc::Cs(sernum, benunit, person,
                        bch, bsa, bsa01, bho, bdict02, bdioa, bdisc, bdimb, bdiwi, bcrdi, bdisv,
                        buntr, bunct, boawr, boamt, boaht, bfamt, bwkmt,
                        bot, bmana, bsauc, bdimbwa, bdiscwa)

  clean_data <- clean_data[ , ben_vars, with = F]

  clean_data[is.na(bsauc), bch := 0]
  clean_data[is.na(bsauc), bsa := 0]
  clean_data[is.na(bsauc), bsa01 := 0]
  clean_data[is.na(bsauc), bdict02 := 0]
  clean_data[is.na(bsauc), bdioa := 0]
  clean_data[is.na(bsauc), bdisc := 0]
  clean_data[is.na(bsauc), bdimb := 0]
  clean_data[is.na(bsauc), bdiwi := 0]
  clean_data[is.na(bsauc), bcrdi := 0]
  clean_data[is.na(bsauc), bdisv := 0]

  clean_data[is.na(bsauc), buntr := 0]
  clean_data[is.na(bsauc), bunct := 0]
  clean_data[is.na(bsauc), boawr := 0]
  clean_data[is.na(bsauc), boamt := 0]
  clean_data[is.na(bsauc), boaht := 0]
  clean_data[is.na(bsauc), bfamt := 0]
  clean_data[is.na(bsauc), bwkmt := 0]

  clean_data[is.na(bsauc), bot := 0]
  clean_data[is.na(bsauc), bmana := 0]
  clean_data[is.na(bsauc), bsauc := 0]
  clean_data[is.na(bsauc), bdimbwa := 0]
  clean_data[is.na(bsauc), bdiscwa := 0]


  clean_data[is.na(bdimbwa), bdimbwa := 0]
  clean_data[is.na(bdiscwa), bdiscwa := 0]

  return(clean_data)

}
