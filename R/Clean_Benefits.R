#' Clean Income and Tax data
#'
#' Generate clean variables for individual market income (excluding benefit income) and taxes from the raw
#' Family Resources Survey data sets that are needed for the microsimulation model.
#'
#' @param data Data table. The combined adult, child, and household raw FRS data files.
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
clean_benefits <- function(data,
                           benefits_data) {

  clean_data <- copy(data[order(sernum, person)])
  clean_benefits_data <- copy(benefits_data)

  #####################
  ### HOUSING #########

  #####################
  ### CHILD BENEFIT ###

  #### Child Benefit

  clean_benefits_data[benefit == 3, b_chb := benamt*(52/12)]

  #########################
  ### SOCIAL ASSISTANCE ###

  #### Social assistance - income support and JSA IB

  clean_benefits_data[benefit == 19 | (benefit == 14 & var2 %in% c(2,4)), b_is_jsaib := benamt*(52/12) ]

  #### Social assistance - employment and support allowance income based

  clean_benefits_data[benefit == 16 & var2 %in% c(2,4), b_esaib := benamt*(52/12) ]

  #####################
  ### DISABILITY ######

  ### employment and support allowance

  clean_benefits_data[benefit == 16 & var2 %in% c(1,3), b_esacb := benamt*(52/12) ]

  ### attendance allowance

  clean_benefits_data[benefit == 12, b_aa := benamt*(52/12)]

  ### disability living allowance (self care)

  clean_benefits_data[benefit == 1, b_dla_sc := benamt*(52/12)]

  ### disability living allowance (mobility)

  clean_benefits_data[benefit == 2, b_dla_mb := benamt*(52/12)]

  ### industrial injuries pension / industrial injuries disablement benefit (IIDB)

  clean_benefits_data[benefit == 15, b_iip := benamt*(52/12)]

  ### carer allowance

  clean_benefits_data[benefit == 13, b_ca := benamt*(52/12)]

  ### severe disablement allowance

  clean_benefits_data[benefit == 10, b_sda := benamt*(52/12)]

  #####################
  ### HEALTH ##########

  ### statutory sick pay

  #####################
  ### UNEMPLOYMENT ####

  #### training allowance

  clean_benefits_data[benefit == 36, b_ta := benamt*(52/12)]

  #### unemployment benefit - JSA contribution based

  clean_benefits_data[benefit == 14 & var2 %in% c(1,3), b_jsacb := benamt*(52/12)]

  #####################
  ### OLD AGE #########

  #### war pension / armed forces compensation scheme

  clean_benefits_data[benefit == 8, b_warpen := benamt*(52/12)]

  #### pension credit

  clean_benefits_data[benefit == 4, b_pencredit := benamt*(52/12)]

  #### winter fuel allowance

  clean_benefits_data[benefit == 4, b_wfa := benamt/12]

  #### basic state pension

  clean_benefits_data[benefit == 5, b_bsp := benamt/12]

  #####################
  ### TAX CREDITS #####

  #### child tax credit

  clean_benefits_data[benefit == 91, b_childtc := benamt*(52/12)]

  #### working tax credit

  clean_benefits_data[benefit == 90, b_worktc := benamt*(52/12)]

  #### other

  clean_benefits_data[benefit == 30, b_oth := benamt*(52/12)]

  #####################
  ### MATERNITY #######

  #### maternity allowance

  clean_benefits_data[benefit == 21, b_ma := benamt*(52/12)]

  #### statutory maternity benefit

  #####################
  ### Other Benefits ##

  #### universal credit

  clean_benefits_data[benefit == 95, b_ucred := benamt*(52/12)]

  #### personal independence payment (mobility part)

  clean_benefits_data[benefit == 97, b_pipmb := benamt*(52/12)]

  #### personal independence payment (daily living part)

  clean_benefits_data[benefit == 98, b_pipmb := benamt*(52/12)]

  ############################
  #### Retain variables ######

  ben_vars <- Hmisc::Cs(sernum, benunit, person,
                        b_chb, b_is_jsaib)


  clean_data <- clean_data[ , ben_vars, with = F]



  return(clean_data)

}
