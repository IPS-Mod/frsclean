#' Clean Health data
#'
#' Generate clean variables for individual status.
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
clean_health <- function(data){

  clean_data <- copy(data)

  #################################################
  #### Clean health condition identifiers

  ### adult dataset

  clean_data[adult == 1,  hcond1  := case_when(disd01 %in% 1 ~ 1, disd01 %in% c(2,3) ~ 0)]
  clean_data[adult == 1,  hcond2  := case_when(disd02 %in% 1 ~ 1, disd02 %in% c(2,3) ~ 0)]
  clean_data[adult == 1,  hcond3  := case_when(disd03 %in% 1 ~ 1, disd03 %in% c(2,3) ~ 0)]
  clean_data[adult == 1,  hcond4  := case_when(disd04 %in% 1 ~ 1, disd04 %in% c(2,3) ~ 0)]
  clean_data[adult == 1,  hcond5  := case_when(disd05 %in% 1 ~ 1, disd05 %in% c(2,3) ~ 0)]
  clean_data[adult == 1,  hcond6  := case_when(disd06 %in% 1 ~ 1, disd06 %in% c(2,3) ~ 0)]
  clean_data[adult == 1,  hcond7  := case_when(disd07 %in% 1 ~ 1, disd07 %in% c(2,3) ~ 0)]
  clean_data[adult == 1,  hcond8  := case_when(disd08 %in% 1 ~ 1, disd08 %in% c(2,3) ~ 0)]
  clean_data[adult == 1,  hcond9  := case_when(disd09 %in% 1 ~ 1, disd09 %in% c(2,3) ~ 0)]
  clean_data[adult == 1,  hcond10 := case_when(disd10 %in% 1 ~ 1, disd10 %in% c(2,3) ~ 0)]

  ### child dataset

  clean_data[adult == 0,  hcond1  := case_when(cdisd01 %in% 1 ~ 1, cdisd01 %in% c(2,3) ~ 0)]
  clean_data[adult == 0,  hcond2  := case_when(cdisd02 %in% 1 ~ 1, cdisd02 %in% c(2,3) ~ 0)]
  clean_data[adult == 0,  hcond3  := case_when(cdisd03 %in% 1 ~ 1, cdisd03 %in% c(2,3) ~ 0)]
  clean_data[adult == 0,  hcond4  := case_when(cdisd04 %in% 1 ~ 1, cdisd04 %in% c(2,3) ~ 0)]
  clean_data[adult == 0,  hcond5  := case_when(cdisd05 %in% 1 ~ 1, cdisd05 %in% c(2,3) ~ 0)]
  clean_data[adult == 0,  hcond6  := case_when(cdisd06 %in% 1 ~ 1, cdisd06 %in% c(2,3) ~ 0)]
  clean_data[adult == 0,  hcond7  := case_when(cdisd07 %in% 1 ~ 1, cdisd07 %in% c(2,3) ~ 0)]
  clean_data[adult == 0,  hcond8  := case_when(cdisd08 %in% 1 ~ 1, cdisd08 %in% c(2,3) ~ 0)]
  clean_data[adult == 0,  hcond9  := case_when(cdisd09 %in% 1 ~ 1, cdisd09 %in% c(2,3) ~ 0)]
  clean_data[adult == 0,  hcond10 := case_when(cdisd10 %in% 1 ~ 1, cdisd10 %in% c(2,3) ~ 0)]

  ######################
  #### Retain variables

  clean_data <- clean_data[, c("sernum", "benunit", "person",
                               "hcond1","hcond2","hcond3","hcond4","hcond5",
                               "hcond6","hcond7","hcond8","hcond9","hcond10")]


  return(clean_data)

}
