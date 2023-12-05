#' Inflation Adjustment
#'
#' Convert all monetary variables into real terms
#'
#' @param data Data table - the FRS dataset.
#' @param year Numeric integer - year corresponding to the start of the financial year i.e. 2020/21 data is indexed as 2020
#' @param index Character - inflation index to use for real terms adjustment, "cpih" (default) or "rpi"
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
Inflation_Adjust <- function(data,
                             year = NULL,
                             index = "cpih"){

  if (index == "cpih"){

    inflation <- frsclean::inflation[measure == "cpih",]
  } else if (index == "rpi") {

    inflation <- frsclean::inflation[measure == "rpi",]
  }

  y <- data.table::copy(year)

  inf_index <- (100/as.numeric(inflation[year == y, "index"]))

  ### Adjust all monetary variables by inf_index to get into the base year terms

  ### INCOME VARIABLES
  data[, yem := yem * inf_index]
  data[, ypp := ypp * inf_index]
  data[, yse := yse * inf_index]
  data[, yiynt := yiynt * inf_index]
  data[, yiytx := yiytx * inf_index]
  data[, yittx := yittx * inf_index]
  data[, ydvtx := ydvtx * inf_index]
  data[, yiy := yiy * inf_index]
  data[, ypr := ypr * inf_index]
  data[, yprtx := yprtx * inf_index]
  data[, yprnt := yprnt * inf_index]
  data[, yptmp := yptmp * inf_index]
  data[, yptot := yptot * inf_index]
  data[, yot01 := yot01 * inf_index]
  data[, yds := yds * inf_index]

  ### BENEFIT VARIABLES
  data[, bch := bch * inf_index]
  data[, bsa := bsa * inf_index]
  data[, bsa01 := bsa01 * inf_index]
  data[, bho := bho * inf_index]
  data[, bdict02 := bdict02 * inf_index]
  data[, bdioa := bdioa * inf_index]
  data[, bdisc := bdisc * inf_index]
  data[, bdimb := bdimb * inf_index]
  data[, bdiwi := bdiwi * inf_index]
  data[, bcrdi := bcrdi * inf_index]
  data[, bdisv := bdisv * inf_index]
  data[, buntr := buntr * inf_index]
  data[, bunct := bunct * inf_index]
  data[, boawr := boawr * inf_index]
  data[, boamt := boamt * inf_index]
  data[, boaht := boaht * inf_index]
  data[, bfamt := bfamt * inf_index]
  data[, bwkmt := bwkmt * inf_index]
  data[, bot := bot * inf_index]
  data[, bmana := bmana * inf_index]
  data[, bsauc := bsauc * inf_index]
  data[, bdimbwa := bdimbwa * inf_index]
  data[, bdiscwa := bdiscwa * inf_index]

  ### ASSET VARIABLES
  data[, afc := afc * inf_index]

  ### EXPENDITURE VARIABLES
  data[, xmp := xmp * inf_index]
  data[, xhc := xhc * inf_index]
  data[, xhcrt := xhcrt * inf_index]
  data[, xhcmomi := xhcmomi * inf_index]
  data[, xhcot := xhcot * inf_index]
  data[, xpp := xpp * inf_index]

  return(data)
}
