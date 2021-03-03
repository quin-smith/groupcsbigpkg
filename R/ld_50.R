#' ld_50
#'
#' @param lb_bodywt
#' @param cups
#' @param mg_caf
#' @param caf_lvl
#'
#' @description Returns the amount of cups of coffee it would take to reach a lethal dose of caffeine overdose given input weight in lbs.  Based on the LD 50 of caffeine (150 mg/kg body weight).
#' @return Output is a numeric.
#' @export
#'
#' @examples
ld_50 <- function(lb_bodywt, cups = 0, mg_caf = 95, caf_lvl = 0) {

  #error testing
  if (lb_bodywt <= 0)
    return("Please enter a positive number for weight")

  #add a message for just one cup of coffee
  if (lb_bodywt == 1)
    return("1 cup will kill you")

  #pounds to kg conversion
  kg_bodywt = lb_bodywt * 0.4535924

  #while loop to do calculation and return message
  while (caf_lvl < 150) {
    cups = cups + 1
    caf_lvl = (cups * mg_caf) / kg_bodywt
    if (caf_lvl >= 150) {
      cat(c(cups, "cups will kill you"))
    }
  }
}
