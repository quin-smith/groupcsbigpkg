#' Calculate the number of years until a population reaches carrying capacity
#'
#' @param starting_stock The starting stock of a population.
#' @param r Intrinsic growth rate.
#' @param K Carrying capacity.
#' @return Years until carrying capacity is reached.
#'
#'





years_to_K <- function(starting_stock, r, K){
  # Set the initial starting stock
  prev_stock <- starting_stock
  runs <- 0
  if(starting_stock<=0){
    return("starting stock out of bounds!")
  }
  else{
    # Create an iterative for loop that'll sequentially run this...
    while(prev_stock < (K-0.1)){
      # Calculate the new stock based on logistic growth minus harvest
      new_stock <- prev_stock + (r * prev_stock) * (1 - (prev_stock/K))
      prev_stock <- new_stock
      # Calculate how many times the loop has run
      runs <- runs+1
    }
  }
  return(runs)
}
