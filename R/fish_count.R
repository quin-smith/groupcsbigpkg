#' fish_count
#'
#' @param fish
#' @param hist.plot
#'
#' @return
#' @export
#'
#' @examples
fish_count <- function(fish, hist.plot = FALSE){
  # Add error if input is not a character/factor:
  if((class(fish) == "character") == FALSE){
    return("fish is not a character vector!")
  }
  fish = as.factor(fish)
  # Prepare outputs
  most_fish = names(which.max(summary(fish)))
  rarest_fish = names(which.min(summary(fish)))
  total_fish = sum(summary(fish))

  # Conditional histogram:
  if(hist.plot) {
    return(list(most = most_fish, rarest = rarest_fish, total = total_fish, fish_plot = plot(fish,
                                                                                           main = sprintf("%i Total Fish Caught", total_fish))))
  } else {
    return(list(most = most_fish, rarest = rarest_fish, total = total_fish))
    }

}
