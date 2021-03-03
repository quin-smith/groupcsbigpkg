#' @title fish_count
#'
#' @param fish
#' @param hist.plot
#'
#' @description Returns a list of the most fish caught, rarest fish caught, and total fish caught.  If hist.plot = TRUE, returns a histogram of fish counts.
#' @return Output will be a list.  If hist.plot = TRUE, will return a histogram
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
