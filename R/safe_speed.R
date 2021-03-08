#' Safe Speed
#' @author Ruoyu Wang
#'
#' @description Estimate the highest "velocity" to stop a car for a given distance for different road (with different friction coefficient)
#'
#' @param v The starting speed for calculation. After each attempt, the velocity will increase 0.1 unit. (m/s)
#' @param safe_distance The goal of braking distance. (m)
#' @param mu The friction coefficient, need to identify for different roads.
#' @param g The acceleration of gravity, a constant. (9.8 m/s)
#' @param d The technical braking distance to be calculate in the loop, the initial default is 0. (m)
#'
#' @return The highest speed estimate until the target braking distance is reached. Numeric.
#' @example
#' safe_speed(0, 0.5, 100)


safe_speed <- function(v, mu, safe_distance,
                       g = 9.8, d = 0){

  v = if(v < 0) {stop("Speed cannot be less than zero!")}
  else {v}


  safe_distance = ifelse(safe_distance < 0,
                         stop("Target Distance cannot be less than zero!"),
                         safe_distance)

  i = 1 # time tracker

  while ((d < safe_distance) && (i < 1000))  {
    d = v^2 / (2 * mu * g)
    v = v + 0.1
    i = i + 1
  }
  return(v)
}

