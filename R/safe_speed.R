# Ruoyu Wang
# Assignment 3 part 1
# Using "while" loop to create a function

# Estimate the highest "velocity" to stop a car for a given distance for different road (with different friction coefficient)

# Variables
# v, in m/s, the starting speed for calculation. After each attempt, the velocity will increase 0.1 unit
# safe_distance, in m, the goal of braking distance
# mu, no unit, the friction coefficient, need to identify for different roads.
# g, 9.8, in m/s2, acceleration of gravity, a constant.
# d, in m, the technical braking distance to be calculate in the loop, the initial default is 0


safe_speed <- function(v, mu, safe_distance, 
                       g = 9.8, d = 0){
  
  v = if(v < 0) {stop("Speed cannot be less than zero!")} else {v}
          
  
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

