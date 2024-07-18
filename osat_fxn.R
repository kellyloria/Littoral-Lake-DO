### function to estimate oxygen saturation. 
# From Garcia and Gordon 1992 L&O.  Takes temp in deg C and bp in mm Hg. 
# Define the function to calculate oxygen saturation
osat <- function(temp, bp) {
  sato <- (exp(2.00907 + 3.22014 * (log((298.15 - temp) / (273.15 + temp))) + 
                 4.0501 * (log((298.15 - temp) / (273.15 + temp))) ^ 2 + 
                 4.94457 * (log((298.15 - temp) / (273.15 + temp))) ^ 3 - 
                 0.256847 * (log((298.15 - temp) / (273.15 + temp))) ^ 4 + 
                 3.88767 * (log((298.15 - temp) / (273.15 + temp))) ^ 5)) * 
    1.4276 * bp / 760
  return(sato)
}
