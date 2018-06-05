#' growthrate
#'
#' Effects of Temperature and Body Weight on Growth Rate estimated by a third order polynomial
#' @author Matt Gargiulo
#' @param T Temperature in Celsius
#' @param a parameter
#' @param b parameter
#' @param c parameter
#' @param d parameter
#' @return G (Specific Growhth rate (unitless))
#' @source https://gauchospace.ucsb.edu/courses/pluginfile.php/2004968/mod_assign/introattachment/0/aquaculture_271-216.pdf?forcedownload=1



growthrate=function(T,a,b,c,d){
  g= (a+(b*T)+(c*(T**2))+(d*(T**3)))
  return(round(g, digits=2))
}




