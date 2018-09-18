#' Retention-Curve: water content vs. pressure head, van Genuchten
#'
#' @param psi vector of pressure heads
#' @param alpha value vG-alpha, unit: reciprocal pressure head
#' @param n value of van Genuchten parameter N
#' @param ThS n value of van Genuchten parameter Ths (saturation water content)
#' @param ThR n value of van Genuchten parameter Thr residual water content
#' @param m value of van Genuchten parameter N. Defaults 1-1/n
#'
#' @return vector of water contents
MvG.swc <- function(
  psi, #pressure head in hPa
  alpha, #MvG alpha
  n, # MvG n
  ThS,
  ThR,
  m = 1-1/n)
{
  wetness <- 1/((1 + (alpha * psi)^n))^(m)
  theta <- wetness * (ThS-ThR) +ThR
  return(theta)
}
