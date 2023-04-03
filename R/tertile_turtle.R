#' Tertile-turtle function
#'
#' Returns a turtle (and, optionally, a tertile)
#'
#' @param x a numeric vector (optional).
#' @return An ASCII turtle. Optionally, returns an ordered factor corresponding to the tertile split of a given vector.
#' @export
#' @importFrom stats quantile
#' @importFrom pewmethods fct_case_when
#' @examples
#' \dontrun{
#' turtle();
#' turtle(1:10, tertile = T);
#' }

tertile_turtle <- function(x = NULL, tertile = F) {
  
  cat("  _____     ____\n /      \\  |  o | \n|        |/ ___\\| \n|_________/     \n|_|_| |_|_|\n")
  
  if (!(is.null(x)) & tertile == T) {
    quant <- quantile(x, c(0.33, 0.66))
    
    return(pewmethods::fct_case_when(
      x < quant[1] ~ 'Low',
      x < quant[2] ~ 'Medium',
      x >= quant[2] ~ 'High'
    ))}
}
