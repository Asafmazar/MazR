#' MazR Squared
#'
#' Computes the squared Spearman correlation between the observed and fitted values of a given model. Please do not use this as an actual measure of explained variance.
#'
#' @param model A model object.
#' @return The squared Spearman correlation between the observed and fitted values of the model.
#' @export
#' @importFrom broom augment
#' @importFrom dplyr %>%
#' @importFrom stats cor.test
#' @importFrom broom tidy
#' @examples
#' \dontrun{
#' library(stats)
#' fit <- lm(mpg ~ wt, data = mtcars)
#' mazar_squared(fit)
#' }
mazar_squared <- function(model) {
  
  # Create tidy df with observed and fitted values
  
  aug_dat <- augment(model) 
  
  # Compute spearman's correlation
  
  cor <- tidy(cor.test(aug_dat[all.vars(terms(model))[1]][[1]],
                       aug_dat$.fitted,
                       method = 'spearman', exact = F))$estimate
  
  names(cor) <- 'rho squared' # Set name as 'rho squared'
  
  return(cor^2) # return squared Spearman's correlation
}
