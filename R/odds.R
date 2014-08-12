#' @rdname odds
#' @export odds logOdds invOdds invLogOdds
#' 
#' @title Odds Conversion
#' @description \tabular{ll}{
#'   \code{odds} \tab converts probability to odds \cr
#'   \code{logOdds} \tab converts odds to log-odds \cr
#'   \code{invOdds} \tab converts odds to probability \cr
#'   \code{invLogOdds} \tab converts log-odds to odds \cr
#' }
#' 
#' @param x a numeric vector of probabilities (0 to 1), odds (0 to Inf), or log.odds (-Inf to Inf).
#' 
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#' 
#' @examples
#' x <- sort(runif(10))
#' odds.df <- data.frame(x = x, odds = odds(x), logOdds = logOdds(x))
#' odds.df
#' invOdds(odds.df$odds)
#' invLogOdds(odds.df$logOdds)

odds <- function(x) ifelse(x < 0 | x > 1, as.numeric(NA), x / (1 - x))