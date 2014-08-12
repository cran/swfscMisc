#' @rdname odds

invLogOdds <- function(x) exp(x) / (1 + exp(x))