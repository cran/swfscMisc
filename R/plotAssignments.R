#' @title Plot assignment distributions
#' @description Plot individual assignment probability distributions. 
#' 
#' @param probs matrix or data.frame of individual assignment probabilities. 
#'   Each column represents probability of assignment to that group and rows sum 
#'   to one.
#' @param orig vector of original group assignments
#' @param type either \code{area} for stacked continuous area plot or 
#'   \code{bar} for discrete stacked bar chart. The latter is prefered for small 
#'   numbers of cases. If not specified, a bar chart will be used if all 
#'   classes have <= 30 cases.
#' @param ylab label for y-axis
#' @param plot display the plot?
#'   
#' @return the \code{ggplot} object is invisibly returned.
#' 
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#' 
#' @examples
#' n <- 40
#' probs <- abs(c(rnorm(n, 80, 10), rnorm(n, 20, 10)))
#' probs <- (probs - min(probs)) / max(probs)
#' probs <- cbind(probs, 1 - probs)
#' colnames(probs) <- NULL
#' orig <- rep(c("Group.1", "Group.2"), each = n)
#' 
#' plotAssignments(probs, orig)
#' 
#' n <- 15
#' probs <- abs(c(rnorm(n, 80, 10), rnorm(n, 20, 10)))
#' probs <- (probs - min(probs)) / max(probs)
#' probs <- cbind(probs, 1 - probs)
#' colnames(probs) <- NULL
#' orig <- rep(c("Group.1", "Group.2"), each = n)
#' 
#' plotAssignments(probs, orig)
#' 
#' @importFrom reshape2 melt
#' @importFrom ggplot2 ggplot aes_string geom_area geom_bar scale_fill_discrete 
#' @importFrom ggplot2 ylab guide_legend facet_wrap labs theme element_text element_blank
#' @export
#'
plotAssignments <- function(probs, orig, type = NULL, ylab = NULL, plot = TRUE) {
  if(is.null(colnames(probs))) colnames(probs) <- paste("Group", 1:ncol(probs), sep = ".")
  df <- data.frame(orig = orig, probs)
  i <- do.call(order, c(as.list(df), list(decreasing = TRUE)))
  df <- df[i, ]
  df$id <- 1:nrow(df)
  freq <- table(df$orig)
  df <- melt(df, id.vars = c("id", "orig"), variable.name = "pred", value.name = "prob")
  levels(df$orig) <- paste(names(freq), " (n = ", freq, ")", sep = "")
  
  type <- if(is.null(type)) {
    if(all(freq <= 30)) "bar" else "area"
  } else {
    match.arg(type, c("bar", "area"))
  }
  
  if(is.null(ylab)) ylab <- "Assignment Probability"
  
  p <- ggplot(df, aes_string("id", "prob")) +
    switch(
      type,
      area = geom_area(aes_string(fill = "pred"), stat = "identity"),
      bar = geom_bar(aes_string(fill = "pred"), stat = "identity")
    ) +
    scale_fill_discrete(guide = guide_legend(title = "Predicted")) +
    # facet_wrap(~ orig, scales = "free_x") +
    ylab(ylab) +
    theme(
      legend.position = "top",
      text = element_text(size = 14),
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      axis.ticks.x = element_blank(),
      plot.title = element_text(hjust = 0)
    )
  if(plot) print(p)
  invisible(p)
}