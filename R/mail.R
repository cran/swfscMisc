#' @export mail
#' 
#' @title Mail
#' @description Sends an email using the UNIX mail function.
#' 
#' @param to recipient of email.
#' @param subject subject line of email.
#' @param message email content.
#' @param file file to be attached.
#' 
#' @author Eric Archer \email{eric.archer@@noaa.gov>}

mail <- function(to, subject = "email from R", message = NULL, file = NULL) {
  cmd <- paste("mail -s", shQuote(subject), to)
  if(!is.null(file)) {
    cmd <- paste(cmd, "<", shQuote(file))
  } else if(!is.null(message)) {
    cmd <- paste("echo", shQuote(paste(message, collapse = "\n")), "|", cmd)
  } else cmd <- paste("echo 'email from R' |", cmd)
  
  system(cmd, wait = FALSE) 
}