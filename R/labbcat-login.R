## labbcat-login.R
##
## Log into a LaBB-CAT server (https://nzilbb.github.io/labbcat-doc/)

##Parameters
##URL of LaBB-CAT server
lc <- "https://apls.pitt.edu/labbcat"
##Environment variables (in ~/.Renviron or PATH) storing username & password
username <- "APLS_USERNAME"
pw <- "APLS_PASSWORD"

##Log in
library(nzilbb.labbcat)
if (nchar(Sys.getenv(username))==0 || nchar(Sys.getenv(pw))==0) {
  stop("Please add APLS_USERNAME and APLS_PASSWORD variables to ~/.Renviron or PATH")
}
invisible(labbcatCredentials(lc, Sys.getenv(username), Sys.getenv(pw)))
