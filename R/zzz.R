.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste0(
    "\nThis is omicsTools version ",
    utils::packageVersion("omicsTools"),
    ".\nomicsTools is free software and comes with ABSOLUTELY NO WARRANTY",
    ".\nPlease use at your own risk."
  ))
}
