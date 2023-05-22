## usethis namespace: start
#' @useDynLib HDLPrepro, .registration = TRUE
## usethis namespace: end
NULL

## usethis namespace: start
#' @importFrom Rcpp sourceCpp
## usethis namespace: end
NULL

## usethis namespace: start
#' @exportPattern "^[[:alpha:]]+"
## usethis namespace: end
NULL

.onUnload <- function (libpath) {
  library.dynam.unload("HDLPrepro", libpath)
}