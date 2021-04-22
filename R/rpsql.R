#' \code{rpsql} package
#'
#' PostgreSQL psql from R
#'
#'
#' @docType package
#' @name rpsql
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    "."
  ))
}
