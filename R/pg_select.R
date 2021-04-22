#' Select one or more columns from a table
#'
#' @param conn Connection
#' @param tbl  Table
#' @param ... Bare columns to be selected. Defaults to all. You can also
#'     provide "*" to select all columns.
#'
#' @return tibble with selected columns
#' @export
#'
#' @examples
#' \dontrun{
#' pg_select(conn, mtcars, mpg, drat)
#' pg_select(conn, mtcars,"mpg","drat")
#' pg_select(conn, mtcars)
#' pg_select(conn, mtcars,"*")
#' }
pg_select <- function(conn, tbl, ...){

  if (length(list(...)) == 0) {

    dots <- "*"

  } else {

      dots <- rlang::ensyms(...) %>%
      purrr::map_chr(~rlang::as_string(.x)) %>%
      stringr::str_c(collapse = ", ")
  }

  tbl <- rlang::ensym( tbl)



  query <- paste0("select ", dots, " from ", rlang::as_string(tbl))

  DBI::dbGetQuery(conn,query)

}
