#' Gets database size
#'
#' @param conn Connection
#' @param dbname Database name. If not provided, it returns the current
#'      database size.
#'
#' @return Named string with database size
#' @export
#'
#' @examples
#' \dontrun{
#' pg_db_size(conn,"postgres")
#' }
pg_db_size <- function(conn,dbname = NULL){

 if (is.null(dbname)){

   dbname <- DBI::dbGetInfo(conn)$dbname

 }

 q <- glue::glue_sql("SELECT pg_size_pretty( pg_database_size({dbname}))",.con = conn)

      DBI::dbGetQuery(conn,q) %>%
       .[[1]] %>%
        stats::setNames(dbname)


}


#' Gets table's size
#'
#' @param conn Connection
#' @param tbl String with table's name
#'
#' @return Named string with table's size
#' @export
#'
#' @examples
#' \dontrun{
#' pg_tbl_size(conn,"data")
#' }
pg_tbl_size <- function(conn, tbl = NULL){


  q <- glue::glue_sql("SELECT pg_size_pretty(pg_total_relation_size({tbl}))",.con = conn)

  DBI::dbGetQuery(conn,q) %>%
    .[[1]] %>%
    stats::setNames(tbl)

}

