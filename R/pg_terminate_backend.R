#' Terminate one or all connections by pid
#'
#' @param conn Connection
#' @param pid NULL, defaults to all connections to databases,
#'       except the your own, or
#'      the connection pid you want to terminate.
#' @details You can list all connections with ```pg_list_connections()```
#' @return data.frame with one row per terminated connection
#' @export
#'
#' @examples
#' \dontrun{
#' pg_terminate_backend(conn)
#' }
pg_terminate_backend <- function(conn, pid = NULL){

  if (!is.null(pid) | !is.numeric(pid)){

    stop("pid argument must be numeric or null")

  }

if (is.null(pid)){

q <- "
  SELECT pg_terminate_backend(pid)
  FROM pg_stat_activity
  WHERE pid <> pg_backend_pid()
  AND datname IS NOT NULL
  AND leader_pid IS NULL
  "

} else {

  q <- glue::glue_sql("select pg_terminate_backend({pid})",.con = conn)

}

DBI::dbGetQuery(conn,q)

}
