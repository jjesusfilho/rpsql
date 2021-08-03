#' List connections
#'
#' @param conn Your connection
#'
#' @return Data.frame with information about current connections
#' @export
#'
#' @examples
#' \dontrun{
#' pg_list_connections(conn= conn)
#' }
pg_list_connections <- function(conn){

  DBI::dbGetQuery(conn,"select * from pg_stat_activity where datname is not null")

}
