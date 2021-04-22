#' Describe schemas
#'
#' @param conn Connection
#' @param schema Optional. If you want to describe just
#'     one schema
#'
#' @return data.frame with shema names and owners
#' @export
#'
#' @examples
#' \dontrun{
#' pg_dn(conn)
#' }
pg_dn <- function(conn, schema = NULL){

  if (is.null(schema)){

  DBI::dbGetQuery(conn,"
                  SELECT n.nspname AS name,
  pg_catalog.pg_get_userbyid(n.nspowner) AS owner
FROM pg_catalog.pg_namespace n
WHERE n.nspname !~ '^pg_' AND n.nspname <> 'information_schema'
ORDER BY 1")

  } else {

q <- glue::glue("
    SELECT n.nspname AS name,
    pg_catalog.pg_get_userbyid(n.nspowner) AS owner
    FROM pg_catalog.pg_namespace n
    WHERE n.nspname OPERATOR(pg_catalog.~) '^({schema})$' COLLATE pg_catalog.default
    ORDER BY 1")

DBI::dbGetQuery(conn,q)
  }
}



#' Describe schemas plus
#'
#' @param conn Connections
#' @param schema Optional. Schema name
#'
#' @return Data.frame with 4 columns: name, owner, access_privileges,
#'     and description
#' @export
#'
#' @examples
#' \dontrun{
#' pg_dnp(conn)
#' }
pg_dnp <- function(conn, schema=NULL){

  if (is.null(schema)){

    DBI::dbGetQuery(conn,"
                    SELECT n.nspname AS name,
  pg_catalog.pg_get_userbyid(n.nspowner) AS owner,
  pg_catalog.array_to_string(n.nspacl, E'\n') AS access_privileges,
  pg_catalog.obj_description(n.oid, 'pg_namespace') AS description
FROM pg_catalog.pg_namespace n
WHERE n.nspname !~ '^pg_' AND n.nspname <> 'information_schema'
ORDER BY 1
                    "
                    )

  } else {


q <- glue::glue("
    SELECT n.nspname AS name,
    pg_catalog.pg_get_userbyid(n.nspowner) AS owner,
    pg_catalog.array_to_string(n.nspacl, E'\n') AS access_privileges,
    pg_catalog.obj_description(n.oid, 'pg_namespace') AS description
    FROM pg_catalog.pg_namespace n
    WHERE n.nspname OPERATOR(pg_catalog.~) '^({schema})$' COLLATE pg_catalog.default
    ORDER BY 1")

DBI::dbGetQuery(conn,q)

  }




}
