#' Describe schemas
#'
#' @param conn Connection
#' @param schema Optional. If you want to describe just
#'     one schema
#' @param plus Logical. Defaults to FALSE, opts to TRUE with you
#'     want information about access privileges and description
#'
#' @return data.frame with shema names and owners
#' @export
#'
#' @examples
#' \dontrun{
#' pg_dn(conn)
#' }
pg_dn <- function(conn, schema = NULL, plus = FALSE){

  if (is.null(schema)){

  if (plus == FALSE){

  DBI::dbGetQuery(conn,"
                  SELECT n.nspname AS name,
  pg_catalog.pg_get_userbyid(n.nspowner) AS owner
FROM pg_catalog.pg_namespace n
WHERE n.nspname !~ '^pg_' AND n.nspname <> 'information_schema'
ORDER BY 1")

  } else {

    DBI::dbGetQuery(conn,"
                    SELECT n.nspname AS name,
  pg_catalog.pg_get_userbyid(n.nspowner) AS owner,
  pg_catalog.array_to_string(n.nspacl, E'\n') AS access_privileges,
  pg_catalog.obj_description(n.oid, 'pg_namespace') AS description
FROM pg_catalog.pg_namespace n
WHERE n.nspname !~ '^pg_' AND n.nspname <> 'information_schema'
ORDER BY 1
")

  }

  } else {

    if (plus == FALSE){

q <- glue::glue("
    SELECT n.nspname AS name,
    pg_catalog.pg_get_userbyid(n.nspowner) AS owner
    FROM pg_catalog.pg_namespace n
    WHERE n.nspname OPERATOR(pg_catalog.~) '^({schema})$' COLLATE pg_catalog.default
    ORDER BY 1")

DBI::dbGetQuery(conn,q)

    } else {

  q <- glue::glue("
                  SELECT n.nspname AS name,
  pg_catalog.pg_get_userbyid(n.nspowner) AS owner,
  pg_catalog.array_to_string(n.nspacl, E'\n') AS access_privileges,
  pg_catalog.obj_description(n.oid, 'pg_namespace') AS description
FROM pg_catalog.pg_namespace n
WHERE n.nspname OPERATOR(pg_catalog.~) '^({schema})$' COLLATE pg_catalog.default
ORDER BY 1;

                  ")

  DBI::dbGetQuery(conn,q)


    }

  }
}


