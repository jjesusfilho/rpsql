#' List all databases or a specific one
#'
#' @param conn Connection
#' @param dbname Optional database name
#'
#' @return data.frame
#' @export
#'
#' @examples
#' \dontrun{
#' pg_l(conn)
#' }
pg_l <- function(conn,
                 dbname = NULL){


  if (is.null(dbname)){

    q <- "SELECT d.datname as name,
       pg_catalog.pg_get_userbyid(d.datdba) as owner,
       pg_catalog.pg_encoding_to_char(d.encoding) as encoding,
       d.datcollate as collate,
       d.datctype as c_type,
       pg_catalog.array_to_string(d.datacl, E'\n') AS access_privileges
FROM pg_catalog.pg_database d
ORDER BY 1"

  DBI::dbGetQuery(conn,q)

  } else {

    q <- glue::glue("SELECT d.datname as name,
       pg_catalog.pg_get_userbyid(d.datdba) as owner,
       pg_catalog.pg_encoding_to_char(d.encoding) as encoding,
       d.datcollate as collate,
       d.datctype as c_type,
       pg_catalog.array_to_string(d.datacl, E'\n') AS access_privileges
FROM pg_catalog.pg_database d
WHERE d.datname OPERATOR(pg_catalog.~) '^({dbname})$' COLLATE pg_catalog.default
ORDER BY 1")

    DBI::dbGetQuery(conn,q)


  }

}



#' List databases with addicional information
#'
#' @param conn Connection
#' @param dbname Optional database's name
#'
#' @return Dataframe
#' @export
#'
#' @examples
#' \dontrun{
#' pg_lp(conn)
#' }
pg_lp <- function(conn, dbname = NULL){


  if (is.null(dbname)){

    q <- "SELECT d.datname as name,
       pg_catalog.pg_get_userbyid(d.datdba) as owner,
       pg_catalog.pg_encoding_to_char(d.encoding) as encoding,
       d.datcollate as collate,
       d.datctype as c_type,
       pg_catalog.array_to_string(d.datacl, E'\n') AS access_privileges,
       CASE WHEN pg_catalog.has_database_privilege(d.datname, 'CONNECT')
            THEN pg_catalog.pg_size_pretty(pg_catalog.pg_database_size(d.datname))
            ELSE 'No Access'
       END as size,
       t.spcname as tablespace,
       pg_catalog.shobj_description(d.oid, 'pg_database') as description
FROM pg_catalog.pg_database d
  JOIN pg_catalog.pg_tablespace t on d.dattablespace = t.oid
ORDER BY 1"



  DBI::dbGetQuery(conn,q)

  } else {

    q <- glue::glue("SELECT d.datname as name,
       pg_catalog.pg_get_userbyid(d.datdba) as owner,
       pg_catalog.pg_encoding_to_char(d.encoding) as encoding,
       d.datcollate as collate,
       d.datctype as c_type,
       pg_catalog.array_to_string(d.datacl, E'\n') AS access_privileges,
       CASE WHEN pg_catalog.has_database_privilege(d.datname, 'CONNECT')
            THEN pg_catalog.pg_size_pretty(pg_catalog.pg_database_size(d.datname))
            ELSE 'No Access'
       END as size,
       t.spcname as tablespace,
       pg_catalog.shobj_description(d.oid, 'pg_database') as description
FROM pg_catalog.pg_database d
  JOIN pg_catalog.pg_tablespace t on d.dattablespace = t.oid
WHERE d.datname OPERATOR(pg_catalog.~) '^({dbname})$' COLLATE pg_catalog.default
ORDER BY 1")


   DBI::dbGetQuery(conn,q)


  }

}

