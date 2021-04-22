#' List functions
#'
#' @param conn Connection
#' @param f Optional. Function name
#'
#' @return Data.frame
#' @export
#'
#' @examples
#' \dontrun{
#' pg_df(conn)
#' }
pg_df <- function(conn, f = NULL){

   if (is.null(f)){

      q <- "SELECT n.nspname as schema,
  p.proname as name,
  pg_catalog.pg_get_function_result(p.oid) as Result_data_type,
  pg_catalog.pg_get_function_arguments(p.oid) as argument_data_types,
 CASE p.prokind
  WHEN 'a' THEN 'agg'
  WHEN 'w' THEN 'window'
  WHEN 'p' THEN 'proc'
  ELSE 'func'
 END as type
FROM pg_catalog.pg_proc p
     LEFT JOIN pg_catalog.pg_namespace n ON n.oid = p.pronamespace
WHERE pg_catalog.pg_function_is_visible(p.oid)
      AND n.nspname <> 'pg_catalog'
      AND n.nspname <> 'information_schema'
ORDER BY 1, 2, 4"

      DBI::dbGetQuery(conn,q)

   } else {

      q <- glue::glue("SELECT n.nspname as schema,
  p.proname as name,
  pg_catalog.pg_get_function_result(p.oid) as result_data_type,
  pg_catalog.pg_get_function_arguments(p.oid) as argument_data_type,
 CASE p.prokind
  WHEN 'a' THEN 'agg'
  WHEN 'w' THEN 'window'
  WHEN 'p' THEN 'proc'
  ELSE 'func'
 END as type
FROM pg_catalog.pg_proc p
     LEFT JOIN pg_catalog.pg_namespace n ON n.oid = p.pronamespace
WHERE p.proname OPERATOR(pg_catalog.~) '^({f})$' COLLATE pg_catalog.default
  AND pg_catalog.pg_function_is_visible(p.oid)
ORDER BY 1, 2, 4")

      DBI::dbGetQuery(conn,q)

   }

}




#' List text search configuration
#'
#' @param conn Connection
#' @param name Optional. Configuration name
#'
#' @return Tibble with columns schema, name, and descriptio
#' @export
#'
#' @examples
#' \dontrun{
#' dF(conn)
#' }
pg_dF <- function(conn, name = NULL){

if (is.null(name)){

q <- "
    SELECT
   n.nspname as schema,
   c.cfgname as name,
   pg_catalog.obj_description(c.oid, 'pg_ts_config') as description
FROM pg_catalog.pg_ts_config c
LEFT JOIN pg_catalog.pg_namespace n ON n.oid = c.cfgnamespace
WHERE pg_catalog.pg_ts_config_is_visible(c.oid)
ORDER BY 1, 2
                  "
 DBI::dbGetQuery(conn,q)

} else {

  q <- glue::glue("SELECT

   n.nspname as schema,
   c.cfgname as name,
   pg_catalog.obj_description(c.oid, 'pg_ts_config') as description
FROM pg_catalog.pg_ts_config c
LEFT JOIN pg_catalog.pg_namespace n ON n.oid = c.cfgnamespace
WHERE c.cfgname OPERATOR(pg_catalog.~) '^({name})$' COLLATE pg_catalog.default
  AND pg_catalog.pg_ts_config_is_visible(c.oid)
ORDER BY 1, 2")

  DBI::dbGetQuery(conn,q)



}

}




