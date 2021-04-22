#' List of aggregate functions
#'
#' @param conn Connection
#'
#' @return Dataframe with aggregate functions' names
#' @export
#'
#' @examples
#' \dontrun{
#' pg_da(conn)
#' }
pg_da <- function(conn){


  q <- "SELECT n.nspname as schema,
  p.proname AS name,
  pg_catalog.format_type(p.prorettype, NULL) AS result_data_type,
  CASE WHEN p.pronargs = 0
    THEN CAST('*' AS pg_catalog.text)
    ELSE pg_catalog.pg_get_function_arguments(p.oid)
  END AS argument_data_types,
  pg_catalog.obj_description(p.oid, 'pg_proc') as description
FROM pg_catalog.pg_proc p
     LEFT JOIN pg_catalog.pg_namespace n ON n.oid = p.pronamespace
WHERE p.prokind = 'a'
      AND n.nspname <> 'pg_catalog'
      AND n.nspname <> 'information_schema'
  AND pg_catalog.pg_function_is_visible(p.oid)
ORDER BY 1, 2, 4"

  DBI::dbGetQuery(conn,q)

}



#' List access methods
#'
#' @param conn Connection
#'
#' @return Dataframe with access methods
#' @export
#'
#' @examples
#' \dontrun{
#' pg_dA(conn)
#' }
pg_dA <- function(conn){

  q <- "SELECT amname AS name,
  CASE amtype WHEN 'i' THEN 'Index' WHEN 't' THEN 'Table' END AS type
FROM pg_catalog.pg_am
ORDER BY 1"

  DBI::dbGetQuery(conn,q)

}




#' List access methods with additional information
#'
#' @param conn Connection
#'
#' @return Dataframe with access methods
#' @export
#'
#' @examples
#' \dontrun{
#' pg_dAp(conn)
#' }
pg_dAp <- function(conn){


  q <- "SELECT amname AS name,
  CASE amtype WHEN 'i' THEN 'Index' WHEN 't' THEN 'Table' END AS type,
  amhandler AS handler,
  pg_catalog.obj_description(oid, 'pg_am') AS description
FROM pg_catalog.pg_am
ORDER BY"

  DBI::dbGetQuery(conn,q)


}


#' List Operarator classes
#'
#' @param conn Connection
#'
#' @return Dataframe with operators' classes
#' @export
#'
#' @examples
#' \dontrun{
#' pg_dAc(conn)
#' }
#'
pg_dAc <- function(conn){
  q <- "
SELECT
am.amname AS am,
pg_catalog.format_type(c.opcintype, NULL) AS input_type,
CASE  WHEN c.opckeytype <> 0 AND c.opckeytype <> c.opcintype
THEN pg_catalog.format_type(c.opckeytype, NULL)
ELSE NULL
END AS storage_type,
CASE
WHEN pg_catalog.pg_opclass_is_visible(c.oid)
THEN pg_catalog.format('%I', c.opcname)
ELSE pg_catalog.format('%I.%I', n.nspname, c.opcname)
END AS operator_class,
(CASE WHEN c.opcdefault
 THEN 'yes'
 ELSE 'no'
 END) AS default
FROM pg_catalog.pg_opclass c
LEFT JOIN pg_catalog.pg_am am on am.oid = c.opcmethod
LEFT JOIN pg_catalog.pg_namespace n ON n.oid = c.opcnamespace
LEFT JOIN pg_catalog.pg_type t ON t.oid = c.opcintype
LEFT JOIN pg_catalog.pg_namespace tn ON tn.oid = t.typnamespace
ORDER BY 1, 2, 4"

  DBI::dbGetQuery(conn)

}




#' List operators' classes with additional information
#'
#' @param conn Connection
#'
#' @return Dataframe
#' @export
#'
#' @examples
#' \dontrun{
#' pg_dAcp(conn)
#' }
pg_dAcp <- function(conn){


  q <- "SELECT
  am.amname AS amo,
  pg_catalog.format_type(c.opcintype, NULL) AS input_type,
  CASE
    WHEN c.opckeytype <> 0 AND c.opckeytype <> c.opcintype
    THEN pg_catalog.format_type(c.opckeytype, NULL)
    ELSE NULL
  END AS storage_type,
  CASE
    WHEN pg_catalog.pg_opclass_is_visible(c.oid)
    THEN pg_catalog.format('%I', c.opcname)
    ELSE pg_catalog.format('%I.%I', n.nspname, c.opcname)
  END AS operator_class,
  (CASE WHEN c.opcdefault
    THEN 'yes'
    ELSE 'no'
  END) AS default,
  CASE
    WHEN pg_catalog.pg_opfamily_is_visible(of.oid)
    THEN pg_catalog.format('%I', of.opfname)
    ELSE pg_catalog.format('%I.%I', ofn.nspname, of.opfname)
  END AS operator_family,
 pg_catalog.pg_get_userbyid(c.opcowner) AS owner

FROM pg_catalog.pg_opclass c
  LEFT JOIN pg_catalog.pg_am am on am.oid = c.opcmethod
  LEFT JOIN pg_catalog.pg_namespace n ON n.oid = c.opcnamespace
  LEFT JOIN pg_catalog.pg_type t ON t.oid = c.opcintype
  LEFT JOIN pg_catalog.pg_namespace tn ON tn.oid = t.typnamespace
  LEFT JOIN pg_catalog.pg_opfamily of ON of.oid = c.opcfamily
  LEFT JOIN pg_catalog.pg_namespace ofn ON ofn.oid = of.opfnamespace
ORDER BY 1, 2, 4"


  DBI::dbGetQuery(conn,q)

}





