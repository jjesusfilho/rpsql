#' List tables or table columns
#'
#' @param conn Connection
#' @param tbl Character. If informed, describes table columns
#' @param plus Additional information?
#'
#' @return Tibble
#' @export
#'
#' @examples
#' \dontrun{
#' pg_d(conn)
#' }
pg_d <- function(conn,tbl = NULL, plus = FALSE){

 if (is.null(tbl)){

   if (plus){

     q <- "SELECT n.nspname as schema,
  c.relname as name,
  CASE c.relkind WHEN 'r' THEN 'table' WHEN 'v' THEN 'view' WHEN 'm' THEN 'materialized view' WHEN 'i' THEN 'index' WHEN 'S' THEN 'sequence' WHEN 's' THEN 'special' WHEN 'f' THEN 'foreign table' WHEN 'p' THEN 'partitioned table' WHEN 'I' THEN 'partitioned index' END as type,
  pg_catalog.pg_get_userbyid(c.relowner) as owner,
  CASE c.relpersistence WHEN 'p' THEN 'permanent' WHEN 't' THEN 'temporary' WHEN 'u' THEN 'unlogged' END as persistence,
  pg_catalog.pg_size_pretty(pg_catalog.pg_table_size(c.oid)) as size,
  pg_catalog.obj_description(c.oid, 'pg_class') as description
FROM pg_catalog.pg_class c
     LEFT JOIN pg_catalog.pg_namespace n ON n.oid = c.relnamespace
WHERE c.relkind IN ('r','p','v','m','S','f','')
      AND n.nspname <> 'pg_catalog'
      AND n.nspname <> 'information_schema'
      AND n.nspname !~ '^pg_toast'
  AND pg_catalog.pg_table_is_visible(c.oid)
ORDER BY 1,2"

     df <- DBI::dbGetQuery(conn, q)

   } else {

   q <- "SELECT n.nspname as schema,
  c.relname as name,
  CASE c.relkind WHEN 'r' THEN 'table' WHEN 'v' THEN 'view' WHEN 'm' THEN 'materialized view' WHEN 'i' THEN 'index' WHEN 'S' THEN 'sequence' WHEN 's' THEN 'special' WHEN 'f' THEN 'foreign table' WHEN 'p' THEN 'partitioned table' WHEN 'I' THEN 'partitioned index' END as type,
  pg_catalog.pg_get_userbyid(c.relowner) as owner
FROM pg_catalog.pg_class c
     LEFT JOIN pg_catalog.pg_namespace n ON n.oid = c.relnamespace
WHERE c.relkind IN ('r','p','v','m','S','f','')
      AND n.nspname <> 'pg_catalog'
      AND n.nspname <> 'information_schema'
      AND n.nspname !~ '^pg_toast'
  AND pg_catalog.pg_table_is_visible(c.oid)
ORDER BY 1,2"

df <-  DBI::dbGetQuery(conn,q)
   }


 } else {

   if (plus){


  q <- glue::glue_sql("
    with cte1 as (
select att.attname as column,
case att.attstorage
when 'p' then 'plain'
when 'm' then 'main'
when 'e' then 'external'
when 'x' then 'extended'
end as attstorage,
attstattarget as stats_target
from pg_attribute att
join pg_class tbl on tbl.oid = att.attrelid
join pg_namespace ns on tbl.relnamespace = ns.oid
where tbl.relname = {tbl}
and not att.attisdropped
)

    select
     column_name as column,
     data_type as type,
     collation_name as collation,
     is_nullable as nullable,
     column_default as default,
     attstorage as storage,
     stats_target,
     description
     from pg_catalog.pg_class c,
     information_schema.columns as i
     inner join cte1 on cte1.column = i.column_name
     inner join lateral (select col_description(c.oid, i.ordinal_position) description)  d on TRUE
     where c.relname = {tbl}
     and i.table_name = {tbl}
", .con = conn)


 df <- DBI::dbGetQuery(conn, q)

   } else {

  q <- glue::glue_sql("
     SELECT
          column_name as column,
          data_type as type,
          collation_name as collation,
          is_nullable as nullable,
          column_default as default
     FROM information_schema.columns
     WHERE table_name = {tbl}
                      ", .con = conn)

  df <- DBI::dbGetQuery(conn, q)

   }

 }

  return(df)
 }
