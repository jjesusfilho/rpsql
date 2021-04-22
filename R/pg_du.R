#' Lists roles and their privileges
#'
#' @param conn Connection
#' @param plus Add description?
#' @param user Optional: username
#'
#' @return tibble
#' @export
#'
pg_du <- function(conn,
                  plus = FALSE,
                  user = NULL){


  if (!is.null(user)){

   filt <- glue::glue("AND r.rolname OPERATOR(pg_catalog.~) '^({user})$' COLLATE pg_catalog.default")

  } else {

    filt <- ""
  }

  if (plus){

    q <- glue::glue("SELECT r.rolname, r.rolsuper, r.rolinherit,
  r.rolcreaterole, r.rolcreatedb, r.rolcanlogin,
  r.rolconnlimit, r.rolvaliduntil,
  ARRAY(SELECT b.rolname
        FROM pg_catalog.pg_auth_members m
        JOIN pg_catalog.pg_roles b ON (m.roleid = b.oid)
        WHERE m.member = r.oid) as memberof,
        pg_catalog.shobj_description(r.oid, 'pg_authid') AS description,
        r.rolreplication,
        r.rolbypassrls
FROM pg_catalog.pg_roles r
WHERE r.rolname !~ '^pg_'
{filt}
ORDER BY 1")

df <-   DBI::dbGetQuery(conn,q)

  } else {

    q <- glue::glue("SELECT r.rolname, r.rolsuper, r.rolinherit,
  r.rolcreaterole, r.rolcreatedb, r.rolcanlogin,
  r.rolconnlimit, r.rolvaliduntil,
  ARRAY(SELECT b.rolname
        FROM pg_catalog.pg_auth_members m
        JOIN pg_catalog.pg_roles b ON (m.roleid = b.oid)
        WHERE m.member = r.oid) as memberof,
        r.rolreplication,
        r.rolbypassrls
FROM pg_catalog.pg_roles r
WHERE r.rolname !~ '^pg_'
{filt}
ORDER BY 1")

 df<- DBI::dbGetQuery(conn,q)

  }

return(df)
}
