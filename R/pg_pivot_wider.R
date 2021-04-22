#' Converts PostgreSQL table from long to wide format
#'
#' @param conn Connection
#' @param tbl Table
#' @param new_tbl Optional. It creates a new table in the database
#'      instead of importing it to R
#' @param id_cols String vector with the name of columns to be used as identities
#' @param names_from Column whose values will become variables
#' @param values_from Column whose values will populate the new variables
#'
#' @return Tibble or a new table on the database
#' @export
#'
pg_pivot_wider <- function(conn, tbl, new_tbl, id_cols = NULL, names_from, values_from){


  q1 <- glue::glue_sql("Select distinct {`names_from`} from {`tbl`}",.con = conn)

  var <- DBI::dbGetQuery(conn,q1) %>%
         dplyr::pull(names_from)

 if (is.null(new_tbl)){
 query <-  glue::glue("sum({`values_from`}) FILTER (WHERE {`names_from`} = '{var}') as {var} ") %>%
    glue::glue_collapse(sep = ",\n") %>%
    {glue::glue("select {id_cols},\n {.} \nfrom {tbl} group by {id_cols} order by {id_cols}")}

   DBI::dbGetQuery(conn,query)

   } else {

     query <-  glue::glue("sum({`values_from`}) FILTER (WHERE {`names_from`} = '{var}') as {var} ") %>%
       glue::glue_collapse(sep = ",\n") %>%
       {glue::glue("create table {`new_tbl`}  as select {id_cols},\n {.} \nfrom {tbl} group by {id_cols} order by {id_cols}")}

     DBI::dbExecute(conn,query)

     }

}
