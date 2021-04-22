#' Convert PostgreSQL table to long format
#'
#' @param conn Connection.
#' @param tbl Database table.
#' @param new_tbl Optional. If you want to create a new table on the database
#'      intead of importing to R.
#' @param cols <tidy-select> Variables to pivot into long format.
#' @param names_to A string specifying the name of the column to create from
#'     the data stored in the column names of tbl.
#' @param values_to A string specifying the name of the column to create
#'     from the data stored in cell values.
#' @return tibbl or a table in the database
#' @export
#'

pg_pivot_longer <- function(conn, tbl, new_tbl = NULL, cols, names_to, values_to){

  `!!` <- rlang::`!!`

  cols <- rlang::enexpr(cols)

  q1 <- glue::glue_sql("select * from {`tbl`} limit 0",.con = conn)

  nomes <- DBI::dbGetQuery(conn,q1)

  nomes2 <- dplyr::select(nomes, !!cols)

  ids <- setdiff(names(nomes),names(nomes2))



  if (is.null(new_tbl)){

  if (ids ==""){

    query <- glue::glue_sql("select
                          unnest(array[{colunas*}]) as {`names_to`},
                          unnest(array[{`colunas`*}]) as {`values_to`}
                          from {`tbl`}
                          ", .con = conn)

    df <- DBI::dbGetQuery(conn,query)

  } else {

  query <- glue::glue_sql("select {`ids`},
                          unnest(array[{colunas*}]) as {`names_to`},
                          unnest(array[{`colunas`*}]) as {`values_to`}
                          from {`tbl`}
                          order by {`ids`}
                          ", .con = conn)

  df <- DBI::dbGetQuery(conn,query)
  }
  return(df)

  } else {

    if (ids ==""){

      query <- glue::glue_sql("create table {`new_tbl`} as
                          select
                          unnest(array[{colunas*}]) as {`names_to`},
                          unnest(array[{`colunas`*}]) as {`values_to`}
                          from {`tbl`}
                          ", .con = conn)

      DBI::dbExecute(conn,query)


    } else {

    query <- glue::glue_sql("create table {`new_tbl`} as
                          select {`ids`},
                          unnest(array[{colunas*}]) as {`names_to`},
                          unnest(array[{`colunas`*}]) as {`values_to`}
                          from {`tbl`}
                          order by {`ids`}
                          ", .con = conn)

    DBI::dbExecute(conn,query)

  }
  }
}
