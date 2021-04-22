#' Creates a summary table from PostgreSQL table's columns
#'
#' @param conn Connection
#' @param tbl  Table name
#' @param ...  Columns' bare-names
#' @param schema Schema name, "public" by default
#' @param max_levels Max number of levels to report for categorical
#' @param outliers Calculate outlier for numeric data? Defaults to FALSE.
#'
#' @details R exports factors as text to PostgreSQL, even though PostgreSQL has enum type
#'     which is somehow equivalent to factors.  This function shows unique values as
#'     variable levels to maximum number of 10, unless otherwise specified.
#'     As of now, it f supports only dates, texts and numbers.
#' @return Dataframe
#' @export
#'
pg_summary <- function(conn,tbl, ..., schema = "public", max_levels = 10, outliers = FALSE){

  dots <- rlang::ensyms(...) %>%
    purrr::map_chr(~rlang::as_string(.x))

  q <- glue::glue_sql(" select column_name, data_type from
information_schema.columns
where table_schema = {schema} and table_name = {tbl}
and column_name IN ({dots*})",.con = conn)

  df <- DBI::dbGetQuery(conn,q)

  q <- glue::glue_sql("select count(*) from {`tbl`}",.con = conn)

  n <- DBI::dbGetQuery(conn,q) %>%
    dplyr::pull("count") %>%
    as.integer()

  data <-  purrr::map2_dfr(df$column_name,df$data_type,~{

    ## Missing

    q <- glue::glue_sql("select count(*) from {`tbl`} where {`.x`} is null",.con = conn)

    nas <- DBI::dbGetQuery(conn,q) %>%
      dplyr::pull("count") %>%
      as.integer()

    nas_prop <- nas/n

    if (.y == "date"){

      q <- glue::glue_sql("select min({`.x`}), max({`.x`}) from {`tbl`}",.con = conn)

      min_max <- DBI::dbGetQuery(conn,q)

      db<-   tibble::tibble(column = .x,
                            n = n,
                            min_d = min_max$min,
                            max_d= min_max$max,
                            missing = nas,
                            missing_prop = nas_prop)

    } else if (.y == "text") {

      q <- glue::glue_sql("select distinct {`.x`} d from {`tbl`}",.con = conn)

      d <- DBI::dbGetQuery(conn,q) %>%
        dplyr::pull("d") %>%
        stringr::str_sort()

      if (length(d) > max_levels){

        d <- NA_character_
      }

      db<-    tibble::tibble(column = .x,
                             n = n,
                             missing = nas,
                             missing_prop = nas_prop,
                             levels = d) %>%
        tidyr::nest(levels= (levels))

    } else {

      q <- glue::glue_sql("select
                        min({`.x`}) as minimo,
                        max({`.x`}) as maximo,
                        avg({`.x`}) AS mean,
                        PERCENTILE_CONT(0.50) WITHIN GROUP (ORDER BY {`.x`} ASC) AS median
                        from {`tbl`}",.con = conn)

      sumario <- DBI::dbGetQuery(conn,q)

      if (outliers == TRUE){

        q <- glue::glue_sql("select {`.x`} as l from {`tbl`}",.con = conn)


        ol <- DBI::dbGetQuery(conn,q) %>%
          dplyr::pull("l") %>%
          graphics::boxplot(plot=FALSE) %>%
          .["out"] %>%
          unlist()




      }

      db <-  tibble::tibble(column = .x,
                            n = n,
                            min_n = sumario$minimo,
                            max_n = sumario$maximo,
                            mean = sumario$mean,
                            median = sumario$median,
                            missing = nas,
                            missing_prop = nas_prop,
                            outliers = ifelse(outliers == FALSE, NA,ol)
      ) %>%
        tidyr::nest(outliers = (outliers))

    }

    db

  })

  if (any(names(data)=="levels")){
    data %>%
      dplyr::mutate(levels = tidyr::replace_na(levels,NA_character_))
  }

  if (any(names(data)=="outliers")){
    data %>%
      dplyr::mutate(outliers = tidyr::replace_na(outliers,NA_character_))
  }

}
