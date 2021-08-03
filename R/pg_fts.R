#' Creates text search configuration
#'
#' @param conn Connection
#' @param name The name of the text search configuration to be created.
#'     The name can be schema-qualified.
#' @param source_config The name of an existing text search configuration to copy.
#'     It defaults to English.
#' @param mapping A list of parsers
#'
#' @return Returns 0 if accomplished.
#' @export
#'
pg_fts <- function(conn,
                   name,
                   source_config = "english",
                   mapping = list(parsers = NULL,
                                  with = NULL)
){



  q <- glue::glue_sql("CREATE TEXT SEARCH CONFIGURATION {`name`} (copy = pg_catalog.{`source_config`})", .con = conn)

  DBI::dbExecute(conn,q)

  q <- glue::glue("ALTER TEXT SEARCH CONFIGURATION {fts_name}
                      ALTER MAPPING FOR {glue::glue_collapse(parsers, sep = ', ')}
                      WITH {glue::glue_collapse(with, sep = ', ')}
                      ")

  DBI::dbExecute(conn, q)

}
