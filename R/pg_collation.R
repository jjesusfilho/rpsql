#' Create non-deterministic collation
#'
#' @param conn Connection
#' @param collation_name Collation name
#' @param locale Locale. Defaults to none = "und-u"
#' @param provider Privider. Defaults to "icu". On Unix systems, you can
#'      get the available locales with the shell command locale -a
#' @param numeric Numeric ordering, sorts sequences of digits by their numeric value
#' @param case_insensitive Ignore case?
#' @param ignore_accents Ignore accents
#' @param alternate_shifted Ignore punctuation?
#' @param uppercase_first Sorts uppercase letter before lowercase letters.
#'
#' @return Dataframe
#' @export
#'
#' @examples
#' \dontrun{
#' pg_collation(conn,
#'             collation_name = "coll_phone",
#'             locale = 'en',
#'             provider = 'icu',
#'             numeric = TRUE,
#'             )
#' }
pg_collation <- function(conn,
                         collation_name = NULL,
                         locale = "und",
                         provider = "icu",
                         numeric = FALSE,
                         case_insensitive = FALSE,
                         ignore_accents = FALSE,
                         alternate_shifted = FALSE,
                         uppercase_first = FALSE
){

  kn <- ""
  ks <- ""
  kc <- ""
  ka <- ""
  kf <- ""


    locale <- paste0(locale,"-u-")


  if (isTRUE(numeric)){

    kn <- "kn-true-"

  }

  if (isTRUE(case_insensitive) & isTRUE(ignore_accents)){

    ks <- "ks-level1-"

  } else if (isTRUE(case_insensitive) & !isTRUE(ignore_accents)){

    ks <- "ks-level2-"

  } else if (!isTRUE(case_insensitive) & isTRUE(ignore_accents)){

    ks <- "ks-level1-"
    kc <- "kc-true-"

  } else {

    ks <- ks
    kc <- kc

  }

  if (isTRUE(alternate_shifted)){

    ka <- "ka-shifted"

  }

  if (isTRUE(uppercase_first)){

    kf <- "kf-upper-"
  }


  q <- glue::glue("CREATE COLLATION {collation_name} (
                     provider = {provider},
                     locale = '{locale}{ks}{kc}{kn}{ka}',
                     deterministic = 'false'
                     )") %>%
      stringr::str_remove_all("-(?=')") %>%
      glue::as_glue()


  DBI::dbExecute(conn,q)

}

