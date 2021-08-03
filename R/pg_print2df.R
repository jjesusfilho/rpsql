pg_print2df <- function(x){

  nomes <- stringr::str_extract(x,".+\n")   %>%
    stringr::str_split("\\|") %>%
    unlist() %>%
    stringr::str_trim()

  dt <- stringr::str_remove(x,".+\n")


  s <- stringr::str_split(dt,"\n")[[1]]

  s <- s[-1]

  purrr::map_dfr(s,~{

    .x %>%
      stringr::str_split("\\|") %>%
      unlist() %>%
      stringr::str_trim() %>%
      stats::setNames(nomes)

  })

}
