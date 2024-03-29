% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pg_select.R
\name{pg_select}
\alias{pg_select}
\title{Select one or more columns from a table}
\usage{
pg_select(conn, tbl, ...)
}
\arguments{
\item{conn}{Connection}

\item{tbl}{Table}

\item{...}{Bare columns to be selected. Defaults to all. You can also
provide "*" to select all columns.}
}
\value{
tibble with selected columns
}
\description{
Select one or more columns from a table
}
\examples{
\dontrun{
pg_select(conn, mtcars, mpg, drat)
pg_select(conn, mtcars,"mpg","drat")
pg_select(conn, mtcars)
pg_select(conn, mtcars,"*")
}
}
