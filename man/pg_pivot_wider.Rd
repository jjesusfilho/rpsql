% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pg_pivot_wider.R
\name{pg_pivot_wider}
\alias{pg_pivot_wider}
\title{Converts PostgreSQL table from long to wide format}
\usage{
pg_pivot_wider(conn, tbl, new_tbl, id_cols = NULL, names_from, values_from)
}
\arguments{
\item{conn}{Connection}

\item{tbl}{Table}

\item{new_tbl}{Optional. It creates a new table in the database
instead of importing it to R}

\item{id_cols}{String vector with the name of columns to be used as identities}

\item{names_from}{Column whose values will become variables}

\item{values_from}{Column whose values will populate the new variables}
}
\value{
Tibble or a new table on the database
}
\description{
Converts PostgreSQL table from long to wide format
}
