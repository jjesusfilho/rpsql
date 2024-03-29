% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pg_dn.R
\name{pg_dn}
\alias{pg_dn}
\title{Describe schemas}
\usage{
pg_dn(conn, schema = NULL, plus = FALSE)
}
\arguments{
\item{conn}{Connection}

\item{schema}{Optional. If you want to describe just
one schema}

\item{plus}{Logical. Defaults to FALSE, opts to TRUE with you
want information about access privileges and description}
}
\value{
data.frame with shema names and owners
}
\description{
Describe schemas
}
\examples{
\dontrun{
pg_dn(conn)
}
}
