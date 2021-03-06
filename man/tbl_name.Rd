% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/tbl_name.R
\name{tbl_name}
\alias{tbl_name}
\title{Create a temporary table name.}
\usage{
tbl_name(name = "temp", tag = as.integer(Sys.time()))
}
\arguments{
\item{name}{base name for table}

\item{tag}{string to append to table base name}
}
\value{
character vector containing a table name.
}
\description{
Create a temporary table name.
}

