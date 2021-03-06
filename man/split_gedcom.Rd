% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{split_gedcom}
\alias{split_gedcom}
\title{Split a tidyged object into two}
\usage{
split_gedcom(tg, xrefs)
}
\arguments{
\item{tg}{A tidyged object to split.}

\item{xrefs}{A vector of xrefs to put into the new tidyged object.}
}
\value{
A new tidyged object containing the xrefs specified. It will also have the same
header and submitter information as the input tidyged object.
}
\description{
Split a tidyged object into two
}
