% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metrics.R
\name{gof}
\alias{gof}
\alias{gof.VennDrawing}
\alias{gof.VennDiagram}
\alias{gof.eulerAPE}
\alias{gof.vennjs}
\title{Goodness of fit tests for euler diagram}
\usage{
gof(obj, orig)

\method{gof}{VennDrawing}(obj, orig)

\method{gof}{VennDiagram}(obj, orig)

\method{gof}{eulerAPE}(obj, orig)

\method{gof}{vennjs}(obj, orig)
}
\arguments{
\item{obj}{A fit euler diagram}

\item{orig}{original values}
}
\value{
Returns a list of goodness-of-fit tests
}
\description{
stress and diagError
}
