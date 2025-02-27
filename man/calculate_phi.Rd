% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fca_general.R
\name{calculate_phi}
\alias{calculate_phi}
\title{Calculates for a subset of attributes the minimal extent based on the
given context}
\usage{
calculate_phi(subset_attributes, context)
}
\arguments{
\item{subset_attributes}{(array): set of attributes}

\item{context}{(matrix): formal context which is used to calculate the extent}
}
\value{
subset (array): the smallest extent (set of objects) in the FCA
based on subset_attributes and the formal context
}
\description{
Calculates for a subset of attributes the minimal extent based on the
given context
}
