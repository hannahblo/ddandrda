% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fca_poset.R
\name{compute_poset_scaling}
\alias{compute_poset_scaling}
\title{Computes a formal context that presents a data matrix as a matrix
of total orders}
\usage{
compute_poset_scaling(x, remove_full_columns = FALSE, complemented = FALSE)
}
\arguments{
\item{x}{A matrix. Every row represents one total order}

\item{remove_full_columns}{(logical): If True, then resulting columns that
consist only of ones, are removed}

\item{complemented}{(logical)  If True, then also the attributes
NOT (x_i^k <= x_i^l , k,l in 1, .. n) are included}
}
\value{
matrix where row i represents the incidence matrix that corresponds
to the i-th row of the data matrix X (Note that e.g. the entry 1,2 of the
i-th row corresponds to the attribute x_i^2 <= x_i^1
}
\description{
Given a matrix x with n columns where every row is one data point,
computes for every data point x_i the incidence_matrix
x_i^k <= x_i^l , k,l in 1, .. n
}
