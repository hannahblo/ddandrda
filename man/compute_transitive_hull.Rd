% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/base_partial_order.R
\name{compute_transitive_hull}
\alias{compute_transitive_hull}
\title{Compute the transitive hull of a partial order}
\usage{
compute_transitive_hull(relation_mat)
}
\arguments{
\item{relation_mat}{Rrepresents a relation matrix. Note that
has to be a squared matrix.}
}
\value{
The transitive hull of the relation matrix relation_mat
}
\description{
'compute_transitive_hull' returns a 0-1-matrix which represents the
order-pairs given by the transitivity property of a partial order
}
\examples{
relation_mat_input <- matrix(0, nrow = 5, ncol = 5)
relation_mat_input[1, 3] <- 1
relation_mat_input[2, 1] <- 1
relation_mat_input[4, 3] <- 1
compute_transitive_hull(relation_mat_input)

}
