% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ufg_depth.R
\name{test_ufree_porder}
\alias{test_ufree_porder}
\title{Test if the subcontext fc given by partial orders and the scaling method
"edge_non_edge_porder" is a union-free premise}
\usage{
test_ufree_porder(fc_sub, ufg_candidate)
}
\arguments{
\item{fc_sub}{(matrix) Formal context describing a subset of partial orders
using the "edge_non_edge_porder" scaling method}

\item{ufg_candidate}{(list of matrices) a list of partial orders given by
indices matrices (based on the same items) which should be checked weatther
they define a union-free premise. Note that length of ufg_candidate must
equl dim(fc_sub)}
}
\value{
logical value stating wether fc_sub is a  union-free premise
}
\description{
This function test if a premise of partial orders is  union-free
based on containing. This function should not be executed without executing
test_generic_porder bevor and getting a TRUE value
}
