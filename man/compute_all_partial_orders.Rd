% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tukeys_depth.R
\name{compute_all_partial_orders}
\alias{compute_all_partial_orders}
\title{All partial orders on a set of q elements}
\usage{
compute_all_partial_orders(n_items, names = (1:n_items), complemented, list)
}
\arguments{
\item{n_items}{is the number of elements of the basic space}

\item{names}{are the names of the all-together n_items elements}

\item{complemented}{if TRUE, the orders are return in a complemented
conceptual scaling}

\item{list}{if TRUE the orders are returned as a list. Otherwise a
formal context is returned}
}
\value{
returns the set of all partial orders on a space of q elements
}
\description{
'compute_all_partial_orders' returns the set of all partial
orders on a set of n_items elements
}
