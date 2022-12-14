% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/definition_formal_context.R
\name{compute_nominal_scaling_vec}
\alias{compute_nominal_scaling_vec}
\title{Computes the formal context for nominal data by the use of nominal scaling}
\usage{
compute_nominal_scaling_vec(data_values, add_column_name = NULL)
}
\arguments{
\item{data_values}{(vector): for each observation one factor value}

\item{add_column_name}{(Null, char): a further definition for the column
names}
}
\value{
dataframe representing the crosstable/formal context
}
\description{
'compute_nominal_scaling_vec' computes for a set of nominal
data points the formal concept based on nominal scaling, without considering
groups
see page 42 of Ganter, B., Will, R. (2008): Formale Begriffsanalyse,
Mathematische Grundlagen, Springer
}
