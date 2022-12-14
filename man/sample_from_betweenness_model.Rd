% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/statistica_depth_models.R
\name{sample_from_betweenness_model}
\alias{sample_from_betweenness_model}
\title{A simple statistical depth model based on betweenness depth}
\usage{
sample_from_betweenness_model(
  context,
  modus,
  scale,
  p,
  n,
  decay_type = "exp",
  ...
)
}
\arguments{
\item{context}{is the context to sample from.}

\item{modus}{is the modus of the statistical model given as a 0-1-vector
representing the attributes of the mode}

\item{scale}{is the scale parameter in the model.}

\item{p}{is the power that is applied to the outlyingness-values.}

\item{n}{is the number of samples to draw.}

\item{decay_type}{is the type of the decay function (one of "exp", "inverse"
or "pearson_vii")}

\item{...}{further parameters (e.g., degrees of freedom for the
Pearson type VII model)}
}
\value{
a sample of n objects of the context in the form of a matrix
where each row is a 0-1 vector representing the attributes of one sampled
object
}
\description{
'sample_from_betweenness_model' computes a sample of objects of
a formal context based on a simple depth-based statistical model of the form

P(X=x) = C_lambda * Gamma((lambda * (1-D^mu(x))^p)),

cf. Blocher et al. 2022, p.20, equation (1).

Here, C_lambda is a normalizing constant, mu is a given modus, lambda is a
scale parameter, Gamma is a (weakly decreasing) decay function, p is an
additional power (the parameter p was not present in Blocher et al. 2022).

The depth function D^mu used here is a simple betweennes based depth (cf.,
the R function 'compute_betweenness_depth')
), where one computes the depth values simply by counting how many points are
between the center mu. This depth function is not quasiconcave but it is
star-shaped.
}
