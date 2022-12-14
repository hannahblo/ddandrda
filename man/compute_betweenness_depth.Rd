% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tukeys_depth.R
\name{compute_betweenness_depth}
\alias{compute_betweenness_depth}
\title{Simple localized depth function based on betweenness w.r.t.a prespecified
modus}
\usage{
compute_betweenness_depth(intent, context, modus)
}
\arguments{
\item{intent}{represents the envisaged object given by all its attributes
given as a 0-1 vector. It is also possible to compute depth values for
more objects. In this case the objects should be given as a matrix where
each row corresponds to one
object.}

\item{context}{the underlying context.}

\item{modus}{the object that represents the center given by the corresponding
intent of the center.}
}
\value{
the depth value(s) of the intent(s).
}
\description{
'compute_betweenness_depth' computes a simple depth function
by counting how many points are between the envisaged point and a
center (modus). This depth function is not quasiconcave but
it is star-shaped.
}
