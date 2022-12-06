#' A simple statistical depth model based on betweenness depth
#'
#' @description 'sample_from_betweenness_model' computes a sample of objects of
#'  a formal context based on a simple depth-based statistical model of the form
#'
#' P(X=x) = C_lambda * Gamma((lambda * (1-D^mu(x))^p)),
#'
#' cf. Blocher et al. 2022, p.20, equation (1).
#'
#' Here, C_lambda is a normalizing constant, mu is a given modus, lambda is a
#' scale parameter, Gamma is a (weakly decreasing) decay function, p is an
#' additional power (the parameter p was not present in Blocher et al. 2022).
#'
#' The depth function D^mu used here is a simple betweennes based depth (cf.,
#' the R function 'compute_betweenness_depth')
#' ), where one computes the depth values simply by counting how many points are
#' between the center mu. This depth function is not quasiconcave but it is
#' star-shaped.
#'
#' @param modus is the modus of the statistical model given as a 0-1-vector
#' representing the attributes of the mode
#'
#' @param context is the context to sample from.
#' @param scale is the scale parameter in the model.
#' @param p is the power that is applied to the outlyingness-values.
#' @param n is the number of samples to draw.
#' @param decay_type is the type of the decay function (one of "exp", "inverse"
#'  or "pearson_vii")
#' @param ... further parameters (e.g., degrees of freedom for the
#' Pearson type VII model)
#' @return a sample of n objects of the context in the form of a matrix
#' where each row is a 0-1 vector representing the attributes of one sampled
#' object
#' @export
sample_from_betweenness_model <- function(context, modus,
                                          scale, p, n,
                                          decay_type = "exp", ...) {
  depths <- compute_betweenness_depth(context, context, modus)
  # depths <- quasiconcave_hull(depths,context) #ACHTUNG: RAUS
  probs <- compute_probs_depth_model(depths, scale, p, decay_type, ...)
  indexs <- sample((1:nrow(context)), size = n, prob = probs, replace = TRUE)
  return(context[indexs, ])
}




#' Compute sampling probabilities of a simple statistical depth model
#' based on a decay function Gamma.
#'
#' @description 'compute_probs_depth_model computes the sampling probabilities
#' of objects of a formal context based on a decay function Gamma.
#'
#' @param depths a vector with depth-values. The depth-values are assumed to be
#'  between zero and one.
#'
#' @param scale is the scale parameter in the model.
#' @param p is the power that is applied to the outlyingness-values.
#' @param decay_type is the type of the decay function (one of "exp", "inverse"
#' or "pearson_vii")
#' @param ... further parameters (e.g., degrees of freedom for the
#' Pearson type VII model)
#' @return a vector of the same length as the length of dpths which gives the
#' sampling probabilities according to the model.
#' @examples
#' depths <- seq(0.0000000001, 0.99, length.out = 1000)
#' scale <- 0.05
#' probs1 <- compute_probs_depth_model(
#'   depths = depths, scale = scale * 1.5,
#'   p = 1, decay_type = "exp"
#' )
#' probs2 <- compute_probs_depth_model(
#'   depths = depths, scale = scale, p = 2,
#'   decay_type = "exp"
#' )
#' probs3 <- compute_probs_depth_model(
#'   depths = depths, scale = scale, p = 0.5,
#'   decay_type = "inverse"
#' )
#' probs4 <- compute_probs_depth_model(
#'   depths = depths, scale = scale, p = 0.1,
#'   decay_type = "inverse"
#' )
#' probs5 <- compute_probs_depth_model(
#'   depths = depths, scale = scale, p = 1,
#'   df = 1, decay_type = "pearson_vii"
#' )
#' probs6 <- compute_probs_depth_model(
#'   depths = depths, scale = scale, p = 1,
#'   df = 10, decay_type = "pearson_vii"
#' )
#' plot(depths, probs1,
#'   type = "l", lwd = 2, ylim = c(
#'     min(c(
#'       probs1, probs2, probs3, probs4,
#'       probs5, probs6
#'     )),
#'     max(c(
#'       probs1, probs2, probs3, probs4,
#'       probs5, probs6
#'     ))
#'   ),
#'   main = "Different decay functions", xlab = "depth values",
#'   ylab = "sampling probabilities"
#' )
#' lines(depths, probs2, col = "grey", lwd = 2)
#' lines(depths, probs3, col = "blue", lwd = 2)
#' lines(depths, probs4, col = "cyan", lwd = 2)
#' lines(depths, probs5, col = "red", lwd = 2)
#' lines(depths, probs6, col = "pink", lwd = 2)
#' legend(x = 0, y = 0.015, lwd = rep(2, 6), col = c(
#'   "black", "grey", "blue", "cyan", "red",
#'   "pink"
#' ), lty = rep(1, 6), legend = c(
#'   "exp (p=1)", "exp (p=2)", "inverse (p=0.5)",
#'   "inverse (p=0.1)", "Pearson type VII (df=1)",
#'   "Pearson type VII (df=10)"
#' ))
#' @export
compute_probs_depth_model <- function(depths, scale, p, decay_type = "exp",
                                      ...) {
  if (decay_type == "exp") {
    probs <- exp(-(1 / scale) * (1 - depths)^p)
  } else if (decay_type == "inverse") {
    # note that for this model scale does not play any role
    if (any(depths >= 1)) {
      print("Do not do this
                              (computing a trivial probability vector.
                              Note that there are depth values >=1 ! and
                              you use the inverse decay function ")
      return(NULL)
    }
    probs <- 1 / ((1 - depths)^p) - 1
  } else if (decay_type == "pearson_vii") {
    depths <- depths
    probs <- PearsonDS::dpearsonVII(((1 - depths)^p),
      location = 0, scale = scale,
      log = FALSE, ...
    )
  }
  probs <- probs / sum(probs)
  return(probs)
}

#' Sampling form an explicitly given depth model
#'
#' @description 'sample_from_expl_depth_model' samples a number of
#' objects from a formal context according to an explicitly given model where in
#' a first step the sampling probabilities of all objects of the context are
#' calculated in according to a depth-based statistical model of the form
#'
#'
#' P(X=x) = C_lambda * Gamma((lambda * (1-D^mu(x))^p)),
#'
#'
#' cf. Blocher et al. 2022, p.20, equation (1).
#' Here, C_lambda is a normalizing constant, mu is a given modus, lambda is a
#' scale parameter, Gamma is a (weakly decreasing) decay function, p is an
#' additional power (the parameter p was not present in Blocher et al. 2022).
#'
#' @param context is the underlying formal context. (Objects that are not
#' elements of the context will not be sampled at all.)
#'
#' @param modus is the modus of the statistical model.
#'
#' @param scale is a scale parameter. Concretely scale = 1/ lambda, see
#' above. The choice of sclae instaed of lambda as a scale parameter is only for
#' convenience. Higher values of scale correspond to a higher variability in the
#' model.
#'
#' @param p is an additional power which allows for further flexibility of the
#' shape of the decay of the probabilities as the depth values get smaller.
#'
#' @param n is the number of samples to draw.
#'
#' @param decay_type is the type of the decay function, more concretely one of
#'  "exp", "inverse" or "pearsonvii".
#'
#' @param depth_function is the used epth function.
#'
#' @param quasiconcavize is by default set to FALSE. If set to TRUE,
#' depth-values that are returned by the depth function are quasiconcavized
#' before the model equation (see above) is applied.
#'
#' @param ... further parameters (e.g., degrees of freedom for the
#' Pearson type VII model)
#'
#' @export
sample_from_expl_depth_model <- function(context, modus,
                                         scale, p, n, decay_type = "exp",
                                         depth_function, quasiconcavize = FALSE,
                                         ...) {
  depths <- depth_function(context, context, modus, ...)
  depths <<- depths
  if (quasiconcavize) {
    depths <- compute_quasiconcave_hull(depths, context)
  }
  probs <- compute_probs_depth_model(depths, scale, p, decay_type, ...)
  indexs <- sample((1:nrow(context)), size = n, prob = probs, replace = TRUE)
  return(context[indexs, ])
}
