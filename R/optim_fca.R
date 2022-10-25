compute_context_all_porders <- function(q, names=(1:q),complemented) {
  perms <- gtools::permutations(q, q)
 colnames(perms) <- names
 context <- ranking_scaling(perms,
                            remove_full_columns = FALSE,
                            complemented = complemented
 )

return(context)}

return_eins <- function(...) {
  1
}



sample_concept <- function(context, steps = 10000, f) {
  maximum <- -Inf
  m <- dim(context)[1]
  n <- dim(context)[2]
  top_attr <- operator_closure_attr_input(rep(0, n), context)
  bottom_obj <- operator_closure_obj_input(rep(0, m), context)

  if (stats::runif(1) > 0.5) {
    a <- rep(1, m)
    b <- top_attr
  } else {
    b <- rep(1, n)
    a <- bottom_obj
  }

  for (i in (1:steps)) {
    if (stats::runif(1) > 0.5) {
      l <- sample((1:n), size = 1)
      bb <- b
      bb[l] <- 1
      aa <- calculate_phi(bb, context)
      bb <- calculate_psi(aa, context)
      alpha <- g_ext(aa, a, context) * n / g_int(b, bb, context) / m
      if (is.na(alpha)) {
        alpha <- 0
      }
    } else {
      o <- sample((1:m), size = 1)
      aa <- a
      aa[o] <- 1
      bb <- calculate_psi(aa, context)
      aa <- calculate_phi(bb, context)
      alpha <- g_int(bb, b, context) * m / g_ext(a, aa, context) / n
      if (is.na(alpha)) {
        alpha <- 0
      }
    }
    #
    if (f(b, context) > maximum) {
      b_maximum <- b
    }
    maximum <- max(maximum, f(b, context))

    if (stats::runif(1) < alpha * f(bb, context) / f(b, context)) {
      a <- aa
      b <- bb
    }
    # Diesen Teil brauchen wir nichtif (runif(1) >= Q) {
    # b <- b_maximum
    # a <- calculate_phi(b, context)
    # }
  }



  return(b_maximum)
}
# T=concept.gen(bg,15000,f)

g_int <- function(b, bb, context) {
  if (all(b == bb)) {
    return(0)
  }
  ans <- 0
  i <- which(bb == 1 & b == 0)
  for (k in i) {
    temp <- b
    temp[k] <- 1
    if (all(operator_closure_attr_input(temp, context) == bb)) {
      ans <- ans + 1
    }
  }
  return(ans)
}



g_ext <- function(a, aa, context) {
  g_int(a, aa, t(context))
}


# T=concept.gen(bg,10000,f)
