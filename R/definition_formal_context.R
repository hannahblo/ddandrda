#' Computes the formal context for nominal data by the use of nominal scaling
#'
#' @description 'compute_nominal_scaling_vec' computes for a set of nominal
#' data points the formal concept based on nominal scaling, without considering
#' groups
#' see page 42 of Ganter, B., Will, R. (2008): Formale Begriffsanalyse,
#' Mathematische Grundlagen, Springer
#'
#' @param data_values (vector): for each observation one factor value
#' @param add_column_name (Null, char): a further definition for the column
#' names
#'
#' @return dataframe representing the crosstable/formal context
compute_nominal_scaling_vec <- function(data_values, add_column_name = NULL) {
  attr <- sort(unique(data_values))
  length_attr <- length(attr)
  number_elements_data <- length(data_values)

  # Memory sapce
  context_logical <- array(0, c(number_elements_data, length_attr))
  column_names_context <- rep("", length_attr)

  # Looping throw all attributes
  for (k in 1:length_attr) {
    # Defining the column name
    column_names_context[k] <- paste(c(
      add_column_name, ": ",
      as.character(attr[k])
    ), collapse = "")
    # Defining the entries of the column. TRUE if value has attribute k
    context_logical[, k] <- as.integer(data_values == attr[k])
  }
  colnames(context_logical) <- column_names_context
  return(context_logical)
}





#' Computes the formal context for ordinal data by the use of ordinal scaling
#'
#' @description 'compute_ordinal_scaling_vec' computes for a set of ordinal
#' data points the formal concept based on ordinal scaling
#' see page 42 of Ganter, B., Will, R. (2008): Formale Begriffsanalyse,
#' Mathematische Grundlagen, Springer
#'
#' @param data_values (vector): for each observation one factor value
#' @param add_column_name (Null, char): a further definition for the column
#' names
#'
#' @return dataframe representing the crosstable/formal context
compute_ordinal_scaling_vec <- function(data_values, add_column_name = NULL) {
  data_values <- as.numeric(as.character(data_values))

  attr <- sort(unique(data_values))
  length_attr <- length(attr)
  number_elements_data <- length(data_values)

  # Memory space
  context_logical <- array(0, c(number_elements_data, length_attr))
  colnames_context <- rep("", length_attr)

  # Loop throw all attributes
  for (k in (1:length_attr)) {
    # Defining the column name
    colnames_context[k] <- paste(c(add_column_name, ": x<=", attr[k]),
      collapse = ""
    )
    # Defining the entries of the column. TRUE if value is smalles than attr
    context_logical[, k] <- (data_values <= attr[k])
  }

  # Changing the colnames of the produced context
  colnames(context_logical) <- colnames_context

  return(context_logical)
}





#' Computes the formal context for ordinal data by the use of dual ordinal
#' scaling
#'
#' @description 'compute_dualordinal_scal_vec' computes for a set of dual
#' ordinal data points the formal concept based on ordinal scaling. Here,
#' the scaling method used is (n,n, >=) of page 42 of
#' Ganter, B., Will, R. (2008): Formale Begriffsanalyse,
#' Mathematische Grundlagen, Springer
#'
#' @param data_values (vector): for each observation one factor value
#' @param add_column_name (Null, char): a further definition for the column
#' names
#'
#' @return dataframe representing the crosstable/formal context
compute_dualordinal_scal_vec <- function(data_values,
                                         add_column_name = NULL) {
  data_values <- as.numeric(as.character(data_values))

  attr <- sort(unique(data_values))
  lenght_attr <- length(attr)
  number_elements_data <- length(data_values)

  # Memory space
  context_logical <- array(0, c(number_elements_data, lenght_attr))
  colnames_context <- rep("", lenght_attr)

  # Loop throw all attributes
  for (k in (1:lenght_attr)) {
    # Defining the column name
    colnames_context[k] <- paste(c(add_column_name, ": x>=", attr[k]),
      collapse = ""
    )
    # Defining the entries of the column. TRUE if value is larger or equal attr
    context_logical[, k] <- (data_values >= attr[k])
  }

  # Changing the colnames of the produced context
  colnames(context_logical) <- colnames_context

  return(context_logical)
}


#' Computes the formal context of spatial observation
#'
#' @description Defining the formal concept of the geometrical formal concept
#' analysis with object points in R^2 and attributes closed half-spaces and
#' relation: point is in relation to half-space if and only if point is an
#' element of this half-space.
#' Since we want to define a redundant formal-context. It is sufficient to
#' consider only the half-spaces which bounds goes throw two different points
#' (objects)
#' REMARK: This function does NOT work when all point given by point_matrix are
#' defined upon one line (are collinear)
#' In particular we assume that all points differ
#'
#' @param point_matrix (nx2 matrix):  represents the considered points (objects)
#' @param add_column_name (logical): if column names are added
#'
#' @return  (list): context (formal context, columns: half_spaces,
#'                                           row: if point within halfspace)
#'                  indexes (describes the index of two points corresponding
#'                              to the half-space)
#'                  point_matrix (input matrix)
compute_spatial_scaling_mat <- function(point_matrix,
                                        add_column_name = TRUE) {


  number_points <- dim(point_matrix)[1]

  # Calculation of the number of half-spaces divided by two
  # We divide, because with this we can reduce the number of needed for-loops
  number_halfsp_divided_2 <- number_points * (number_points - 1) / 2
  # is always integer
  context_first_part <- array(0, c(number_points, number_halfsp_divided_2))
  context_second_part <- context_first_part

  # Memory space for index and the designations filled within the for-loops
  points_def_boundary <- array(0, c(number_halfsp_divided_2, 2))
  names_first_part <- rep("", number_halfsp_divided_2)
  names_second_part <- rep("", number_halfsp_divided_2)

  # Functioning of the three loops:
  # Each half-space is defined by its bound, which goes through two points.
  # Remark that one bound defines two half-spaces. Therefore we have defined two
  # context memories.
  # The first two loops go through all bounds. The third loop considers each
  # point and calculates (using the scalar product) if it is within one of the
  # two half-space.

  # Counting how many half-spaces we already went trough
  i <- 1
  for (k in 1:(number_points - 1)) {
    for (l in (k + 1):number_points) {

      # Saving the names of the half-spaces
      names_first_part[i] <- paste(rownames(point_matrix)[k],
                                   rownames(point_matrix)[l],
                                   "_>=0",
                                   sep = "")
      names_second_part[i] <- paste(rownames(point_matrix)[k],
                                    rownames(point_matrix)[l],
                                    "_<=0",
                                    sep = "")
      for (m in 1:number_points) {
        # Calculation of the scalar product
        v1 <- point_matrix[k, ] - point_matrix[l, ]
        v2 <- point_matrix[m, ] - point_matrix[l, ]
        s <- v1[1] * v2[2] - v1[2] * v2[1]
        # Assignment to the half-spaces
        if (s > 0) {
          context_first_part[m, i] <- 1
        }
        if (s < 0) {
          context_second_part[m, i] <- 1
        }
        if (s == 0) {
          context_second_part[m, i] <- 1
          context_first_part[m, i] <- 1
        }
      }

      # Saving for each half-space the two spaces which define the boundary
      # in an index way
      points_def_boundary[i, ] <- c(k, l)

      # Next half-space memory space
      i <- i + 1
    }
  }

  if (add_column_name == TRUE) {
    # Naming the rows and columns
    colnames(context_first_part) <- names_first_part
    colnames(context_second_part) <- names_second_part
    rownames(context_first_part) <- rownames(point_matrix)
    rownames(context_second_part) <- rownames(point_matrix)
  }



  result <- list(context = (cbind(context_first_part, context_second_part)),
                 points_def_spatial_boundary = rbind(points_def_boundary,
                                                     points_def_boundary),
                 point_matrix = point_matrix)

  return(result)

}






#' Computes the formal context based on input data and scaling method.
#'
#' @description This function computes the formal context for nominal, inter-
#' ordinal, spatial and partial order data points. Note that for each
#' observation different data types can be observed. In each case the objects of
#' the formal context are the observation.
#'
#' Tho following data types can be transferred to a formal context by this
#' method:
#' - factor obersavtion: Here the standard nominal scaling method is implemented
#' see: Ganter, B., Will, R. (2008): Formale Begriffsanalyse,
#' Mathematische Grundlagen, Springer
#' - oridnal observation: The ordinal and interordinal scaling method is
#' implemented. If not included into scaling_methods, the interordinal scaling
#' is applyed. If not, then include "ordinal" to the vector or scaling methods.
#' see: Ganter, B., Will, R. (2008): Formale Begriffsanalyse,
#' Mathematische Grundlagen, Springer
#' - spatial observation: TODO not done yet
#' - partial order observations: The attributes are either only if a relation
#' between two items exits or also that the relation does not exist. The default
#' method is the formal context with relation exists and not exists attributes.
#' If "porder_edge" is included to the vecotr of scaling_methods, then only the
#' relation exits part is computed.
#'
#' @param input_factor (NULL or vector of factor values)
#' @param input_ordinal_numeric (NULL or vector of ordinal/numeric values)
#' @param input_spatial (NULL or list of two dimensional vectors)
#' @param input_porder (NULL or list of square same sized matrices)
#' @param scaling_methods (NULL or vector containing "porder_edge" or/and
#' "ordinal")
#' @param input_check (logical)
#'
#' @return dataframe representing the crosstable/formal context
#'
#' @examples
#' attr_numeric_4 <- as.numeric(c(1.2, 1, 1.6, 2))
#' attr_nominal_4 <- as.factor(c(
#'   "factor_1", "factor_2",
#'   "factor_2", "factor_1"
#' ))
#' compute_conceptual_scaling(
#'   input_factor = attr_nominal_4,
#'   input_ordinal_numeric = attr_numeric_4
#' )
#'
#' relation_1 <- matrix(0, ncol = 4, nrow = 4)
#' diag(relation_1) <- 1
#' relation_2 <- relation_1
#' relation_2[1, c(2, 3, 4)] <- 1
#' relation_2[2, c(3, 4)] <- 1
#' relation_2[3, 4] <- 1
#' list_porder_1 <- list(relation_1, relation_2)
#' compute_conceptual_scaling(input_porder = list_porder_1)
#' compute_conceptual_scaling(
#'   input_porder = list_porder_1,
#'   scaling_methods = c("porder_edge")
#' )
#'
#' @export
compute_conceptual_scaling <- function(input_factor = NULL,
                                       input_ordinal_numeric = NULL,
                                       input_spatial = NULL,
                                       input_porder = NULL,
                                       scaling_methods = NULL,
                                       input_check = TRUE) {
  # Input check
  if (!is.logical(input_check)) {
    stop("input_check should be logical")
  }
  if (input_check) {
    check_input_ccs_1(
      input_factor = input_factor,
      input_ordinal_numeric = input_ordinal_numeric,
      input_spatial = input_spatial,
      input_porder = input_porder,
      scaling_methods = scaling_methods
    )
    check_input_ccs_2(
      input_factor = input_factor,
      input_ordinal_numeric = input_ordinal_numeric,
      input_spatial = input_spatial,
      input_porder = input_porder,
      scaling_methods = scaling_methods
    )
    check_input_ccs_3(
      input_factor = input_factor,
      input_ordinal_numeric = input_ordinal_numeric,
      input_spatial = input_spatial,
      input_porder = input_porder,
      scaling_methods = scaling_methods
    )
  }


  # Set up / Basic information. Note that one of the above input checks already
  # proved that the entries have either all the same length or are NULL
  length_values <- unique(c(
    length(input_factor),
    length(input_ordinal_numeric),
    dim(input_spatial)[1],
    length(input_porder)
  ))
  number_obj <- setdiff(length_values, c(0))

  f_context <- matrix(NA, nrow = number_obj, ncol = 0)
  # TODO
  # add the name of rows

  # Scaling of partial orders
  if (!is.null(input_porder)) {
  porder_context <- t(matrix(unlist(input_porder),
    ncol = length(input_porder),
    nrow = length(input_porder[[1]])
  ))

  if (is.null(colnames(input_porder[[1]]))) {
    colnames(input_porder[[1]]) <- rownames(input_porder[[1]]) <-
      seq(1, dim(input_porder[[1]])[1])
  }
  colnames(porder_context) <-
    unlist(lapply(rownames(input_porder[[1]]),
      FUN = function(y) {
        lapply(colnames(input_porder[[1]]),
          FUN = function(x) {
            paste0(y, "<=", x)
          }
        )
      }
    ))


  if (!("porder_edge" %in% scaling_methods)) {
    porder_context_dual <- 1 - porder_context
    colnames(porder_context_dual) <- unlist(
      lapply(rownames(input_porder[[1]]), FUN = function(y) {
        lapply(colnames(input_porder[[1]]), FUN = function(x) {
          paste0(y, " neq<=", x)
        })
      })
    )

    porder_context <- cbind(porder_context, porder_context_dual)
  }

  f_context <- cbind(f_context, porder_context)
}
# Scaling of spatial data
if (!is.null(input_spatial)) {
  spatial_context <- compute_spatial_scaling_mat(input_spatial,
    add_column_name = TRUE
  )$context

  f_context <- cbind(f_context, spatial_context)
}

  # Scaling of nominal data
  if (!is.null(input_factor)) {
    nominal_context <- compute_nominal_scaling_vec(input_factor, "nominal")
    f_context <- cbind(f_context, nominal_context)
  }


  # Scaling of ordinal, numeric data
  if (!is.null(input_ordinal_numeric)) {
    # TODO
    # Name of Rows
    ordinal_context <- compute_ordinal_scaling_vec(
      as.numeric(input_ordinal_numeric), "numeric"
    )
    f_context <- cbind(f_context, ordinal_context)
    if (!("ordinal" %in% scaling_methods)) {
      dual_context <- compute_dualordinal_scal_vec(
        as.numeric(input_ordinal_numeric), "numeric"
      )
      f_context <- cbind(f_context, dual_context)
    }
  }

  return(f_context)
}
