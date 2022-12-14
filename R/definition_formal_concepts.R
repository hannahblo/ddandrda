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
#' @description 'compute_dual_ordinal_scaling_vec' computes for a set of dual
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
compute_dual_ordinal_scaling <- function(data_values,
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





#' Auxiliary function for calculate_conceptual_scaling
#'
#' @description This function is needed to calculate the number of attributes to
#' represent each column. This depends on the used class of the values in the
#' column.
#'
#' @param data_matrix (dataframe): each row represents one attribute
#'  (not necessarily two-valued)
#'
#' @return (list): number of attributes needed (all and per attribute),
#'  the column names and the class of the elements
compute_number_columns_attr <- function(data_matrix) {
  number_attr <- dim(data_matrix)[2]
  colnames_data <- colnames(data_matrix)

  # Memory space
  number_column_per_attr <- rep(0, number_attr)
  column_names <- NULL
  class_elements_columns <- rep("", number_attr)

  # Looping throw all attrbutes given by the input
  for (k in (1:number_attr)) {
    # Saving which mode have the elements in column k
    class_elements_columns[k] <- class(data_matrix[, k])

    # if the mode is "ordered", "numeric" or "integer" the values are ordinal
    # anddual-ordinal saved and hence we have 2*(number of different values in
    # column k)
    if (class(data_matrix[, k])[1] == "ordered" ||
      class(data_matrix[, k])[1] == "numeric" ||
      class(data_matrix[, k])[1] == "integer") {
      number_column_per_attr[k] <- 2 * length(unique(data_matrix[, k]))
    }
    # if the mode of column k is "factor" for each value one column is needed
    if (class(data_matrix[, k])[1] == "factor") {
      number_column_per_attr[k] <- length(unique(data_matrix[, k]))
    }

    # Now we save the colnames by adding to the already defined ones
    column_names <- c(column_names, rep(
      colnames_data[k],
      number_column_per_attr[k]
    ))
  }

  return(list(
    class_elements_columns = class_elements_columns,
    number_column_per_attr = number_column_per_attr,
    number_column_all_attr = sum(number_column_per_attr),
    column_names = column_names
  ))
}









#' Help Function of compute_conceptual_scaling()
#' Test if all input variables are valid
check_input_ccs_1 <- function(input_factor = NULL,
                            input_ordinal_numeric = NULL,
                            input_spatial = NULL,
                            input_porder = NULL,
                            scaling_methods = NULL) {


  if (all(
    is.null(input_factor),
    is.null(input_ordinal_numeric),
    is.null(input_spatial),
    is.null(input_porder)
  )) {
    stop("Data input is empty.")
  }

 # bool input_vaild = FALSE;
  # input_valid &= is.null(input_factor);

  i# f(input_valid){
#
  # } Nicco c beispiel

  length_values <- unique(c(
    length(input_factor),
    length(input_ordinal_numeric),
    length(input_spatial),
    length(input_porder)
  ))
  number_obj <- setdiff(length_values, c(0))
  if (length(number_obj) > 1) {
    stop("Data inputs must have same length or be NULL.")
  }

  if (!is.null(input_spatial)) {
    stop("Is not implemented yet.")
  }
}


#' Help Function of compute_conceptual_scaling()
#' Test if all input variables are valid
check_input_ccs_2 <- function(input_factor = NULL,
                              input_ordinal_numeric = NULL,
                              input_spatial = NULL,
                              input_porder = NULL,
                              scaling_methods = NULL) {
  if (!is.null(input_porder) &&
      (!is.list(input_porder) || !is.matrix(input_porder[[1]]))) {
    stop("input_spatial must either be NULL or a list of matrices.")
  }

  if (!is.null(input_factor) &&
      (!(class(input_factor)[1] == "factor"))) {
    stop("input_factor must either be NULL or a vector of factors.")
  }

  # TODO
  # check scaling methods used

  # TODO
  # when input rows have names --> check if the order is everywhere the same

}


#' Help Function of compute_conceptual_scaling()
#' Test if all input variables are valid
check_input_ccs_3 <- function(input_factor = NULL,
                              input_ordinal_numeric = NULL,
                              input_spatial = NULL,
                              input_porder = NULL,
                              scaling_methods = NULL) {


  if (!is.null(input_ordinal_numeric) &&
      (!(class(input_ordinal_numeric)[1] == "ordered" ||
         class(input_ordinal_numeric)[1] == "numeric" ||
         class(input_ordinal_numeric)[1] == "integer"))) {
    stop("input_ordinal_numeric must either be null or of class ordered,
           numeric or integer.")
  }
}


#' Computes the formal context for ordinal data by the use of dual ordinal
#' scaling
#'
#' @description This function computes the formal context for nominal, inter-
#' ordinal data points. Here, different data types can be observed.
#' Information about the scalind methods can be found in:
#' Ganter, B., Will, R. (2008): Formale Begriffsanalyse,
#' Mathematische Grundlagen, Springer
#' TODO CHANGES IN THE CODE HAVE NOT BEEN ADDED IN DESCRIPTION
#'
#' @param data_matrix (dataframe): each row represents one attribute
#' (not necessarily two-valued)
#'
#' @return dataframe representing the crosstable/formal context
#'
#' @examples
#' attr_nominal <- as.factor(c("factor_1", "factor_2", "factor_2", "factor_1"))
#' attr_numeric <- as.factor(c(1.2, 1, 1.6, 2))
#' compute_conceptual_scaling(data.frame(
#'   nominal = attr_nominal,
#'   numeric = as.numeric(as.character(attr_numeric))
#' ))
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
    check_input_ccs_1(input_factor = input_factor,
                      input_ordinal_numeric = input_ordinal_numeric,
                      input_spatial = input_spatial,
                      input_porder = input_porder,
                      scaling_methods = scaling_methods)
    check_input_ccs_2(input_factor = input_factor,
                      input_ordinal_numeric = input_ordinal_numeric,
                      input_spatial = input_spatial,
                      input_porder = input_porder,
                      scaling_methods = scaling_methods)
    check_input_ccs_3(input_factor = input_factor,
                      input_ordinal_numeric = input_ordinal_numeric,
                      input_spatial = input_spatial,
                      input_porder = input_porder,
                      scaling_methods = scaling_methods)
  }


  # Set up / Basic information
  length_values <- unique(c(
    length(input_factor),
    length(input_ordinal_numeric),
    length(input_spatial),
    length(input_porder)
  ))
  number_obj <- setdiff(length_values, c(0))

  f_context <- matrix(NA, nrow = number_obj, ncol = 0)
  # TODO
  # add the name of rows

  # Scaling of partial orders
  if (!is.null(input_porder)) {

    # TODO
    # add the names of columns
    if (!("porder_edge" %in% scaling_methods)) {
      porder_context <- matrix(NA,
        ncol = length(input_porder[[1]]) * 2,
        nrow = length(input_porder)
      )
      for (i in seq_along(input_porder)) {
        porder_context[i, ] <- c(input_porder[[i]], !input_porder[[i]])
      }
    }


    if ("porder_edge" %in% scaling_methods) {
      porder_context <- matrix(NA,
        ncol = length(input_porder[[1]]),
        nrow = length(input_porder)
      )
      for (i in seq_along(input_porder)) {
        porder_context[i, ] <- c(input_porder[[i]])
      }
    }

    f_context <- cbind(f_context, porder_context)
  }

  # Scaling of spatial data
  # TODO

  # Scaling of nominal data
  if (!is.null(input_factor)) {

    nominal_context <- compute_nominal_scaling_vec(input_factor)
    # TODO
    # add names of columns
    f_context <- cbind(f_context, nominal_context)
  }


  # Scaling of ordinal, numeric data
  if (!is.null(input_ordinal_numeric)) {
    # TODO
    # Name of Rows
    ordinal_context <- compute_ordinal_scaling_vec(
      as.numeric(input_ordinal_numeric)
    )
    f_context <- cbind(f_context, ordinal_context)
    if (!("ordinal" %in% scaling_methods)) {
      dual_context <- compute_dual_ordinal_scaling(
        as.numeric(input_ordinal_numeric)
      )
      f_context <- cbind(f_context, dual_context)
    }
  }

  return(f_context)
}
