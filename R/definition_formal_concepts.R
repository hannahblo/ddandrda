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
compute_dual_ordinal_scaling_vec <- function(data_values,
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




#' Computes the formal context for ordinal data by the use of dual ordinal
#' scaling
#'
#' @description This function computes the formal context for nominal, inter-
#' ordinal data points. Here, different data types can be observed.
#' Information about the scalind methods can be found in:
#' Ganter, B., Will, R. (2008): Formale Begriffsanalyse,
#' Mathematische Grundlagen, Springer
#'
#' @param data_matrix (dataframe): each row represents one attribute
#' (not necessarily two-valued)
#'
#' @return dataframe representing the crosstable/formal context
#'
#' @examples
#' attr_nominal <- as.factor(c("factor_1", "factor_2", "factor_2", "factor_1"))
#' attr_numeric <- as.factor(c(1.2, 1, 1.6, 2))
#' compute_conceptual_scaling(data.frame(nominal = attr_nominal,
#'                       numeric = as.numeric(as.character(attr_numeric))))
#' @export
compute_conceptual_scaling <- function(data_matrix) {
  number_obj <- dim(data_matrix)[1]
  number_attr <- dim(data_matrix)[2]
  colnames_data <- colnames(data_matrix)

  number_columns_needed <- compute_number_columns_attr(data_matrix)

  # Memory spaces
  context_converted <- array(0, c(
    number_obj,
    number_columns_needed$number_column_all_attr
  ))
  colname_context <- rep("", number_columns_needed$number_column_all_attr)

  t <- 1
  for (k in (1:number_attr)) {

    if (class(data_matrix[, k])[1] == "ordered" ||
      class(data_matrix[, k])[1] == "numeric" ||
      class(data_matrix[, k])[1] == "integer") {
      # Calculating the context for the attribute k
      inner_context <- cbind(
        compute_ordinal_scaling_vec(
          as.numeric(data_matrix[, k]), colnames_data[k]
        ),
        compute_dual_ordinal_scaling_vec(
          as.numeric(data_matrix[, k]), colnames_data[k]
        )
      )
      # number of columns needed for saving
      column_number_k <- number_columns_needed$number_column_per_attr[k] - 1

      # Saving
      context_converted[, t:(t + column_number_k)] <- inner_context
      colname_context[t:(t + column_number_k)] <- colnames(inner_context)

      t <- t + column_number_k + 1
    }
    if (class(data_matrix[, k])[1] == "factor") {
      # Calculating the context for the attribute k
      inner_context <- compute_nominal_scaling_vec(
        data_matrix[, k],
        colnames_data[k]
      )
      # number of columns needed for saving
      column_number_k <- number_columns_needed$number_column_per_attr[k] - 1

      # Saving
      context_converted[, t:(t + column_number_k)] <- inner_context
      colname_context[t:(t + column_number_k)] <- colnames(inner_context)

      t <- t + column_number_k + 1
    }
  }

  # Changing colnames
  colnames(context_converted) <- colname_context

  return(context_converted)
}
