#' Transpose DataFrame
#'
#' This function transposes a data frame by converting rows to columns and columns to rows.
#' The first column is assumed to contain the variable names, and the remaining columns contain the values.
#'
#' @param df A data frame to be transposed.
#'
#' @return A transposed data frame.
#' @examples
#' # Example usage:
#' df <- data.frame(
#'   ID = c("A", "B", "C"),
#'   Var1 = c(1, 2, 3),
#'   Var2 = c(4, 5, 6)
#' )
#' transpose_df(df)
#' @importFrom tidyr pivot_longer pivot_wider
#' @export
transpose_df <- function(df) {
  df %>%
    tidyr::pivot_longer(-1) %>%
    tidyr::pivot_wider(names_from = 1, values_from = 3)
}
