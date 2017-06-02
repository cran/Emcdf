
#' @title Computes bivariate empirical joint distribution
#' @description This function computes empirical joint distribution (joint CDF) table with dynamical programming.
#' @param data a numeric matrix with two columns.
#'
#' @details
#' This is an optimization for bivariate data.
#
#' @export
#' @examples
#' n = 10^2
#' set.seed(123)
#' x = rnorm(n)
#' y = rnorm(n)
#' data = cbind(x, y)
#' Biemcdf(data)
#'
#' @return
#' a matrix of values of empirical joint CDF function, where rows and columns are the sorted variables.
#' Columns are the first variable, and rows are the second variable.
#'
#'
Biemcdf = function(data){
  if(!is.matrix(data) || !is.numeric(data))
    stop("data must be a numeric matrix.")
  if(ncol(data) != 2)
    stop("data must have 2 columns as variables.")

  out = biemcdf_output(data[,1], data[,2], FALSE) / nrow(data)
  colnames(out) = round(sort(data[,1]), 3)
  rownames(out) = round(sort(data[,2]), 3)

  return (out)
}
