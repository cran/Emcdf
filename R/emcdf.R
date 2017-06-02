

#' @title Computes multivariate empirical joint distribution
#' @description This function computes empirical joint distribution (joint CDF) with single/ multi-thread.
#' @param data a numeric matrix stores data. Or an S4 object of class "emcdf_obj".
#' @param a a numeric vector or matrix of parameters for CDF function.
#'
#' @details
#' When data is a numeric matrix, this function computes joint empirical CDF with single thread.
#' When data is an object of class "emcdf_obj", it computes with multi-thread. Parameter "a"
#' must have equal length (or equal column number) as the column number of data. Both single-thread and multi-thread emcdf
#' algorithms are faster than using the bulit-in function sum\{base\}. See example for simulation.
#' Note that initializing threads and spliting data takes time though it's a one-time task.
#' Thus for big data, big number of CDF computation, multi-thread is recommended. Yet for small data,
#' small number of CDF computation, single thread is faster.
#' @export
#' @examples
#'
#' n = 10^6
#' set.seed(123)
#' x = rnorm(n)
#' y = rnorm(n)
#' z = rnorm(n)
#' data = cbind(x, y, z)
#' #The aim is to compute F(0.5,0.5,0.5) with three
#' #approaches and compare the performances.
#' #To avoid CPU noises, we repeat the computation 10 times.

#' #compute with R built-in function, sum()
#' sum_time = system.time({
#'   aws1 = c()
#'   for(i in 1:10)
#'     aws1[i] = sum(x <= 0.5& y <=0.5& z <=0.5)/n
#' })[3]
#'
#' #compute with emcdf single-thread
#' a = matrix(rep(c(0.5, 0.5, 0.5), 10), 10, 3)
#' single_time = system.time({
#'    aws2 = emcdf(data, a)
#' })[3]
#'
#compute with emcdf multi-thread
#' obj = initF(data, 4)
#' multi_time = system.time({
#'    aws3 = emcdf(obj, a)
#' })[3]

#' aws2 == aws1
#' aws3 == aws1
#' sum_time
#' single_time
#' multi_time

#'
#' @return
#' a numeric (vector) as value(s) of empirical joint CDF function.
#'
#'
emcdf = function(data, a){
  if(!is.numeric(a))
    stop("a must be a numeric vector or matrix.")

#emcdf_obj as input
  if(class(data) == "emcdf_obj"){
    if((is.vector(a) && data@k != length(a)) || (is.matrix(a) && data@k != ncol(a)))
      stop("column number of data must equal to length of a.")

    return (multi_emcdf(data, a)/data@n)
  }

#matrix as input
  else{
    if(!is.numeric(data) || !is.matrix(data))
       stop('data must be a numeric matrix or an object of class "emcdf_obj".')
    if((is.vector(a) && ncol(data) != length(a)) || (is.matrix(a) && ncol(data) != ncol(a)))
        stop("column number of data must equal to length (ncol) of a.")

    n = nrow(data)

    if(is.vector(a)){
      if(ncol(data) == 1)
        return (single1(data, a)/n)
      else if(ncol(data) == 2)
        return (single2(data, a)/n)
      else if(ncol(data) == 3)
        return (single3(data, a)/n)
      else if(ncol(data) == 4)
        return (single4(data, a)/n)
      else if(ncol(data) == 5)
        return (single5(data, a)/n)
      #...
      else
        return (single(data, a)/n)
    }

    else{
      if(ncol(data) == 1)
        return (single1_m(data, a)/n)
      else if(ncol(data) == 2)
        return (single2_m(data, a)/n)
      else if(ncol(data) == 3)
        return (single3_m(data, a)/n)
      else if(ncol(data) == 4)
        return (single4_m(data, a)/n)
      else if(ncol(data) == 5)
        return (single5_m(data, a)/n)
      #...
      else
        return (single_m(data, a)/n)

    }

  }


  return (-1);
}
