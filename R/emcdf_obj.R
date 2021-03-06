setClass("emcdf_obj",

  slots = list(
    p = "externalptr",
    k = "integer",
    n = "integer"
    )

)

setGeneric("multi_emcdf", function(object, a){standardGeneric("multi_emcdf")})


setMethod("multi_emcdf", signature(object = "emcdf_obj"), function(object, a){
  if(is.vector(a))
    return (compute(object@p, a))
  else
    return (compute_m(object@p, a))
})

