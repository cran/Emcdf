library(lattice)

#' @importFrom grDevices colorRampPalette
#' @importFrom graphics plot
#' @importFrom stats update
#'
#' @title Plots multivariate empirical joint distribution of bivariate data
#' @description This function plots empirical joint distribution (joint CDF) with levelplot, and 3D wireframes.
#' @param data a numeric matrix / data frame of two variables.
#' @param type a character spicifies plot types. Must be one of "levelplot", "wireframe", or "multiple_wireframe".
#' @param angle a numeric scalar for z axis rotation. With default = 60 degrees.
#' @param main a character of plot title.
#' @details
#' When type  = "multiple_wireframe", this function plots 8 wireframes of directions 0 to what parameter angle is.
#' This process takes longer.
#' When type = "levelplot", parameter angle has no effect.
#'
#' @export
#' @import lattice
#' @import stats
#' @import graphics
#' @import methods
#' @examples
#'
#' n = 10
#' set.seed(123)
#' x = rnorm(n)
#' y = x^2 + 0.1*rnorm(n)
#' data = cbind(x, y)
#' plotcdf(data, type = "multiple_wireframe")
#'


plotcdf = function(data, type = "levelplot", angle = 60, main = paste("Bivariate CDF of", deparse(substitute(data)))){
  if(!is.numeric(data)|| (!is.matrix(data) && !is.data.frame(data)))
    stop("data must be a numeric matrix / data frame.")
  if(ncol(data) != 2)
    stop("data must have two columns.")
  if(!type %in% c("levelplot", "wireframe", "multiple_wireframe"))
    stop('type must be one of "levelplot", "wireframe", "multiple_wireframe".')
  if(!is.numeric(angle) || length(angle) != 1)
    stop("angle must be a numeric scalar.")

  n = nrow(data)
  out = Biemcdf(data)/n
  sortX = sort(data[,1])
  sortY = sort(data[,2])
  rownames(out) = sortY
  colnames(out) = sortX

  myout = data.frame(cbind(rep(sortX, each = n), rep(sortY, by = n), as.vector(out)))


  if(type == "levelplot"){
    p = levelplot(myout[,3] ~ myout[,1]*myout[,2], drape = T, colorkey = T, shade = F,
      col.regions = colorRampPalette(c("yellow", "red"))(100), main = main, xlab = colnames(data)[1],
      ylab = colnames(data)[2], zlab = "prob")
    plot(p)
  }

  if(type == "wireframe"){
    p = wireframe(myout[,3] ~ myout[,1]*myout[,2], drape = T, colorkey = T, scales = list(arrows = T),
      col.regions = colorRampPalette(c("yellow", "red"))(100), main = main, xlab = colnames(data)[1],
      ylab = colnames(data)[2], zlab = "prob", screen = list(z = angle,
          x = -60, y = 0))

    plot(p)
  }

  if(type == "multiple_wireframe"){

    p = wireframe(myout[,3] ~ myout[,1]*myout[,2], drape = T, colorkey = T, scales = list(arrows = T),
    col.regions = colorRampPalette(c("yellow", "red"))(100), main = main, xlab = colnames(data)[1],
    ylab = colnames(data)[2], zlab = "prob")

    npanel <- 4
    rotz <- seq(0, angle, length = 2*npanel)
    pl <- update(p[rep(1, 2*npanel)], layout = c(2, npanel),
      panel = function(..., screen) {
        crow <- current.row()
        ccol <- current.column()
        panel.wireframe(..., screen = list(z = rotz[crow + 4*(ccol-1)],
          x = -60, y = 0))
      }
    )

    print(pl)
  }

}


