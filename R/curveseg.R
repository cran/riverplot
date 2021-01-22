#' Draw a curved segment 
#'
#' Draws a curved segment from point \code{(x0,y0)} to \code{(x1,y1)}. The
#' segment is a framgent of a sinusoid, has a defined width and can either
#' have a single color or a color gradient.
#'
#'
#'
#' @param x0 X coordinate of the starting point
#' @param y0 X coordinate of the starting point
#' @param x1 X coordinate of the end point
#' @param y1 X coordinate of the end point
#' @param width Width of the segment to plot
#' @param nsteps Number of polygons to use for the segments. The more, the
#' smoother the picture, but at the same time, the more time-consuming to
#' display.
#' @param col Color to use. Ignored if grad is not \code{NULL}.
#' @param grad Gradient to use. Can be anything that
#' \code{colorRampPalette} can understand.
#' @param lty Line type for drawing of the segment. Use \code{lty=0} for no line.
#' @param form "sin" for a sinusoidal segment. "line" for a straight segment.
#' @param fix.pdf Draw a border around segments with line type lty in a desperate attempt to fix the PDF output.
#' @return no value is returned
#' @examples 
#' # a DNA strand
#' plot.new()
#' par( usr= c( 0, 4, -2.5, 2.5 ) )
#'
#' w    <- 0.4
#' cols <- c( "blue", "green" )
#' init <- c( -0.8, -0.5 )
#' pos  <- c( 1, -1 )
#' step <- 0.5
#'
#' for( i in rep( rep( c( 1, 2 ), each= 2 ), 5 ) ) {
#'   curveseg( init[i], init[i] + step, pos[1], pos[2], width= w, col= cols[i] )
#'   init[i] <- init[i] + step
#'   pos <- pos * -1
#' }
#' @export
curveseg <- function( x0, x1, y0, y1, width= 1, nsteps= 50, col= "#ffcc0066", grad= NULL, lty= 1, form= c( "sin", "line" ), fix.pdf=0 ) {
  w <- width

  if( ! is.null( grad ) ) {
    grad <- colorRampPaletteAlpha( grad )( nsteps )
  } else {
    nsteps <- 3
    grad <- rep(col, 3)
    #grad <- rep( col, nsteps )
  }

  grad <- c(grad[1], grad, grad[length(grad)])

  form <- match.arg( form, c( "sin", "line" ) )

  if(x0 > x1) stop("curveseg: x0 > x1")
 
  dx <- get.da.dx(x0, y0, x1, y1, w)
  #printf("dx[1]=%.2f dx[2]=%.2f", dx[1], dx[2])
  #printf("p0 = [%.2f, %.2f] p1= [%.2f, %.2f]", x0, y0, x1, y1)

  if(form == "sin") {
    tmp <- seq(-pi/2, pi/2, length.out=nsteps)
    yy <- y0 + ( y1 - y0 ) * ( sin( tmp ) + 1 ) / 2
  } else if(form == "line") {
    yy <- seq(y0, y1, length.out=nsteps)
  }

  yy  <- c(y0, yy, y1)

  xx <- seq( x0 + dx[2], x1 - dx[1], length.out= nsteps )
  xxA <- c(x0, xx, x1)
  xx <- seq( x0 + dx[1], x1 - dx[2], length.out= nsteps )
  xxB <- c(x0, xx, x1)

  # if(max(dx) > abs(x1 - x0)) {
  #   print(cbind(xxA, xxB, yy))
  # }

  for(i in 1:(nsteps + 1)) {
    polygon( c(xxA[i], xxA[i+1], xxB[i+1], xxB[i]),
             c(yy[i], yy[i+1], yy[i+1] + w, yy[i] + w), col=grad[i], border=grad[i], lty=fix.pdf)
    lines( c( xxA[i], xxA[i+1] ), c( yy[i], yy[i+1] ), lty= lty )
    lines( c( xxB[i], xxB[i+1] ), c( yy[i] + w, yy[i+1] + w ), lty= lty )
  }

  return(0)
 
  if( form == "sin" ) {
    xx  <- seq( -pi/2, pi/2, length.out= nsteps )
    yy <- y0 + ( y1 - y0 ) * ( sin( xx ) + 1 ) / 2
    xx <- seq( x0, x1, length.out= nsteps )
  }

  if( form == "line" ) {
    xx <- seq( x0, x1, length.out= nsteps )
    yy <- seq( y0, y1, length.out= nsteps )
  }

  for( i in 1:(nsteps-1) ) {
    polygon( c( xx[i], xx[i+1], xx[i+1], xx[i] ),
             #c( yy[i], yy[i+1], yy[i+1] + w, yy[i] + w ), col= grad[i], border= grad[i] )
             c( yy[i], yy[i+1], yy[i+1] + w, yy[i] + w ), col= grad[i], border=grad[i], lty=fix.pdf )
    lines( c( xx[i], xx[i+1] ), c( yy[i], yy[i+1] ), lty= lty )
    lines( c( xx[i], xx[i+1] ), c( yy[i] + w, yy[i+1] + w ), lty= lty )
  }

}

.tol <- .Machine$double.eps^0.5

myeq <- function(a, b, tol=.Machine$double.eps^0.5) 
  abs(a - b) <= tol

#cart2pol <- function(x, y) c(r=sqrt(x*x + y*y), phi=atan2(y, x))
#pol2cart <- function(r, phi) c(x=r*cos(phi), y=r*sin(phi))
get.da.pnt <- function(x, y, w) { 
  h <- x^2 + y^2 - w^2
  if(h < .tol) return(c(0, 0))

  h <- sqrt(h)
  #printf("h=%.2f, x=%.2f, y=%.2f, w=%.2f", h, x, y, w)
  y2 <- (h/w * x + y)/(1 + h^2 / w^2)
  x2 <- x - y2 * h / w
  return(c(x=x2, y=y2)) 
}


get.da.dx <- function(x1, y1, x2, y2, w) {

  if(myeq(y1, y2)) return(c(0,0)) 

  xx <- grconvertX(c(x1, x2), from="user", to="inch")
  yy <- grconvertY(c(y1, y2, w, 0), from="user", to="inch")

  x1 <- xx[1] ; x2 <- xx[2] ; y1 <- yy[1] ; y2 <- yy[2]
  w  <- yy[3] - yy[4]
  if(w < .tol) return(c(0, 0)) # this can happen sometimes

  x <- x2 - x1
  y <- w - abs(y2 - y1) 

  xy <- get.da.pnt(x, y, w)
  if(xy[1] < .tol) return(c(0, 0))

  dx <- w * (w - xy[2]) / xy[1]

  dx <- grconvertX(c(dx, 0), from="inch", to="user")
  if(y2 > y1) return(c(0, dx[1] - dx[2]))
  else        return(c(dx[1] - dx[2], 0))
}





