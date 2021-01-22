#' Label with background
#' 
#' Create a label with background
#'
#' Creates a label with a background, a little extra margin (if necessary) etc.
#' @param text a character vector of labels
#' @param x,y numeric vectors (coordinates)
#' @param bg character vector; background color for the labels
#' @param margin numeric vector; margin (in percentage of a single character) for width and
#'        height around the labels
#' @param border character vector; see \code{\link{rect}} for details
#' @param pos character vector; position where labels should be placed,
#'        relative to the coordinates. Can be one of "topleft", "top", "topright",
#'        "left", "center", "right", "bottomleft", "bottom" and "bottomright".
#' @param cex numeric vector; cex to be used for drawing the text
#' @param ... any further parameters are passed to the \code{\link{text}} function
#' @export
bglabel <- function(x, y, text, bg="#cccccc99", margin=0.5, border=NA, pos="center", cex=1, ...) {

  pos <- sapply(pos, function(p) match.arg(p, c("topleft", "top", "topright",
                        "left", "center", "right",
                        "bottomleft", "bottom", "bottomright")))

  #srt <- match.arg(srt, c(0, 90, 180, 270))

  .chcklength <- function(x, n, name) {
    if(length(x) == 1) x <- rep(x, n)
    if(length(x) != n) stop("length of %s must be 1 or length of text", name)
    x
  }

  n <- length(text)
  x      <- .chcklength(x, n, "x")
  y      <- .chcklength(y, n, "y")
  bg     <- .chcklength(bg, n, "bg")
  cex    <- .chcklength(cex, n, "cex")
  margin <- .chcklength(margin, n, "margin")
  pos    <- .chcklength(pos, n, "pos")
  border <- .chcklength(border, n, "border")

  for(i in 1:length(text)) {
    sw <- (strwidth(text[i], cex=cex[i])  + 2 * margin[i] * strwidth("X", cex=cex[i]))/2
    sh <- (strheight(text[i], cex=cex[i]) + 2 * margin[i] * strheight("gkldjq", cex=cex[i]))/2

    x1 <- switch(pos[i],
      topleft     =x[i] - sw,
      left        =x[i] - sw,
      bottomleft  =x[i] - sw,
      top         =x[i],
      center      =x[i],
      bottom      =x[i],
      topright    =x[i] + sw,
      right       =x[i] + sw,
      bottomright =x[i] + sw)

    y1 <- switch(pos[i],
      topleft     =y[i] + sh,
      top         =y[i] + sh,
      topright    =y[i] + sh,
      left        =y[i],
      center      =y[i],
      right       =y[i],
      bottomleft  =y[i] - sh,
      bottom      =y[i] - sh,
      bottomright =y[i] - sh)

    #printf("x1=%.3f, y1=%.3f", x1, y1)
    rect(x1 - sw, y1 - sh, x1 + sw, y1 + sh, border=border[i], col=bg[i])
    text(x1, y1, labels=text[i], cex=cex[i], ...)
  }
}


x2posmat <- function(x) {

  pos.m <- t(data.frame(
    x=x$nodes$x,
    top=x$nodes$y + x$nodes$size/2,
    center=x$nodes$y,
    bottom=x$nodes$y - x$nodes$size/2,
    lpos=x$nodes$y - x$nodes$sizeL/2,
    rpos=x$nodes$y - x$nodes$sizeR/2,
    row.names=x$nodes$ID
  ))

  return(pos.m)
}


# convert between x and y coordinates
#' @importFrom graphics grconvertX grconvertY
dx2dy <- function(dx) 
  grconvertY(grconvertX(dx, from="user", to="in"), from="in", to="user")

dy2dx <- function(dy) 
  grconvertX(grconvertY(dy, from="user", to="in"), from="in", to="user")

calcdx <- function(dx, dy) {



}

## what it says :-)
draw.edges <- function(x, col="#ffcc33", lty=lty, nsteps=50, boxw=0.2, fix.pdf=0) {
  dmsgc("Drawing edges...\n")

  # for each node, we need to to store the position of the current slot, on
  # the right and on the left
	pos.m <- x2posmat(x)

  w <- boxw / 2 

  edgepos <- NULL

  for(i in 1:nrow(x$edges)) {
    n1 <- x$edges$N1[i]
    n2 <- x$edges$N2[i]
    id <- x$edges$ID[i]
    dmsgc("draw.edges N1=%s N2=%s w=%.2f\n", n1, n2, w)

    dx1 <- dx2 <- w
    if(getattr(x$styles, n1, "nodestyle")  %in% c("invisible", "point")) dx1 <- 0
    col1 <- getattr(x$styles, n1, "col")

    if(getattr(x$styles, n2, "nodestyle")  %in% c("invisible", "point")) dx2 <- 0
    col2 <- getattr(x$styles, n2, "col")

    ss <- x$edges$Value[i] 

    if(getattr(x$styles, id, "edgestyle") == "straight") {
      form <- 'line' 
    } else {
      form <- 'sin' 
    }

    # determine the type of edge coloring coloring to use
    grad <- c(col1, col2)
    col  <- NULL
    #printf("edgecol: %s", getattr(x$styles, id, "edgecol"))
    if(getattr(x$styles, id, "edgecol") == "col") {
      grad <- NULL
      col  <- getattr(x$styles, id, "col")
    }

    #print(grad)
    x0 <- pos.m[ "x", n1 ] + dx1
    y0 <- pos.m[ "rpos", n1 ]
    x1 <- pos.m[ "x", n2 ] - dx2
    y1 <- pos.m[ "lpos", n2 ]

    edgepos <- rbind(edgepos, c(x0, y0, x1, y1, ss))

    if(x0 > x1) 
      warning("draw.edges: plot area too small, consider using a larger area (or smaller labels)")
    else
      curveseg(x0, x1, y0, y1, width=ss, grad=grad, col=col,
               lty=lty, nsteps=nsteps, form=form, fix.pdf=fix.pdf)

    pos.m[ "rpos", n1 ] <- pos.m[ "rpos", n1 ] + ss
    pos.m[ "lpos", n2 ] <- pos.m[ "lpos", n2 ] + ss
  }


  colnames(edgepos) <- c("X0", "Y0", "X1", "Y1", "W")
  for(c in colnames(edgepos)) x$edges[,c] <- edgepos[,c]

  dmsgc("Done.\n")
  return(x)
}

## what it says
draw.nodes <- function(x, 
   lty=1, col=NULL, srt=NULL, textcol=NULL, textpos=NULL, boxw=0.2) {
  dmsgc("Drawing nodes...\n")

	pos.m <- x2posmat(x)

  w <- boxw / 2

  for(n in names(x)) {
    if(!debug && getattr(x$styles, n, "nodestyle") =="invisible") next ;

    # if specific values are provided, they override the styles
    if(is.null(.lty <- lty)) .lty <- getattr(x$styles, n, "lty")  
    if(is.null(.col <- col)) .col <- getattr(x$styles, n, "col")  
    if(is.null(.srt <- srt)) .srt <- getattr(x$styles, n, "srt")  
    if(is.null(.textpos <- textpos)) .textpos <- getattr(x$styles, n, "textpos")  
    if(is.null(.textcol <- textcol)) .textcol <- getattr(x$styles, n, "textcol")

    .textcex <- getattr(x$styles, n, "textcex")

    if(is.null(x$nodes$labels) || is.na(x$nodes[n,]$labels)) lab <- n
    else                                              lab <- x$nodes[n,]$labels
    
    if(getattr(x$styles, n, "nodestyle") =="point") {
      points(pos.m[ "x", n ], pos.m[ "center", n ], pch=19, col=col)
    } else {

      rect(
        pos.m[ "x", n ] - w, pos.m[ "bottom", n ], 
        pos.m[ "x", n ] + w, pos.m[ "top", n ], 
        lty=.lty, col=.col)
    }

    if(debug) 
      rect(
        pos.m[ "x", n ] - w, pos.m[ "bottom", n ], 
        pos.m[ "x", n ] + w, pos.m[ "top", n ], 
        lty=1, col="white")

    if(debug) bglabel(pos.m[ "x", n], pos.m["center",n], n, bg="white", border="black")
    else text(pos.m[ "x", n ], pos.m[ "center", n ], lab, col=.textcol, srt=.srt, pos=.textpos, cex=.textcex)
  }
  dmsgc("Done.\n")
}

