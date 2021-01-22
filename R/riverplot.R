debug <- TRUE
debug <- FALSE

dmsgf <- function(...) if(debug) print(sprintf(...))
dmsg  <- function(...) if(debug) print(...)
dmsgc <- function(...) if(debug) cat(sprintf(...))

## make sure that N1's position < N2's position
orderConnections <- function(x) {

  .reorder <- function(xx) xx[ order(x$nodes[xx, "x"]) ]
  x$edges[ , c("N1", "N2") ] <- t(apply(x$edges[ , c("N1", "N2") ], 1, .reorder))

  return(x)
}

# return a non-existing node name
newid <- function(NN, nlist) {
  if(!NN %in% nlist) return(NN)
  i <- 0 ; nn <- paste0(NN, ".", i)
  while(nn %in% nlist) { i <- i + 1 ; nn <- paste0(NN, ".", i) }
  nn
}


## return the last node ID from a riverplot object
last_node <- function(x) x$nodes$ID[nrow(x$nodes)]
last_edge <- function(x) x$edges$ID[nrow(x$edges)]

## find edge ID by its nodes
edge_bynodes <- function(e, N1=NULL, N2=NULL) {
  if(is.null(N1) && is.null(N2)) return(1:nrow(e))
  if(!is.null(N2) && !is.null(N1)) {
    return(which(e$N1 == N1 & e$N2 == N2))
  }
  if(is.null(N2)) return(which(e$N1 == N1))
  if(is.null(N1)) return(which(e$N2 == N2))
}


## insert a row into a data frame at a given position, minding the row
## names which should not be modified
df.insert <- function(df, i, row, rowname) {
  if(is.null(df)) return(df)
  if(!is.list(row)) stop("row must be a list")
  if(!all(names(rowname) %in% colnames(df)))
    stop("df.insert error: all names of rowname must be in colnames of df")

  rnames <- rownames(df)
  nr <- nrow(df)
  if(i > nr) {
    i <- nr + 1
  }

  for(n in colnames(df))
    if(!n %in% names(row)) row[[n]] <- NA

  if(i <= nr) {
    df[seq(i + 1, nr + 1), ] <- df[seq(i, nr), ]
    df[i, ] <- row
    if(i == 1) rownames(df) <- c(rowname, rnames)
    else rownames(df) <- c(rnames[1:(i-1)], rowname, rnames[i:nr])
  } else {
    df <- rbind(df, row)
    rownames(df) <- c(rnames, rowname)
  }

  return(df)
}


## adds a new node with specified parameters to an existing riverplot
## object, returns the result
add_node <- function(x, id, xpos, at.id=NULL, style=NULL) {
  dmsgc("add_node: new id %s at %d\n", id, xpos)

  # add a new row to nodes
  newnode <- list(ID=id, x=xpos)
  #print(newnode)

  if(is.null(at.id)) at.id <- x$nodes$ID[nrow(x$nodes)]
  posi <- match(at.id, x$nodes$ID) + 1
  x$nodes <- df.insert(df=x$nodes, i=posi, row=newnode, rowname=id)
  #print(x$nodes)
  if(!is.null(style)) x$styles[[id]] <- style
  x
}

## select all edges going out from a node
right_edges <- function(x, id) get_edges_by_side(x, id, "right")
left_edges  <- function(x, id) get_edges_by_side(x, id, "left")

get_edges_by_side <- function(x, id, side) {
  if(side == "right") {
    sel  <- x$edges$N1 == id
    N2   <- x$edges$N2[sel]
    N2   <- N2[ x$nodes[N2, "x"] > x$nodes[id, "x"] ]
    eids <- x$edges$ID[ sel & x$edges$N2 %in% N2 ]
  } else {
    sel  <- x$edges$N2 == id
    N1   <- x$edges$N1[sel]
    N1   <- N1[ x$nodes[N1, "x"] < x$nodes[id, "x"] ]
    eids <- x$edges$ID[ sel & x$edges$N1 %in% N1 ]
  }
  
  eids
}


## we need to define our internal classes on loading the package
.onLoad <- function(libname, pkgname) {

  setClass("foo", representation("list"))
  setMethod("[", "foo", 
  function(x, i) {
    x2 <- x@.Data[i]
    new("foo", x2)
  })

  setMethod(">", "foo", function(e1, e2) {

    ifelse( 
      (e1[[1]][[1]] >  e2[[1]][[1]] && e1[[1]][[2]] >= e2[[1]][[2]]) ||
      (e1[[1]][[1]] >= e2[[1]][[1]] && e1[[1]][[2]] >  e2[[1]][[2]]), TRUE, FALSE)
  })

  setMethod("==", "foo", function(e1, e2) {
    !(e1 > e2 || e2 > e1)
  })

  invisible()
}



## try to disentangle the edges by reordering them. We use a custom sort here
reorder_edges <- function(x) {

  if(!ypos_present(x)) 
    stop("reorder_edges: Missing y, this should not happen at this stage")

  dmsgc("reorder_edges: Reordering edges...\n")

  edges <- sapply(1:nrow(x$edges), function(i) c( x$nodes[x$edges$N1[i],"y"], x$nodes[x$edges$N2[i], "y"]), simplify=F)
  fooe <- t(simplify2array(edges))
  #class(edges) <- "foo"
  edges <- new("foo", edges)
  #print(cbind(x$edges, fooe))
  x$edges <- x$edges[ order(edges), ]
  #print(cbind(x$edges, fooe))
  #return(x)

  for(id in names(x)) {
    eids <- right_edges(x, id)
    eids <- match(eids, x$edges$ID)

    N2   <- x$edges[eids, "N2"]
    if(length(N2) == 1) next ;
    ord  <- order(x$nodes[N2, "y"])
    neworder <- 1:nrow(x$edges)
 #   if(any(neworder[eids] != neworder[eids][ord]))
 #     dmsgc("Reordering for node %s\n", id)
    neworder[eids] <- neworder[eids][ord]

    x$edges <- x$edges[neworder,]
  }

  dmsgc("Reordering edges... L\n")

  for(id in names(x)) {
    eids <- left_edges(x, id)
    eids <- match(eids, x$edges$ID)
  
    N1   <- x$edges[eids, "N1"]
  
    if(length(N1) == 1) next ;
  
    ord  <- order(x$nodes[N1, "y"])
    neworder <- 1:nrow(x$edges)
    neworder[eids] <- neworder[eids][ord]
    if(any(neworder[eids] != neworder[eids][ord]))
      dmsgc("reorder_edges: Reordering for node %s\n", id)
    x$edges <- x$edges[neworder,]
  }

  x
}

## adds a new edge with specified parameters to an existing riverplot
## object, returns the result
add_edge <- function(x, id, N1, N2, Value, style=NULL) {
  eid <- newid(id, x$edges$ID)
  # dmsgf("add_edge: id=%s new edge ID=%s", id, eid)

  x$edges <- rbind(x$edges, rep(NA, ncol(x$edges)))
  nn <- nrow(x$edges)
  rownames(x$edges)[nn] <- eid
  x$edges$ID[nn] <- eid
  x$edges$N1[nn] <- N1
  x$edges$N2[nn] <- N2
  x$edges$Value[nn] <- Value

  if(!is.null(style)) x$styles[[eid]] <- style
  x
}

## for two nodes, their positions and a position of a new node in between
## it calculates the color gradient from N1 through new node to N2
average_color <- function(x, N1, N2, p) {
  x1 <- x$nodes[N1, "x"]
  x2 <- x$nodes[N2, "x"]
  if(x1 >= x2) {
    warning("average_color: the positions of the nodes are incorrect")
    return(NA)
  }

  f  <- round(100 * (p - x1)/(x2 - x1))
  # dmsgf("average_color: x1=%.1f, x2=%.1f, p=%.1f, f=%d", x1, x2, p, f)
  col1 <- getattr(x$styles, N1, "col")
  col2 <- getattr(x$styles, N2,   "col")
  tmp <- colorRampPaletteAlpha(c(col1, col2))(100)[f]
  return(tmp)
}

add_mid_points_to_edge <- function(x, all.pos, ei) {

    posL <- which(x$nodes[ x$edges$N1[ei], "x" ] == all.pos)
    posR <- which(x$nodes[ x$edges$N2[ei], "x" ] == all.pos)
    dpos <- posR - posL
    if(dpos < 0) stop(sprintf("add_mid_points: something is wrong, posR[%s] < posL[%s]",
        x$edges$N1[ei], x$edges$N2[ei]))
    if(posR - posL < 2) return(x) ;

    prev.id <- x$edges$N1[ei]
    prev.at <- ifelse(match(x$edges$N1[ei], x$nodes$ID) > match(x$edges$N2[ei], x$nodes$ID),
      x$edges$N1[ei], x$edges$N2[ei])
      

    # add mid-nodes and edges
    for(j in 1:(dpos - 1)) {
      new.pos <- all.pos[ posL + j ]

      new.id <- newid(prev.id, names(x))
      #printf("new.id=%s", new.id)
      style <- getstyle(list(nodestyle="invisible", automatic=TRUE))
      x <- add_node(x, id=new.id, xpos=all.pos[posL + j], at.id=prev.at, style=style)
      x$styles[[new.id]] <- style

      newcol   <- average_color(x, prev.id, x$edges$N2[ei], new.pos)
      # dmsgc("new color=%s\n", newcol)
      x$styles <- setattr(x$styles, new.id, "col", newcol)

      x <- add_edge(x, id=paste0(prev.id, "->", new.id),
                       N1=prev.id,
                       N2=new.id,
                       Value=x$edges$Value[ei])

      prev.id <- new.id
      prev.at <- new.id
      x$styles <- copyattr(x$styles, x$edges$ID[ei], last_edge(x), "horizontal")
    }
    x$edges$N1[ei] <- prev.id

  return(x)
}


## join all nodes that have identical left (dir=="l") or right (dir=="r")
## partner at a given position
join_nodes_at_pos <- function(x, p, dir="l") {

  # for each x position, we search for nodes which can be merged together:
  # which have a neighbor above or below them
  #dmsgc("join_nodes_at_pos: pos=%.2f\n", p)
  sel     <- x$nodes$x == p 
  
  # find "automatic" nodes
  styles <- sapply(x$nodes$ID, function(id) { 
    tmp <- getattr(x$styles, id, "automatic")
    if(is.null(tmp)) tmp <- FALSE ; tmp })

  sel <- sel & styles
  if(!any(sel)) return(x) 
  # else dmsgc("Found!\n")

  cons <- sapply(x$nodes$ID[sel], function(id) connections(x, id, dir=dir), simplify=TRUE)

  # unique lists of nodes on the left
  uniq <- unlist(cons[ sapply(cons, length) == 1 ])
  uniq <- unique(uniq[ duplicated(uniq) ])

  for(s in uniq) {
    # dmsgc("joining partners of %s\n", s)
    matches <- names(cons)[cons == uniq]
    id1 <- matches[1]
    # left / right edge of id1
    for(i in 2:length(matches)) {

      if(dir == "l") e1  <- edge_bynodes(x$edges, N1=s, N2=id1)
      else           e1  <- edge_bynodes(x$edges, N1=id1, N2=s)

      id2 <- matches[i]
      # dmsgc("joining %s and %s\n", id1, matches[i])

      if(dir == "l") e2  <- edge_bynodes(x$edges, s, id2)
      else           e2  <- edge_bynodes(x$edges, id2, s)

      x$edges$Value[e1] <- x$edges$Value[e1] + x$edges$Value[e2]

      # remove node s -> id2
      x$edges <- x$edges[-e2,]

      # edges to the right/left of id2
      if(dir == "l") eR <- edge_bynodes(x$edges, N1=id2)
      else           eR <- edge_bynodes(x$edges, N2=id2)

      # rewire all edges on the right of N2 to N1
      if(dir == "l") for(e in eR) x$edges$N1[e] <- id1
      else           for(e in eR) x$edges$N2[e] <- id1

      # remove node id2
      x$nodes <- x$nodes[ x$nodes$ID != id2, ]
    }

    # copy the color attribute from the common node
    x$styles <- copyattr(x$styles, s, id1, "col")
  }
  

  return(x)
}

## adds new, invisible nodes to better direct output jumping over certain
## stops
add_mid_points <- function(x, default_style=NULL) {

  all.pos  <- sort(unique(x$nodes$x))

  for(ei in 1:nrow(x$edges)) {
    x <- add_mid_points_to_edge(x, all.pos, ei)
  }

  for(p in rev(all.pos)) {
    x <- join_nodes_at_pos(x, p, dir="l")
  }
 
  for(p in all.pos) {
    x <- join_nodes_at_pos(x, p, dir="r")
  }

  return(x)
}

## return a list of nodes with which a given node is connected
connections <- function(x, N, dir="l") {
  ret <- c()
  switch(dir,
    l=unique(x$edges$N1[ x$edges$N2 == N ]),
    r=unique(x$edges$N2[ x$edges$N1 == N ]),
    t=unique(c(
    x$edges$N2[ x$edges$N1 == N ],
    x$edges$N1[ x$edges$N2 == N ])))
}

## calculate the sizes of the nodes, left, right and total
calcsizes2 <- function(x) {
  dmsgc("Calculating sizes...\n")

  e      <- x$edges
  nnames <- names(x)

  # total of values going into the node from left
  lefts  <- sapply(nnames, function(n) { sum(e$Value[ e$N2 ==n ]) })
  names(lefts) <- nnames

  # total sum of values going out from the node on the right
  rights <- sapply(nnames, function(n) { sum(e$Value[ e$N1 ==n ]) })
  names(rights) <- nnames
  
  # sizey is, for each node, the greater of the value "lefts" and "rights"
  sizey <- apply(cbind(lefts, rights), 1, max)
  names(sizey) <- nnames
  pos.list  <- sort(unique(x$nodes$x))

  x$nodes$size <- sizey
  x$nodes$sizeL <- lefts
  x$nodes$sizeR <- rights
  dmsgc("Done.\n")
  return(x)
}

## the largest node size
pos.maxsize <- function(x) {
  pos.list  <- sort(unique(x$nodes$x))
  pos.maxes <- sapply(pos.list, function(p) sum(x$nodes$size[ x$nodes$x ==p ]))
  return(max(pos.maxes))
}


## calculate the vertical positions of the nodes
calcY <- function(x, gravity="top", node_margin=0.1) {
  dmsgc("Calculationg positions...\n")

  # all available horizontal slots
  pos.list <- sort(unique(x$nodes$x))

  nnodes <- names(x)

  # calculate the distance between nodes
  max.y <- pos.maxsize(x)
  interstop <- max.y * node_margin # XXX
  dmsgf("calcY interstop=%.5f", interstop)
  #print(x$nodes)

  if(is.null(x$nodes$y)) x$nodes$y <- NA
  if(!is.null(x$nodes$y) && all(!is.na(x$nodes$y))) calc <- FALSE
  else calc <- TRUE

  for(i in 1:length(pos.list)) {
    p <- pos.list[ i ]

    cur.y <- 0
    nn <- nnodes[ x$nodes$x ==p ] 
    #printf("pos: %d", p)
    if(gravity =="top") nn <- rev(nn)
    for(n in nn) {
      dmsgc("calcY: processing %s\n", n)
      #print(n)
      if(! is.null(x$nodes$y) && ! is.na(x$nodes[n,]$y)) {
        # do nothing
      } else if(gravity =="top") {
        #print("top gravity")
        x$nodes[n,]$y <- cur.y - x$nodes[n,]$size / 2
        cur.y <- cur.y - x$nodes[n,]$size - interstop
      } else if(gravity %in% c("bottom", "center")) {
        x$nodes[n,]$y <- cur.y + x$nodes[n,]$size / 2
        cur.y <- cur.y + x$nodes[n,]$size + interstop
      }
    }
  }

  if(calc && gravity =="center") {
    bottoms <- x$nodes$y - x$nodes$size / 2
    tops    <- x$nodes$y + x$nodes$size / 2
    for(p in pos.list) {
      sel <- x$nodes$x == p
      ylim <- range(c(bottoms[sel], tops[sel]))
      dy <- ylim[2] - ylim[1]
      x$nodes[sel,]$y <- x$nodes[sel,]$y - dy/2
    }
  }

  dmsgc("done.\n")
  return(x)
}

## recalculate Y position taking into account specific node properties
recalcY <- function(x) {

  e <- x$edges
  n <- x$nodes

  # all available horizontal slots
  pos.list <- sort(unique(x$nodes$x))
  bottoms <- setNames(n$y - n$size/2, names(x))

  for(p in pos.list) {

    sel <- n$ID[ n$x == p ]

    e.sel <- e$N1 %in% sel 

    # force the "horizontal" attribute
    attr <- unlist(sapply(e$ID[e.sel], function(id) getattr(x$styles, id, "horizontal")))
    e.sel <- e$ID %in% names(attr)[ attr ]

    for(e1 in e[ e.sel, "ID" ]) {
      n1 <- e[ e1, "N1" ]
      n2 <- e[ e1, "N2" ]
      dmsgc("Correcting edge >%s< %s => %s\n", e1, n1, n2)

      # position of the edge in the order of edges
      sel2 <- which(e$ID == e1)
      e1.v <- e[ e1, "Value" ]

      # determine exact position of this edge on left and right
      sel  <- which(e$N1 == n1)
      y1 <- sum(e$Value[ sel[ sel < sel2 ] ]) + e1.v/2

      sel  <- which(e$N2 == n2)
      y2 <- sum(e$Value[ sel[ sel < sel2 ] ]) + e1.v/2

      dy <- y2 - y1

      dy <- (bottoms[n2] + y2) - (bottoms[n1] + y1)
      
      x$nodes[n2,"y"] <- x$nodes[n2, "y"] - dy

    }
  }

  x
}



## check whether vertical information is present
ypos_present <- function(x) {
  if(is.null(x$nodes$y) || all(is.na(x$nodes$y))) return(FALSE)
  yrange <- range(x$nodes$y)

  if(yrange[1] == yrange[2]) return(FALSE)
  TRUE
}


## scale node sizes automatically
autoscale <- function(x) {

  # if y values are not defined for nodes, 
  # we keep scale at 1
  if(! ypos_present(x)) return(1)

  yrange <- range(x$nodes$y)
  yrange <- yrange[2] - yrange[1]
  dmsgf("autoscale: yrange=%f", yrange)

  ns <- max(x$edges$Value)

  if(ns ==0) return(1) # not our problem

  yscale <- 0.15 * yrange / ns
  dmsgf("autoscale: yscale=%.2f", yscale)
  return(yscale)
}

#' @rdname riverplot
#' @export
plot.riverplot <- function(x, ... ) riverplot(x, ...)

#' Create a Sankey plot
#'
#' Create a Sankey plot
#'
#' This functions create a Sankey plot given a riverplot object
#' (\code{plot} is just a wrapper for the \code{riverplot} function.
#' The object to be drawn is a list specifying the plot; see
#' the \code{\link{makeRiver}} function for exact specifications and
#' the \code{\link{riverplot.example}} to see how it can be created.
#' Whether or not the list used to plot is exactly of class
#' \code{riverplot-class} does not matter as long as it has the correct
#' contents. 
#'
#' Style information which is missing from the riverplot object \code{x} (for example, if the
#' node style is not specified for each node in the object) is taken from the \code{default.style} parameter.
#' See functions \code{\link{default.style}()} and
#' \code{\link{updateRiverplotStyle}()} to learn how to create and
#' modify the styles.
#'
#' Whether or not the list used to plot is exactly of class
#' \code{riverplot-class} does not matter as long as it has the correct
#' contents. These functions here are for the convenience of checking that
#'
#' The nodes are drawn from bottom to top in the order they are found in
#' the riverplot object. There is no clever algorithm for placing the nodes
#' minimizing the number of crossing edges yet; you need to manipulate the
#' object directly to achieve the desired effect.
#'
#' @section Known problems:
#' There is a problem with transparency and PDFs.
#' In short, if you try to save your riverplot graphics as PDF, you will
#' observe thin, white vertical lines everywhere on the curves. The reasons
#' for that are unclear, but have something to do with PDF rendering (if you
#' generate EPS, the output looks good).
#' 
#' There is a kind of fix to that: use the fix.pdf=TRUE option. Unfortunately,
#' this solution does not work if you use transparent colors (you will have a
#' different kind of vertical lines). Unfortunately, I don't have a solution
#' for that problem yet.
#'
#'@param x An object of class riverplot
#'@param direction "lr" (left to right) or "rl" (right to left)
#'@param lty Line style to use
#'@param default_style default graphical style
#'@param gravity how the nodes are placed vertically. No effect if node
#'       vertical positions are specified via \var{node_ypos} member
#'@param node_margin how much vertical space should be kept between the
#'       nodes
#'@param nodewidth width of the node (relative to font size)
#'@param plot_area fraction of vertical and horizontal space to be used as main plot area
#'       If it is a numeric vector of two numbers, the first one is
#'       horizontal space, the second vertical. 
#'@param nsteps number of interpolating steps in drawing the segments
#'@param disentangle try to disentangle connections between the nodes. If
#'       FALSE, the vertical ordering of the connections is the same as in the
#'       x$edges data frame.
#'@param add_mid_points attempt to get a smoother plot by adding additional
#'       nodes. Set this parameter to \code{FALSE} if you are setting node
#'       vertical position manually. If add_mid_points is equal to TRUE (the
#'       default), then the
#'       mid points are added only if \var{node_ypos} is empty.
#'@param yscale scale the edge width values by multiplying with this
#'       factor. If \var{yscale} is equal to "auto", scaling is done
#'       automatically such that the vertical size of the largest node is
#'       approximately 15% of the range of ypos (if present). 
#'       If no \var{node_ypos} is specified in the riverplot object, no scaling is
#'       done. If \var{yscale} is equal to 1, no scaling is done.
#'       This parameter only influences the plot if the y positions of the
#'       nodes are provided in \code{x$nodes}.
#'@param add If TRUE, do not call plot.new(), but add to the existing plot.
#'@param usr coordinates at which to draw the plot in form (x0, x1, y0, y1). 
#'       If NULL, par("usr") will be used instead. 
#'@param adjust.usr If TRUE, the par("usr")
#'       will be modified to suit the x and y coordinates of the riverplot
#'       nodes (whether the coordinates were given in the nodes, or
#'       calculated by the function). In combination with providing x and y
#'       coordinates, this allows a true representation of a riverplot object.
#'       Necessary if you plan to plot additional, external data. If TRUE,
#'       then \code{rescale} is set to FALSE.
#'       See \code{\link{minard}} data set and example for details.
#'@param rescale if TRUE, then the plot will be fit into the given
#'       user coordinates range (set by the usr parameter, for example, or the whole plot
#'       region). If FALSE, the x and y positions of the nodes will be
#'       treated as user coordinates and used to directly plot on the device.
#'@param fix.pdf Try to fix PDF output if it looks broken (with thin white lines). Don't use this option if you are using transparent colors.
#'@param bty box type to draw around the plot; see \code{bty} in documentation for \code{\link{par}} for details.
#'@param ... any further parameters passed to riverplot() are appended to the default style
#'@return \code{riverplot} returns a riverplot object, a graph which you
#' can plot again with riverplot(), but which additionally contains
#' information on node position and size in the \code{$nodes} member.
#'@seealso default.style updateRiverplotStyle minard
#'@examples 
#' x <- riverplot.example()
#' plot(x)
#' plot(x, srt=90, lty=1)
#'
#' # add graphics at nodes
#' foo <- plot(x, srt=90, lty=1)
#' points(foo$nodes$x, foo$nodes$y, pch=19, cex=2)
#'
#' # redraw the same graph using positions from foo object
#' plot(foo, yscale=1)
#'@importFrom grDevices col2rgb dev.flush dev.hold rgb
#'@importFrom graphics lines par plot.new points polygon rect strwidth strheight text
#'@importFrom stats setNames
#'@import methods
#'@export
riverplot <- function(x, direction="lr", lty=0, 
                             default_style=NULL, 
                             gravity="top", 
                             node_margin=0.1, 
                             nodewidth=1.5,
                             plot_area=c(1, 0.5),
                             nsteps=50,
                             disentangle=TRUE,
                             add_mid_points=TRUE,
                             yscale="auto",
                             add=FALSE,
                             usr=NULL,
                             adjust.usr=FALSE,
                             rescale=TRUE,
                             fix.pdf=FALSE,
                             bty="n",
                             ...
                            ) {

  ds <- list(...)
  default_style <- getstyle(ds, defaults=default_style)

  direction <- match.arg(direction, c("lr", "rl"))
  gravity   <- match.arg(gravity, c("center", "top", "bottom"))
  fix.pdf   <- as.numeric(fix.pdf)

  if(is.null(plot_area)) plot_area <- 1
  if(length(plot_area) == 2) {
    plot_area_x <- plot_area[1]
    plot_area_y <- plot_area[2]
  } else {
    plot_area_x <- plot_area_y <- plot_area[1]
  }

  if(!add) plot.new()
  x2 <- checkriverplot(x)

  # update styles for all nodes
  x2 <- update_styles(x2, default_style)

  # N1 must be the node on the left, N2 on the right
  x2 <- orderConnections(x2)

  # add mid points automatically depending whether y-positions of nodes are specified
  if(add_mid_points && !ypos_present(x2)) {
    dmsg("adding mid points")
    x2 <- add_mid_points(x2)
  }

  if(is.null(yscale)) yscale <- 1
  else if(yscale == "auto") {
    #if(adjust.usr) yscale <- 1
    #else           
      yscale <- autoscale(x2)
  }
  x2$edges$Value <- x2$edges$Value * yscale

  # calculate node size by summing in- and out-going edges
  x2 <- calcsizes2(x2)
  x2 <- calcY(x2, gravity=gravity, node_margin=node_margin)

  # scale accordingly to graphic window parameters
  lim <- get_lim(x2, plot_area_x, plot_area_y)
  nodewidth <- strwidth("hjKg") * nodewidth / 2
  usr <- update_usr(usr, adjust.usr, lim, nodewidth)
  x2 <- rescale(x2, usr, nodewidth, rescale, direction, adjust.usr, lim) 

  x2 <- recalcY(x2)
  if(disentangle) x2 <- reorder_edges(x2)
  x2 <- recalcY(x2)

  # put device on hold; very slow otherwise
  dev.hold()
  on.exit({
    dev.flush()
    })

  # actual drawing
  draw.edges(x2, lty=lty, nsteps=nsteps, boxw=nodewidth, fix.pdf=fix.pdf)
  draw.nodes(x2, boxw=nodewidth, lty=lty)
  .box(usr, bty)
  return(invisible(x2))
}

## like box(), but around any arbitrary rectangle 
.box <- function(usr, bty="n") {

  if(is.null(bty) || bty == "n") return()

  x1 <- usr[1] ; x2 <- usr[2] ; y1 <- usr[3] ; y2 <- usr[4]

  bty <- match.arg(bty, c("7", "l", "o", "u", "c", "]", "n"))
  if(bty %in% c("7", "o", "c", "]")) lines(c(x1, x2), c(y2, y2)) # top 
  if(bty %in% c("7", "o", "u", "]")) lines(c(x2, x2), c(y1, y2)) # right
  if(bty %in% c("c", "o", "u", "l")) lines(c(x1, x1), c(y1, y2)) # left
  if(bty %in% c("l", "o", "c", "u", "]")) lines(c(x1, x2), c(y1, y1)) # bottom 
}

## if usr needs to be adjusted, do it here. Otherwise, if null, fill it
## with the current value of par("usr")
update_usr <- function(usr=NULL, adjust.usr=FALSE, lim, nodewidth) {

  xlim <- lim$xlim
  ylim <- lim$ylim

  if(is.null(usr)) usr <- par("usr")

  if(adjust.usr) {
    # set the usr parameter to fit the figure
    par(usr=c(xlim[1] - nodewidth/2, xlim[2] + nodewidth/2, ylim[1], ylim[2]))
    usr <- par("usr")
  } 

  usr
}

## calculate the extreme x and y values (plus add space for the plot area)
get_lim <- function(x, plot_area_x, plot_area_y) {

  xlim   <- range(x$nodes$x)

  b <- (xlim[2] - xlim[1]) * (1 - plot_area_x)/plot_area_x
  xlim <- xlim + c(-b, b)

  ylim <- with(x$nodes, range(c(y - size/2, y + size/2)))

  b <- (ylim[2] - ylim[1]) * (1 - plot_area_y)/plot_area_y
  ylim <- ylim + c(-b, b)

  return(list(xlim=xlim, ylim=ylim))
}

## to fit the riverplot object on the screen, either screen must be adapted
## to the object, or the object must be rescaled. (no fitting is done if
## rescale option is equal to FALSE)
rescale <- function(x, usr, nodewidth, rescale, direction, adjust.usr, lim) {

  ## horizontal and vertical limits needed to calculate scaling

  xlim <- lim$xlim
  ylim <- lim$ylim
 
  if(rescale && !adjust.usr) {
    # rescale the coordinates to on screen coordinates

    xscale <- (usr[2] - usr[1] - nodewidth)/(xlim[2] - xlim[1])
    yscale <- (usr[4] - usr[3])/(ylim[2] - ylim[1]) 

    x$nodes$x <- (x$nodes$x - xlim[1]) * xscale + usr[1] + nodewidth/2

    x$nodes$y <- (x$nodes$y - ylim[1]) * yscale + usr[3]
    x$nodes[,c("size", "sizeL", "sizeR")] <- 
      x$nodes[,c("size", "sizeL", "sizeR")] * yscale

    x$edges$Value <-x$edges$Value * yscale
  }

  if(direction == "rl") x$nodes$x <- usr[2] + usr[1] - x$nodes$x
  if(debug) rect(usr[1], usr[3], usr[2], usr[4])
  return(x)
}







