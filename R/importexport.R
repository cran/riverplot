#' Create a new riverplot object
#' 
#' Create a new riverplot object
#' 
#' Functions to create a new object of the riverplot class from the
#' provided data.
#'
#' \code{makeRiver} creates a plot from an object which specifies the graph
#' directly, i.e. all nodes, their horizontal positions on the plot, provided
#' styles etc. See sections below for detailed explanations.
#' 
#' @section Structure of the riverplot objects:
#' 
#' A riverplot object is a list with the following entries:
#' \describe{
#'    \item{nodes}{A data frame specifying the nodes, containing at least
#' the columns "ID" and "x" (horizontal position of the node). Optionally,
#' it can also contain columns "labels" (the labels to display) and "y"
#' (vertical position of the node on the plot)}
#'    \item{edges}{A data frame specifying the edges and graph topology,
#' containing at least the columns "ID", "N1", "N2" and "Value", specifying,
#' respectively, the ID of the edge, the parent node, the child node, and the
#' size of the edge.}
#'    \item{styles}{A named list of styles. Names of this list are the node or edge IDs. Values are
#' styles specifying the style of the given node or edge (see below).}
#' }
#'
#' Whether or not the list used to plot is exactly of class
#' \code{riverplot-class} does not matter as long as it has the correct
#' contents. The \code{makeRiver} function is here are for the convenience of checking that
#' this is the case and converting information in different formats.
#'
#' @section Generating riverplot objects:
#'
#' To generate and fool-proof riverplot objects, you can use the
#' \code{makeRiver} function. This functions allows a number of ways of
#' specifying the node and edge information.
#'
#' Nodes can be specified as a character vector (simply listing the nodes) or
#' as a data frame. 
#' \itemize{
#'           \item character vector: in this case, you also need to provide the
#' \var{node_xpos} argument to specify the horizontal positions of the nodes.
#'           \item data frame: the data frame must have at least a column called "ID";
#' the horizontal position can be specified either with \var{node_xpos}
#' argument or by column "x" in the data frame. Optionally, the data frame
#' can include columns "labels" and "y" (vertical positions of the node).
#' Any \var{NA} values are ignored (not entered into the riverplot project).
#' Additionaly, the data frame may contain style information.
#' }
#'
#' Edges / graph topology can be specified in one of two objects: either
#' a named list, or a data frame:
#' \itemize{ 
#'     \item you can supply 
#' a named list with edges of the graph. The name of each
#' element is the name of the outgoing (parental) node. Each element is a named list; the
#' names of the list are the names of the incoming (child) node IDs; the values are
#' the width of the edge between the outgoing and incoming nodes.
#'     \item Alternatively, you can provide the edges as a data frame. Each row
#' corresponds to an edge, and the data frame must have the following
#' columns:
#'
#' \describe{
#'   \item{N1}{The ID of the first node}
#'   \item{N2}{The ID of the second node}
#'   \item{Value}{The width of the edge between N1 and N2}
#' }
#' If an ID column is absent, it will be generated from N1 and N2 by
#' joining the N1 and N2 ID's with the "->" string.
#' Additionaly, the data frame may contain style information.
#' Any \var{NA} values are ignored (not entered into the riverplot object).
#' }
#'
#'
#' @section Riverplot styles:
#'
#' Styles are lists containing attributes (such as "col" for color or
#' "nodestyle") and values. There is no real difference between node and edge
#' styles, except that some attributes only apply to nodes or edges. See
#' \code{\link{riverplot-styles}} for more information on style attributes.
#'
#' When \code{makeRiver} generates the riverplot
#' object, it combines style information from the following sources in the
#' following order:
#' \itemize{
#' \item parameter \var{default_style} is a style applied to all nodes and edges
#' \item if the parameter \var{nodes} and/or \var{edges} is a data frame, it
#' may include columns with names corresponding to style attributes. For
#' example, a column called "col" will contain the color attribute for any
#' nodes / edges. \var{NA} values in these columns are ignored.
#' \item \var{styles} is a lists of styles, with
#' names corresponding to node IDs or edge IDs, which will replace any
#' previously specified styles.
#' }
#'
#' @param edges A named list or a data frame specifying the edges between the
#'        nodes.
#' @param nodes Data frame with node ID's, positions and optionally other
#'        information
#' @param node_xpos A named vector of numeric values specifying 
#'        the horizontal positions on the plot.
#' @param node_ypos A named vector of numeric values specifying 
#'        the vertical positions on the plot.
#' @param node_labels A named character vector of labels for the nodes
#' @param node_styles Deprecated
#' @param edge_styles Deprecated
#' @param styles A named list specifying the styles for the nodes and edges
#' @param default_style list containing style information which is applied
#'        to every node and every edge
#' @return A riverplot object which can directly be plotted.
#' @examples
#' nodes <- c( LETTERS[1:3] )
#' edges <- list( A= list( C= 10 ), B= list( C= 10 ) )
#' r <- makeRiver( nodes, edges, node_xpos= c( 1,1,2 ),
#'   node_labels= c( A= "Node A", B= "Node B", C= "Node C" ),
#'   node_styles= list( A= list( col= "yellow" )) )
#' plot( r )
#'
#' # equivalent form:
#' nodes <- data.frame( ID= LETTERS[1:3],
#'                x= c( 1, 1, 2 ),
#'                col= c( "yellow", NA, NA ),
#'                labels= c( "Node A", "Node B", "Node C" ),
#'                stringsAsFactors= FALSE )
#' r <- makeRiver( nodes, edges )
#' plot( r )
#' # all nodes but "A" will be red:
#' r <- makeRiver( nodes, edges, default_style= list( col="red" ) )
#' plot( r )
#' # overwrite the node information from "nodes":
#' r <- makeRiver( nodes, edges, node_styles= list( A=list( col="red" ) ) )
#' plot( r )
#' @author January Weiner
#' @export

makeRiver <- function( nodes, edges, 
                       node_labels= NULL, 
                       node_xpos= NULL, node_ypos=NULL, 
                       styles=NULL,
                       node_styles= NULL, edge_styles= NULL, 
                       default_style= NULL ) {
  ret <- list()

  if( is.null( nodes ) ) stop( "nodes cannot be NULL" )
  if( class( nodes ) == "character" ) {
    nodes <- unique( nodes )
    nodes <- data.frame( ID=as.character(nodes), x= 1:length( nodes ) )
  } else if( class( nodes ) %in% c( "matrix", "data.frame" ) ) {
    nodes <- data.frame(nodes)
    if( is.null( nodes[, "ID" ] ) ) 
      stop( "nodes must have an ID column" )
    nodes$ID <- as.character(nodes$ID)
  } else {
    stop( "Incorrect type for nodes object" )
  }

  rownames( nodes ) <- nodes$ID

  if( ! is.null( node_xpos ) ) {
    if( ! is.numeric( node_xpos ) ) 
      stop( "node_xpos must be numeric vector" )
    nodes$x <- node_xpos
  }

  if( ! is.null( node_ypos ) ) {
    if( ! is.numeric( node_ypos ) ) 
      stop( "node_ypos must be numeric vector" )
    nodes$y <- node_ypos
  }

  if( ! is.null( node_labels ) ) {
    if( class( node_labels ) != "character" )
      stop( "node_labels must be a character vector" )
    nodes$labels <- node_labels
  }

  nnames    <- nodes$ID
  #print( nodes )

  # check the edges
  if( class( edges ) == "list" )
    edges <- edgelist2df( edges )

  if( class( edges ) != "data.frame"
    || is.null( names( edges ) ) )
    stop( "Incorrect edges parameter" )

  # check the edges
  edges       <- checkedges(edges, nodes$ID)
 
  # check whether styles is sane
  if( !is.null( node_styles ) ) {
    if( class( node_styles ) != "list"  ||
        is.null( names( node_styles ) ) )
      stop( "node_styles must be a named list" )
  } 

  # check whether styles is sane
  if( !is.null( edge_styles ) ) {
    if( class( edge_styles ) != "list"  ||
        is.null( names( edge_styles ) ) )
      stop( "edge_styles must be a named list" )
  } 

  # check whether styles is sane
  if( !is.null( styles ) ) {

    if(class(styles) != "list"  ||
        is.null(names(styles))) {
      warning( "style must be a named list" )
      styles <- NULL
    }
      #stop(paste0(styles))
  } 
  # add the style information from nodes and edges
  node_styles <- mergestyles( readStyleCols( nodes ), node_styles )
  edge_styles <- mergestyles( readStyleCols( edges ), edge_styles )
  styles <- mergestyles(edge_styles, mergestyles(node_styles, styles))
 
  if( ! is.null( default_style ) ) {
    if( ! "list" %in% class( default_style ) ) stop( "default_style must be a list" )
    # update styles
    for( n in c( nodes$ID, edges$ID ) ) {
      ret$styles[[n]] <- getstyle( ret$styles[[n]], default_style )
    }
  }

  ret$edges       <- edges
  ret$nodes       <- nodes
  ret$styles <- mergestyles(ret$styles, styles)

  #ret$node_ypos <- node_ypos
  class( ret ) <- c( class( ret), "riverplot" )
  return(ret)
}

# convert node_edge in list format to data frame
edgelist2df <- function( edges ) {

  if( is.null( names( edges ) ) ) 
    stop( "Incorrect edges parameter" )
  pairs <- NULL
  ids   <- c()
  vals  <- c()

  for( n1 in names( edges ) ) {
    for( n2 in names( edges[[n1]] ) ) {
      pairs <- rbind( pairs, c( n1, n2 ) )
      ids   <- c( ids, paste0( n1, "->", n2 ) )
      vals  <- c( vals, edges[[n1]][[n2]] )
    }
  }

  ret <- data.frame( ID= ids, N1= pairs[,1], N2= pairs[,2], Value= vals, stringsAsFactors= FALSE )
  rownames( ret ) <- ret$ID
  #print( ret )
  return( ret )
}


## check the nodes data frame
checknodes <- function(nodes) {

  if(is.null(nodes)) 
    stop("checknodes: node object missing")

  if(is.null(nodes$ID))
    stop("checknodes: node ID column missing")

  # check for duplicate IDs
  if(any(duplicated(nodes$ID))) {
    ndups <- sum(duplicated(nodes$ID))
    warning(sprintf("checknodes: duplicate node IDs found, removing %d nodes", ndups))
    nodes <- nodes[!duplicated(nodes$ID), ]
  }

  # rownames and IDs must be identical
  rownames(nodes) <- nodes$ID
  return(nodes)
}


## check whether the x$edges object is sane
checkedges <- function(edges, nnames) {
  # mandatory columns present?
  if(ncol(edges) < 3 
       || ! all( c("N1", "N2", "Value") %in% colnames(edges)))
    stop("edges must have the columns N1, N2 and Value")

  # autogenerate IDs if necessary
  if(!"ID" %in% colnames(edges)) {
    edges$ID <- paste0(edges$N1, "->", edges$N2)
  } else {
    edges$ID <- as.character(edges$ID)
  }

  # make sure that node and edge names don't clash. Not the best
  # solution...
  if(any(edges$ID %in% nnames))
    stop( "edges must not have the same IDs as nodes" )

  # all edges between nodes which are known?
  all.edges <- c(as.character(edges$N1), as.character(edges$N2))
  if(! all(all.edges %in% nnames)) {
    sel <- (! as.character(edges$N1) %in% nnames) | (! as.character(edges$N2) %in% nnames )
    n <- sum(sel)
    tmp <- paste(unique(all.edges[!all.edges %in% nnames]), collapse="\n")

    warning( sprintf( "unknown nodes present in the edges parameter, removing %d edges\nUnknown nodes:\n%s", n, tmp ) )
    edges <- edges[ ! sel, ]
  }

  # search for duplicated *pairs*
  tmp <- t( apply( edges[ , c( "N1", "N2" ) ], 1, sort ) )
  sel <- duplicated( tmp )
  if( any( sel ) ) {
    n <- sum( sel )
    warning( sprintf( "duplicated edge information, removing %d edges ", n) )
    edges <- edges[ ! sel, ]
  }

  # edge ID's must not be duplicated
  if(any(duplicated(edges$ID))) {
    n <- sum(duplicated(edges$ID))
    warning(sprintf("duplicated edge information, removing %d edges ", n))
    edges <- edges[ ! duplicated( edges$ID ), ]
  }

  # edges must have a numeric value
  if(any(is.na(edges$Value))) {
    edges$Value <- as.numeric(edges$Value)
    n <- sum(is.na(edges$Value))
    warning(sprintf( "NA's (or non-numeric values) in edges, removing %d edges", n))
    edges <- edges[ ! is.na( edges$Value ), ]
  }

  if(nrow(edges) == 0) stop( "No edges left to draw" )
  rownames(edges) <- edges$ID
  return(edges)
}

## check nodes and edges of a riverplot object
checkriverplot <- function(x) {

  if(!is(x, "riverplot")) {
    if(!is.list(x)) stop("checkriverplot: x must be a list or a riverplot object")
    warning("checkriverplot: converting x to riverplot object")
    class(x) <- "riverplot"
  }

  if(is.null(x$nodes$ID)) x$nodes$ID <- paste0("N.", 1:nrow(x$nodes))
  if(is.null(x$edges$ID)) x$edges$ID <- paste0("E.", 1:nrow(x$edges))

  x$nodes$ID <- as.character(x$nodes$ID)

  x$nodes <- checknodes(x$nodes)
  x$edges <- checkedges(x$edges, names(x))

  x
}

df2conn <- function( edges ) {

  if( ncol( edges ) < 3 
       || ! all( c( "N1", "N2", "Value" ) %in% colnames( edges ) ) )
    stop( "edges must have the columns N1, N2 and Value" )

  ret <- list()

  for( i in 1:nrow( edges ) ) {

    n1 <- edges[ i, "N1" ]
    n2 <- edges[ i, "N2" ]
    v  <- edges[ i, "Value" ]
    if( is.null( ret[[n1]] ) ) ret[[n1]] <- list()
    ret[[n1]][[n2]] <- v
  }

  return( ret )
}



# convert column-codes styles into a list
readStyleCols <- function( df ) {

  cnames <- colnames( df )
  ds <- names( default.style() ) 
  ids <- df$ID

  styles <- NULL
  for( k in intersect( ds, cnames ) ) {
    #print( k )
    if( is.null( styles ) ) styles <- list()

    v <- df[ , k ]
    names( v ) <- ids
    v <- v[ ! is.na( v ) ]

    for( n in names( v ) ) {
      if( is.null( styles[[n]] ) ) {
        styles[[n]] <- list()
      }
      styles[[n]][[k]] <- v[n]
    }

  }

  return( styles )
}


##' @rdname makeRiver
##' @export
#makeRiverFromSource <- function() {
#  ret <- list()
# 
# 
# 
#  class( ret ) <- c( class( ret), "riverplot" )
#}


