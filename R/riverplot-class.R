
print.riverplot <- function( x ) {
  cat(sprintf( "RiverPlot object with %d nodes:\n", length(names(x))))
  print(names(x))
  return(invisible(x))
}

as.matrix.riverplot <- function( x ) conn2mtx( x$conn )

names.riverplot     <- function( x )  x$nodes$ID
length.riverplot    <- function( x ) length( x$nodes )


