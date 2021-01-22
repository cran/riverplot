#' Generate an example for riverplot
#'
#' The plotting functions in the riverplot package work on an object of the
#' riverplot class. This function returns an object of the riverplot class to
#' demonstrate how such an object (which is actually a simple list) can be
#' created.
#' 
#' 
#'
#' @title Generate examples for riverplot
#'
#' @param no which example to generate
#' @author January Weiner <january.weiner@@gmail.com>
#' @examples
#' x <- riverplot.example()
#' plot( x )
#' x <- riverplot.example(no=2)
#' riverplot(x, lty=1, plot_area=1, disentangle=TRUE, 
#'    gravity="c", default_style=list(nodestyle="invisible"))
#' @importFrom RColorBrewer brewer.pal
#' @export
riverplot.example <- function(no=1) {
  
  ret <- list( 

    nodes= data.frame(
      ID=LETTERS[1:8],
      x = c( 1, 2, 2, 3, 3, 4, 5, 1 ),
      labels=LETTERS[1:8],
      #labels= c( NA, NA, "Node C", rep( NA, 4 ), "Node H" ),
      stringsAsFactors= FALSE ),

  # nodes=  c(   B=2,   A=1,   Q=1, C=2,   D=3,   E=3, F=4, G=5 ),
   styles= list( 
     A=list( 
       col=  "#00990099",
       lty=0,
       textcol= "#FFFFFF99"
       ),
     H=list(
       col= "#FF000099",
       textcol= "#FFFFFF99"
     ),
     C= list( col="purple"),
     B= list( col= "#00006699", textcol= "white" ),
     F= list( col= "yellow" ),
     G= list( col= "pink" ),
     D= list( col= "#00FF0099" )
   )
  )

  ret$edges <- data.frame(
    N1=   c( "A", "A", "A", "H", "H", "H", "C", "B", "B", "C", "C" ),
    N2=   c( "B", "C", "D", "D", "F", "G", "E", "D", "F", "D", "F" ),
    Value= c( 10,  20,   5,  10,  10,  20,  15,   5,  10,  20,  10 ),
    stringsAsFactors= F )

  class(ret) <- c(class( ret), "riverplot")
  if(no == 1) return(ret)

  # another example
  ret <- list(
    nodes=data.frame(ID=LETTERS[1:11],
    x=c(1, 2, 2, 3, 3, 4, 5, 5, 6, 6, 7),
    #y=c(3, 2, 4, 1, 5, 3, 1, 5, 2, 4, 3),
    labels=LETTERS[1:11],
    stringsAsFactors=FALSE), 
    edges=data.frame(ID=paste0("e.", LETTERS[c(1:5, 7:11)]),
                     N1=LETTERS[c(1:5, 7:11)],
                     N2="F", Value=10, stringsAsFactors=FALSE)
    )


  ret$edges <- ret$edges[c(5, 3, 1, 2, 4, 10, 8, 6, 7, 9), ]
  rownames( ret$nodes ) <- ret$nodes$ID

  ret$nodes <- ret$nodes[c("D", "F", "C", "A", "B", "E", "G", "I", "K", "J", "H"), ]

  cols <- brewer.pal(10, "Spectral")
  ret$styles <- sapply(1:10, function(i) list(col=paste0(cols[i], "99")), simplify=FALSE)
  names(ret$styles) <- LETTERS[1:11][-6]
  ret$styles[["F"]] <- list(col="white")


  class( ret ) <- c( class( ret), "riverplot" )
  return( ret )
}


