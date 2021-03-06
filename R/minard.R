#' Minard Napoleon Russian campaign data
#'
#' The data set used by Charles Joseph Minard to generate the
#' famous graph. The example below shows how to recreate the
#' main panel of the graph using riverplot from the provided data.
#' 
#' First, node and edge data frames must get new column names (see
#' \code{\link{makeRiver}} function for details). Then, based on the
#' direction of the Napoleon army, style information (right and left edge
#' color style for each node) is entered in the
#' \var{nodes} variable. Then, a riverplot object is generated from the
#' nodes and edges data frames.
#'
#' To use the same color coding as Minard, the \var{direction} variable is
#' converted to color codes in the \var{col} column of the
#' \var{edges} object.
#'
#' Finally, a plot is created using \code{lty=1} and a style in which nodes
#' are not shown, and the edges are straight (like in the original Minard
#' plot) rather than curved.
#'
#' @format Named list with two data frames: 
#' \describe{
#' \item{nodes}{data frame with geographic locations of the Napoleon army
#' (longitude and latitude) and the direction of the march}
#' \item{edges}{connections between positions}
#'}
#' @source Charles Joseph Minard
#' @rdname minard
#' @examples
#' # example how to convert data into a riverplot object
#' data(minard)
#' nodes <- minard$nodes
#' edges <- minard$edges
#' colnames(nodes) <- c("ID", "x", "y")
#' colnames(edges) <- c("N1", "N2", "Value", "direction")
#'
#' # color the edges by troop movement direction
#' edges$col <- c("#e5cbaa", "black")[factor(edges$direction)]
#'
#' # color edges by their color rather than by gradient between the nodes
#' # The "edgecol" column is interpreted as a style keyword with value "col"
#' edges$edgecol <- "col"
#'
#' # generate the riverplot object and a style
#' river <- makeRiver(nodes, edges)
#' style <- list(edgestyle= "straight", nodestyle= "invisible")
#'
#' # plot the generated object. Given that we want to plot the cities as well
#' # (external data), the user coordinates for the plot and for the external
#' # data should be the same. This is achieved by the adjust.usr option.
#' # Alternatively, one can call plot.new, set usr manually and call riverplot
#' # with the options rescale=FALSE and add=TRUE.
#' # plot_area parameter is for creating suitable margins within the plot area
#' par(bg="grey98", mar=rep(3,4))
#' plot(river, lty=1, default_style=style, plot_area=c(0.9, 0.7), adjust.usr=TRUE)
#' u <- par("usr")
#' rect(u[1], u[3], u[2], u[4])
#'
#' # add latitude and longitude
#' abline(h=54:56, col="grey")
#' bglabel(u[1], 54:56, sprintf("%d°N", 54:56), pos="topright", bg=NA, col="grey", font=3)
#' lbl <- seq(20, 40, by=5)
#' abline(v=lbl, col="grey")
#' bglabel(lbl, u[3], sprintf("%d°E", lbl), pos="topright", bg=NA, col="grey", font=3)
#' 
#' # Add cities. Use "bglabel()" to have a background frame and better
#' # positioning.
#' with(minard$cities, points(Longitude, Latitude, pch=19))
#' with(minard$cities, bglabel(Longitude, Latitude, Name, pos="topright"))
#' @author January Weiner
"minard"
