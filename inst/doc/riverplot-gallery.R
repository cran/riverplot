## ----setup, echo=FALSE, results="hide", include=FALSE-------------------------
library(knitr)
library(riverplot)
opts_chunk$set(cache=FALSE, autodep=TRUE, tidy=FALSE, fig.width=5, warning=FALSE, fig.height=5)
opts_knit$set(width=75)

## ----export,fig.width=10,fig.height=7-----------------------------------------
options(stringsAsFactors=FALSE)
## goods exports / imports for 2015 in billions
edges <- read.csv(text="N1,N2,Value
From EU,To China,170.4
From China,To EU,350.6
From EU,To US,426.0
From US,To EU,272.7
From China,To US,482
From US,To China,116", 
header=T, stringsAsFactors=FALSE)
print(edges)

nodes <- data.frame(ID=unique(c(edges$N1, edges$N2)),
    x=rep(c(1,2), each=3), y=c(1,2,3,2,1,3)) 

cols <- c(China="#00990066",
          EU   ="#00009966",
          US   ="#99000066")

style <- sapply(nodes$ID, function(id)
  list(col=cols[ gsub("(From|To) ", "", id) ]), simplify=FALSE)

r <- makeRiver(nodes=nodes, edges=edges, styles=style)
par(bg="grey98")
d <- list(srt=0, textcex=1.5) # default style
plot(r, plot_area=1, nodewidth=10, default_style=d)

## ----export2,fig.width=10,fig.height=7----------------------------------------
eu <- c( "Austria", "Italy", "Belgium", "Latvia", "Bulgaria",
"Lithuania", "Croatia", "Luxembourg", "Cyprus", "Malta", "Czech Republic",
"Netherlands", "Denmark", "Poland", "Estonia", "Portugal", "Finland", "Romania",
"France", "Slovakia", "Germany", "Slovenia", "Greece", "Spain", "Hungary", "Sweden",
"Ireland", "United Kingdom")

library(maptools)
data(wrld_simpl)
m <- wrld_simpl[ wrld_simpl@data$NAME != "Antarctica", ]

nodes$labels <- gsub(" .*", "", nodes$ID)
nodes$x <- c( 15, 106, -101, 
             106,  15, -101)

nodes$y <- c(53, 21, 49, 
             40, 63, 29 )
ccol <- rep("grey", nrow(m@data))
ccol[ m@data$NAME == "China" ] <- cols["China"]
ccol[ m@data$ISO2 == "US" ] <- cols["US"]
ccol[ m@data$NAME %in% eu ] <- cols["EU"]

r <- makeRiver(nodes=nodes, edges=edges, styles=style)
plot(m, border=F, col=ccol)
d <- list(srt=0, textcex=1.5)
plot(r, add=T, rescale=F, yscale=0.02, default_style=d, nodewidth=5)


## ----minard,fig.width=10,fig.height=7-----------------------------------------
library(riverplot)
data(minard)
nodes <- minard$nodes
edges <- minard$edges
colnames(nodes) <- c("ID", "x", "y")
colnames(edges) <- c("N1", "N2", "Value", "direction")

# color the edges by troop movement direction
edges$col <- c("#e5cbaa", "black")[factor(edges$direction)]

# color edges by their color rather than by gradient between the nodes
# The "edgecol" column is interpreted as a style keyword with value "col"
edges$edgecol <- "col"

# generate the riverplot object and a style
river <- makeRiver(nodes, edges)
style <- list(edgestyle= "straight", nodestyle= "invisible")

# plot the generated object. Given that we want to plot the cities as well
# (external data), the user coordinates for the plot and for the external
# data should be the same. This is achieved by the adjust.usr option.
# Alternatively, one can call plot.new, set usr manually and call riverplot
# with the options rescale=FALSE and add=TRUE.
# plot_area parameter is for creating suitable margins within the plot area
par(bg="grey98", mar=rep(3,4))
plot(river, lty=1, default_style=style, plot_area=c(0.9, 0.7), adjust.usr=TRUE)
u <- par("usr")
rect(u[1], u[3], u[2], u[4])

## add latitude and longitude
abline(h=54:56, col="grey")
bglabel(u[1], 54:56, sprintf("%d°N", 54:56), pos="topright", bg=NA, col="grey", font=3)
lbl <- seq(20, 40, by=5)
abline(v=lbl, col="grey")
bglabel(lbl, u[3], sprintf("%d°E", lbl), pos="topright", bg=NA, col="grey", font=3)

## Add cities. Use "label()" to have a background frame and better
## positioning.
with(minard$cities, points(Longitude, Latitude, pch=19))
with(minard$cities, bglabel(Longitude, Latitude, Name, pos="topright"))



## ----ex1_0--------------------------------------------------------------------
library(riverplot)
options(stringsAsFactors=FALSE)
nodes <- data.frame(ID=LETTERS[1:7], x=c(1, 1, 1, 1, 2, 3, 3))
edges <- data.frame(ID=paste0("E.", letters[1:6]),
        N1=c("A", "B", "C", "D", "F", "G"),
        N2=c("E", "E", "E", "E", "E", "E"),
        Value=c(10, 40, 20, 5, 25, 50))
par(mar=rep(0,4))
r <- makeRiver(nodes, edges)
plot(r)

## ----ex1_1--------------------------------------------------------------------
nodes2 <- nodes[7:1,]
r2 <- makeRiver(nodes2, edges)
plot(r2)

## ----ex1_xy-------------------------------------------------------------------
r2 <- plot(r)
str(r2$nodes) # nodes contain now column "y"
r2$nodes["E", "y"] <- 1
plot(r2)

## ----ex1_gravity,fig.width=10,fig.height=4------------------------------------
par(mfrow=c(1,3))
plot(r, gravity="b") 
plot(r, gravity="c")
plot(r, gravity="t") # default

## ----ex1_node_margin,fig.width=10,fig.height=4--------------------------------
par(mfrow=c(1,3))
plot(r, node_margin=0.1) # default
plot(r, node_margin=0.5)
plot(r, node_margin=0.9) 

## ----ex1_yscale,fig.width=10,fig.height=4-------------------------------------
par(mfrow=c(1,3))
plot(r2, yscale=0.1)
plot(r2, yscale=0.5)
plot(r2, yscale=0.9)

## ----ex1_plotarea,fig.width=10,fig.height=4-----------------------------------
par(mfrow=c(1,3))
par(mar=rep(1,4))
plot(r, plot_area=c(1, 0.5), bty="o") # default
plot(r, plot_area=1, bty="o") # max area in both directions
plot(r, plot_area=c(0.5, 1), bty="o") # squeezed laterally

## ----ex1_usr------------------------------------------------------------------
plot(NULL, xlim=c(1,10), ylim=c(1,10), bty="n", xlab="", ylab="")
abline(h=1:10, col="grey")
abline(v=1:10, col="grey")
plot(r, add=TRUE, usr=c(2,8,2,8), plot_area=1, bty="o")
plot(r, add=TRUE, usr=c(1,4,1,4), plot_area=1, bty="o")
plot(r, add=TRUE, usr=c(7,12,7,12), plot_area=1, bty="o")

## ----ex1_adjust.usr,fig.width=12,fig.height=6---------------------------------
par(mfrow=c(1,2))
nodes2 <- nodes[order(nodes$ID),]
nodes2$y <- c(50*(1:4),125,100,150)
r2 <- makeRiver(nodes2, edges)
plot(r2, adjust.usr=TRUE, plot_area=c(0.8,0.8))
points(nodes2$x, nodes2$y, cex=3)
axis(side=1)
axis(side=2)
plot(r2, adjust.usr=TRUE, plot_area=c(0.8,0.8), yscale=0.5)
points(nodes2$x, nodes2$y, cex=3)

