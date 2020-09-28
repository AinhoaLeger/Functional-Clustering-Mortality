
#-----------------------------------------------------------------------
# Image Plot
# This function is a modified version of myImagePlot function from 
# Chris Seidel and available on http://www.phaget4.org/R/myImagePlot.R
#-----------------------------------------------------------------------

 myImagePlot <- function(x, colors, ...){

 # the first argument is the data matrix
 # the second argument is the colors of the clusters as arguments

     min <- min(x)
     max <- max(x)
     yLabels <- rownames(x)
     xLabels <- colnames(x)
     title <-c()

  # Check for additional function arguments
  if( length(list(...)) ){
    Lst <- list(...)
    if( !is.null(Lst$zlim) ){
       min <- Lst$zlim[1]
       max <- Lst$zlim[2]
    }
    if( !is.null(Lst$yLabels) ){
       yLabels <- c(Lst$yLabels)
    }
    if( !is.null(Lst$xLabels) ){
       xLabels <- c(Lst$xLabels)
    }
    if( !is.null(Lst$title) ){
       title <- Lst$title
    }
  }

  # Check for null values
  if( is.null(xLabels) ){
   xLabels <- c(1:ncol(x))
  }
  if( is.null(yLabels) ){
   yLabels <- c(1:nrow(x))
  }

  layout(matrix(data=c(1,2), nrow=1, ncol=2), widths=c(4,1), heights=c(1,1))

  # Set color range
  ColorRamp <- c("gray98",colors)
  ColorLevels <- seq(min, max, length=length(ColorRamp))

  # Reverse Y axis
  reverse <- nrow(x) : 1
  yLabels <- yLabels[reverse]
  x <- x[reverse,]

  # Data Map
  par(mar = c(3,5,2.5,2))
  image(1:length(xLabels), 1:length(yLabels), t(x), col=ColorRamp,
	xlab="", ylab="", axes=FALSE, zlim=c(min,max))
  if( !is.null(title) ){
    title(main=title)
  }
  axis(BELOW<-1, at=1:length(xLabels), labels=xLabels, cex.axis=0.8)
  axis(LEFT <-2, at=1:length(yLabels), labels=yLabels, las= HORIZONTAL<-1,
  cex.axis=0.8)
  abline(h=c(5.5,11.5,16.5,19.5,28.5),v=c(11.5,21.5,31.5,41.5,51.5),lty=2)

  # Color and Text Scale
  # default: par(mar = c(3,2.5,2.5,2))
  par(mar = c(3,2.5,2.5,2))
  image(1, ColorLevels[-1],
      matrix(data=ColorLevels[-1], ncol=length(ColorLevels[-1]),nrow=1),
      col=ColorRamp[-1], xlab="", ylab="", xaxt="n", yaxt="n")
  if(max(x)==3){zLabels <- c("Cluster 1","Cluster 2","Cluster 3")}
  if(max(x)==4){zLabels <- c("Cluster 1","Cluster 2","Cluster 3",
		"Cluster 4")}
  if(max(x)==5){zLabels <- c("Cluster 1","Cluster 2","Cluster 3",
		"Cluster 4","Cluster 5")}
  if(max(x)==6){zLabels <- c("Cluster 1","Cluster 2","Cluster 3",
		"Cluster 4","Cluster 5","Cluster 6")}
  if(max(x)==7){zLabels <- c("Cluster 1","Cluster 2","Cluster 3",
		"Cluster 4","Cluster 5","Cluster 6","Cluster 7")}
  axis(LEFT <- 2, at=1:length(zLabels), labels=zLabels, las=HORIZONTAL<-1,
  cex.axis=0.8)

  layout(1)
  }




