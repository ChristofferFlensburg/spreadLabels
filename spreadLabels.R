#Takes a set of points with coordinates x, and y, of which the logical vector toLabel
#denotes the points to be labeled. xR and yR are the size (the radius) of the labels,
#used for collision ellipse.
#returns a matrix with label x-values in row 1 and label y-values in row 2.



#' labelspreader
#' 
#' Determines clever placement of labels for plots made in base R
#' 
#' 
#'
#' @param x The set of x coordinates for the points to plot 
#' @param y The set of y coordinates for the points to plot 
#' @param xR Numeric value, corresponding to the radius size in the X direction 
#' for the labels, used for collision ellipse []need to specify the unit!
#' @param yR Numeric value, corresponding to the radius size in the Y direction 
#' for the labels, used for collision ellipse []need to specify the unit!
#' @param toLabel Vector of logical values, of the same length of 
#' @param verbose Logical, whether to report the intermediate output of running
#' the function
#'
#' @return A data.frame with 2 columns, \code{xlabs_coord} and \code{ylabs_coord}, 
#' to be used downstream as an input e.g. to \code{text}
#' @export
#'
#' @examples
#' library(ggplot2)
#' diamonds
#' set.seed(42)
#' minidiamonds <- diamonds[sample(1:50000,size = 100),]
#' plot(minidiamonds$carat, minidiamonds$price)
#' mypos <- spreadLabels(minidiamonds$carat, minidiamonds$price,xR=5,yR=5,toLabel=rep(TRUE,100))
#' text(labels = minidiamonds$clarity,x=mypos$xlabs_coord,y=mypos$ylabs_coord,pos = 1)
spreadLabels <- function(x, y, xR, yR, toLabel, verbose=TRUE) {
  #starting condition
  labelPos = sapply(which(toLabel), function(i) c('x'=x[i], 'y'=y[i]))
  origin = labelPos
  allX = c(labelPos[1,], x)
  allY = c(labelPos[2,], y)
  relDist = apply(labelPos, 2, function(pos) (pos[1] - allX)^2/xR^2 + (pos[2] - allY)^2/yR^2)
  overlap = relDist < 1  #maybe should use square collision box instead?
  overlap[row(overlap) == col(overlap)] = 0

  #randomly move away from locally densest area until not overlapping 
  while ( sum(overlap) > 0 ) {
    labelPos = sapply(1:ncol(labelPos), function(id) {
      dist = relDist[,id]
      dist[id] = 100
      closest = (dist < runif(1,1,6))
      posOri = labelPos[,id]
      if ( min(dist) >= 1 ) return(posOri)
      pos0 = c('x'=mean(allX[closest]), 'y'=mean(allY[closest]))
      diff = posOri - pos0
      if ( diff[1] == 0 & diff[2] == 0 ) diff = c('x'=0, 'y'=1)
      newPos = posOri + runif(1,0.1,0.5)*diff/sqrt((diff[1]/xR)^2 + (diff[2]/yR)^2)
      return(newPos)
    })

    allX = c(labelPos[1,], x)
    allY = c(labelPos[2,], y)
    relDist = apply(labelPos, 2, function(pos) (pos[1] - allX)^2/xR^2 + (pos[2] - allY)^2/yR^2)
    overlap = relDist < 1
    overlap[row(overlap) == col(overlap)] = 0

    if ( verbose ) cat(colSums(overlap), '\n')
  }

  #tighten rubber band towards starting point
  tight = F
  while ( !tight ) {
    tight=T
    for ( id in 1:ncol(labelPos) ) {
      diff = origin[,id] - labelPos[,id]
      dist = sqrt((labelPos[1,]-x[toLabel])^2+(labelPos[2,]-y[toLabel])^2)
      newPos = labelPos[,id] + 0.1*diff
      
      dists = sqrt((newPos[1] - allX)^2/xR^2 + (newPos[2] - allY)^2/yR^2)
      dists[id] = 100
      if ( min(dists) >= 0.99 ) {
        if ( verbose ) cat('tightening', id, '\n')
        tight=F
        labelPos[,id] = newPos
        allX = c(labelPos[1,], x)
        allY = c(labelPos[2,], y)
      }
    }
  }
  
  # one thought: maybe more tidy to refer to with a df and names - would go more in line?
  labelPos <- data.frame(
    xlabs_coord = labelPos[1,],
    ylabs_coord = labelPos[2,],
    stringsAsFactors = FALSE
  )
  
  return(labelPos)
}

