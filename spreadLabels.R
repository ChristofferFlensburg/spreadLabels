#Takes a set of points with coordinates x, and y, of which the logical vector toLabel
#denotes the points to be labeled. xR and yR are the size (the radius) of the labels,
#used for collision ellipse.
spreadLabels = function(x, y, xR, yR, toLabel, verbose=T) {
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

  return(labelPos)
}

