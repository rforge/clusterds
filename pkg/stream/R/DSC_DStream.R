#######################################################################
# stream -  Infrastructure for Data Stream Mining
# Copyright (C) 2013 Michael Hahsler, Matthew Bolanos, John Forrest 
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along
# with this program; if not, write to the Free Software Foundation, Inc.,
# 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.


DStream <- setRefClass("DStream",
  fields = list(
    ### dimensionality
    d	              = "integer",
    ### this is a vector of length d (size of a grid cell)
    gridsize		    = "numeric",
    ### attraction boundary (FIXME: Needs to be implemented)
    epsilon		      = "numeric",
    ### decay (note lambda is different than in the paper!)
    lambda			    = "numeric",
    gaptime         = "integer",
    attraction      = "logical",
    
    ### dense grid threshold Cm > 1 -> Dm = Cm/(N*(1-decay_factor))
    Cm              = "numeric",
    ### sparse grid threshold 0<Cl<1 -> Dl = Cl/(N*(1-decay_factor))
    Cl              = "numeric",
    ### other grid types
    ### transitional grid Cl < d < Cm
    ### sporadic grid pi = (Cl * (1-decay_factor))/(N*(1-decay_factor))
    
    
    ### store the grid
    grid	 		      = "hash",
    npoints         = "integer",
    decay_factor		= "numeric",
    mins            = "numeric", ### we need mins and maxs to get N
    maxs            = "numeric"
  ),
  
  methods = list(
    initialize = function(
      gridsize = 0.1,
      d = NA_integer_,
      lambda = 1e-3,
      epsilon = gridsize*.25,
      gaptime = 1000L,
      Cm = 1,
      Cl = .5,
      attraction = FALSE
    ) {
      
      d  <<- d
      gridsize <<- gridsize
      epsilon <<- epsilon
      lambda <<- lambda
      gaptime	<<- gaptime
      Cm <<- Cm
      Cl <<- Cl
      attraction <<- attraction
      
      if(attraction) stop ("attraction not implemented yet!")
      
      grid  <<- hash()
      npoints <<- 0L
      ### this is what the paper calls lambda!
      decay_factor <<- 2^(-lambda)
      mins <<- NA_real_
      maxs <<- NA_real_
      
      .self
    }
  )
)


  DSC_DStream <- function(gridsize = 0.1, d=NA_integer_, lambda = 1e-3, 
    gaptime=1000L, Cm=1, Cl=.5, attraction=FALSE, epsilon=gridsize*.25) {
  
  dstream <- DStream$new(gridsize, as.integer(d), lambda, epsilon, 
    as.integer(gaptime), Cm, Cl, attraction)
  l <- list(description = "DStream", RObj = dstream)
  class(l) <- c("DSC_DStream", "DSC_Micro", "DSC_R", "DSC")
  l
}

DStream$methods(list(
  cluster = function(newdata, debug = FALSE) {
    'Cluster new data.' ### online help
    
    if(debug) cat("Debug cluster for DStream\n")
    
    newdata <- as.matrix(newdata)
    
    ### first data point
    if(is.na(d)) {
      d <<- ncol(newdata)
      if(length(gridsize) != d) gridsize <<- rep(gridsize, d)
    }
    
    for(i in 1:nrow(newdata)) {
      
      npoints <<- npoints + 1L
      
      ### decay and remove sporadic grids
      if(decay_factor<1 && !npoints%%gaptime) {
        if(length(grid)>0) {
          values(grid) <<- vg <- values(grid) * decay_factor^gaptime
          ### remove sporadic grids
          N <- prod(maxs-mins+1L)
          remove <- vg < Cl*(1-decay_factor)/N/(1-decay_factor)
          if(debug) cat("Removing ", sum(remove) ," sporadic grids\n")
          for (k in keys(grid)[remove]) grid[[k]] <<- NULL
        }
      } 
      
      # find grid cell and insert point
      grid_id <- floor(newdata[i,]/gridsize)
      key <- paste(grid_id, collapse=":")
      
      val <- grid[[key]]
      if(is.null(val)) grid[[key]] <<- 1
      else grid[[key]] <<- val+1
      
      ### update maxs/mins
      maxs <<- apply(cbind(maxs, grid_id), MARGIN=1, max, na.rm=TRUE)
      mins <<- apply(cbind(mins, grid_id), MARGIN=1, min, na.rm=TRUE)

      if(debug && !i%%100) cat("Processed",i,"points\n")
      
    }
  },
  
  ### FIXME: make this toArray!
  toMatrix = function(type=c("dense", "transitional", "all")) {
    type <- match.arg(type)

    coords <- get_micro(weight=TRUE, translate=FALSE, type=type)
    
    ns <- (maxs-mins)+1L
    mat <- matrix(NA, nrow=ns[1], ncol=ns[2])
    for(i in 1:nrow(coords)) {
      mat[coords[i,1]-mins[1]+1L, coords[i,2]-mins[2]+1L] <- coords[["weight"]][i]
    }
    
    rownames(mat) <- (mins[1]:maxs[1]) * gridsize[1]+gridsize[1]/2
    colnames(mat) <- (mins[2]:maxs[2]) * gridsize[2]+gridsize[2]/2
    mat
  },
  
  get_micro = function(weight=FALSE, translate=TRUE, 
    type=c("dense", "transitional", "all")) {
    type <- match.arg(type)
    
    if(translate) {
      coords <- as.data.frame(t(sapply(keys(grid), 
        FUN=function(y) as.numeric(unlist(strsplit(y, ':'))), 
        USE.NAMES=FALSE)*gridsize+gridsize/2))
    }else{
      coords <- as.data.frame(t(sapply(keys(grid), 
        FUN=function(y) as.numeric(unlist(strsplit(y, ':'))),
        USE.NAMES=FALSE)))
    }
    
    vals <- values(grid)
    
    N <- prod(maxs-mins+1L)
    
    ### add missing decay
    vals <- vals*decay_factor^(npoints%%gaptime)
    
    if(type=="transitional") {
      ### sparse grid threshold 0<Cl<1 -> Dl = Cl/(N*(1-decay_factor))
      take <- vals > Cl/N/(1-decay_factor)
      coords <- coords[take,]
      vals <- vals[take]
    } else if(type=="dense") {
      ### dense grid threshold Cm > 1 -> Dm = Cm/(N*(1-decay_factor))
      take <- vals > Cm/N/(1-decay_factor)
      coords <- coords[take,]
      vals <- vals[take]
    }
      
    if(weight) coords[["weight"]] <- vals
    coords
  }
)
)

get_microclusters.DSC_DStream <- function(x) 
  x$RObj$get_micro(weight=FALSE, type="trans")

get_microweights.DSC_DStream <- function(x) 
  x$RObj$get_micro(weight=TRUE, type="trans")[["weight"]]

plot.DSC_DStream <- function(x, ..., image=FALSE) {
  if(!image) return(plot.DSC(x, ...))
  
  if(x$RObj$d!=2) stop("Image visualization only works for 2D data!") 
  
  mat <- x$RObj$toMatrix("transitional")
  
  image(mat, col=gray.colors(256), axes=FALSE)
  box()
  axis(side=1, labels=colnames(mat), at =seq(0,1, length.out=ncol(mat)))
  axis(side=2, labels=rownames(mat), at =seq(0,1, length.out=nrow(mat)))
}

