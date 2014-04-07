#######################################################################
# Moving Generator -  Infrastructure for Moving Streams
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

add_cluster <- function(x, c) 
  UseMethod("add_cluster")
get_clusters <- function(x) 
  UseMethod("get_clusters")
remove_cluster <- function(x, i) 
  UseMethod("remove_cluster")

dsd_MG_refClass <- setRefClass("dsd_MG", 
  fields = list(
    t = "numeric",
    dimension = "numeric",
    clusters = "list",
    numberPerStep = "numeric"
  ),
  methods = list(
    initialize = function(d) {
      dimension  <<- d
      t <<- 1
      .self
    }
    
  ),
)

dsd_MG_refClass$methods(
  add_cluster = function(c) {
    clusters <<- append(clusters, list(c))
  },
  get_points = function(n,assignment = FALSE) {
    j <- 0
    data <- numeric()
    a <- numeric()
    
    while(j < n) {
      attributes <- do.call(rbind, lapply(clusters,function(x){x$RObj$get_attributes(t)}))
      
      cluster <- unlist(attributes[,1])
      density <- unlist(attributes[,2])
      
      pointsPerSecond <- sum(density)
      pointsLeftInSecond <- pointsPerSecond - (t - floor(t))*pointsPerSecond
      if((j + pointsLeftInSecond) > n)
        k <- n-j
      else 
        k <- pointsLeftInSecond
      t <<- t + k/pointsPerSecond
      
      if(k>=1) {
        s <- 1:length(clusters)
        prob <- density
        clusterOrder <- sample(x=s,size=k, replace=TRUE, prob=prob)
        
        data <- rbind(data,t(sapply(clusterOrder, FUN = function(i) {
            clusters[[i]]$RObj$get_points(t)
        })))
       
        a <- c(a,unlist(lapply(clusterOrder,function(x){
          if(!is.na(cluster[x]) && cluster[x] == 0) return(0)
          if (any(is.na(cluster))) return(x)
          cluster[x]
        })))
        j <- nrow(data)
      }
    }
    data <- data.frame(data)
    if(assignment)
      attr(data,"assignment") <- a
    data[1:n,]
  }
)

### creator    
DSD_MG<- function(dimension = 2, ...) {
  
  desc <- "Moving Data Generator"
  
  x <- structure(list(description = desc,
                 RObj = dsd_MG_refClass$new(d = dimension)),
            class = c("DSD_MG","DSD_R","DSD"))
  
  lapply(list(...),function(c){x$RObj$add_cluster(c)})
  
  x
}

get_points.DSD_MG <- function(x, n=1, assignment = FALSE,...) {
  x$RObj$get_points(n,assignment)
}

add_cluster.DSD_MG <- function(x, c) {
  x$RObj$add_cluster(c)
}

reset_stream.DSD_MG <- function(dsd) {
  dsd$RObj$t <- 1
}

print.DSD_MG <- function(x, ...) {
  cat(paste(x$description, " (", paste(class(x), collapse=", "), ")", '\n', sep=""))
  cat(paste('With', length(x$RObj$clusters), 'clusters', 'in', x$RObj$dimension, 'dimensions', '\n'))
}

get_clusters.DSD_MG <- function(x) {
  x$RObj$clusters
}

remove_cluster.DSD_MG  <- function(x,i) {
  x$RObj$clusters[[i]] <- NULL
}
