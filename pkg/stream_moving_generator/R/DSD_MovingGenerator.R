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

add_cluster <- function(x, time, ...) 
  UseMethod("add_cluster")

dsd_movingGenerator_refClass <- setRefClass("dsd_movingGenerator", 
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

dsd_movingGenerator_refClass$methods(
  add_cluster = function(c) {
    clusters <<- append(clusters, c$RObj)
  },
  get_points = function(n,assignment = FALSE) {
    j <- 0
    data <- numeric()
    assignment <- numeric()
    
    while(j < n) {
      attributes <- matrix(unlist(lapply(clusters,function(x){x$get_attributes(t)})),ncol=2+dimension,byrow=TRUE)
      
      pointsPerSecond <- sum(attributes[,2])
      pointsLeftInSecond <- pointsPerSecond - (t %% 1)*pointsPerSecond
      if((j + pointsLeftInSecond) > n)
        k <- n-j
      else 
        k <- pointsLeftInSecond
      t <<- t + k/pointsPerSecond
      j <- j + k
      
      if(k>0) {
        clusterOrder <- sample(x=c(1:length(clusters)), 
                               size=k, 
                               replace=TRUE, 
                               prob=attributes[,2])
        data <- rbind(data,t(sapply(clusterOrder, FUN = function(i) {
          MASS::mvrnorm(1, mu=attributes[i,1:dimension+2], Sigma=diag(attributes[i,1],dimension))
        })))
        assignment <- c(assignment,clusterOrder)
      }
    }
    attr(data,"assignment") <- assignment
    data
  }
)

### creator    
DSD_MovingGenerator<- function(dimension = 2) {
  
  desc <- "Moving Data Generator"
  
  
  structure(list(description = desc,
                 RObj = dsd_movingGenerator_refClass$new(d = dimension)),
            class = c("DSD_MovingGenerator","DSD_R","DSD"))
}

get_points.DSD_MovingGenerator <- function(x, n=1, assignment = FALSE,...) {
  x$RObj$get_points(n,assignment)
}

add_cluster.DSD_MovingGenerator <- function(x, c) {
  x$RObj$add_cluster(c)
}

reset_stream.DSD_MovingGenerator <- function(x) {
  x$RObj$t <- 1
}
