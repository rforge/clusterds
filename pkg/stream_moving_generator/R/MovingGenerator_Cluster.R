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

add_keyframe <- function(x, time, ...) 
  UseMethod("add_keyframe")
get_attributes <- function(x, time, ...) 
  UseMethod("get_attributes")

movingGenerator_cluster_refClass <- setRefClass("movingGenerator_cluster", 
                               fields = list(
                                 keyframes = "data.frame",
                                 dimension = "numeric"
                               ), 
                               
                               methods = list(
                                 initialize = function(d = 2) {
                                   dimension  <<- d
                                   keyframes	<<- data.frame(t =  numeric(0),v = numeric(0), d = numeric(0))
                                   lapply(1:d,function(x){
                                     keyframes[paste("X",x,sep="")] <<- numeric(0)
                                   })
                                   .self
                                 }
                                 
                               ),
)

movingGenerator_cluster_refClass$methods(
  add_keyframe = function(t,v,d,...) {
   keyframes <<- rbind(keyframes,setNames(as.list(c(t,v,d,...)), c("time","variance","density",paste("X",1:dimension,sep=""))))
   keyframes <<- keyframes[with(keyframes, order(time)), ]
  },
  get_attributes = function(time) {
    x <- matrix(get_attribute(time,c("variance","density",paste("X",1:dimension,sep=""))),ncol=(dimension+2),byrow=TRUE)
    colnames(x) <- c("variance","density",paste("X",1:dimension,sep=""))
    x
  },
  get_attribute = function(time,attributes) {
    unlist(lapply(time,function(t){
    outer <- findInterval(t, c(-Inf, keyframes$time))
    inner <- outer -1
    if(outer==1) {
      return(unlist(lapply(attributes,function(attribute){
        get(attribute,keyframes)[1]
      })))
    }
    if(inner==nrow(keyframes)) {
      return(unlist(lapply(attributes,function(attribute){
        get(attribute,keyframes)[nrow(keyframes)]
      })))
    }
    return(unlist(lapply(attributes,function(attribute){
      (get(attribute,keyframes)[inner]-get(attribute,keyframes)[outer])/(keyframes$time[inner]-keyframes$time[outer])*(t-keyframes$time[inner])+get(attribute,keyframes)[inner]  
    })))
  }))  
  }
)

### creator    
MovingGenerator_Cluster<- function(dimension = 2) {
  
  desc <- "Moving Generator Cluster"
  
  
  structure(list(description = desc,
            RObj = movingGenerator_cluster_refClass$new(d = dimension)),
            class = c("MovingGenerator_Cluster"))
}

get_points.MovingGenerator_Cluster <- function(x, time = 1) {
  x$RObj$get_points(time)
}

add_keyframe.MovingGenerator_Cluster <- function(x, t, v = 1, d = 1,...) {
  x$RObj$add_keyframe(t,v,d,...)
}

get_attributes.MovingGenerator_Cluster <- function(x, t) {
  x$RObj$get_attributes(t)
}