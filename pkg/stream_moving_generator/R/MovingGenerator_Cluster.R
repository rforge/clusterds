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
                                 keyframes = "data.frame"
                               ), 
                               
                               methods = list(
                                 initialize = function() {
                                   
                                   keyframes	<<- data.frame(t =  numeric(0),r = numeric(0))
                                   
                                   .self
                                 }
                                 
                               ),
)

movingGenerator_cluster_refClass$methods(get_points = function(t) {
  t
},
                                         add_Keyframe = function(t,r) {
                                           keyframes <<- rbind(keyframes,setNames(as.list(c(t,r)), c("time","radius")))
                                           keyframes <<- keyframes[with(keyframes, order(time)), ]
}
)

### creator    
MovingGenerator_Cluster<- function(k, weighted = TRUE, iter.max = 10, nstart = 1,
                                   algorithm = c("Hartigan-Wong", "Lloyd", "Forgy",
                                                 "MacQueen")) {
  
  desc <- "Moving Generator Cluster"
  
  
  structure(list(description = desc,
                 RObj = movingGenerator_cluster_refClass$new()),
            class = c("MovingGenerator_Cluster"))
}

get_points.MovingGenerator_Cluster <- function(x, time = 1) {
  x$RObj$get_points(time)
}

add_keyframe.MovingGenerator_Cluster <- function(x, t = 1, r = 1) {
  x$RObj$add_Keyframe(t,r)
}

get_attributes.MovingGenerator_Cluster <- function(x, t) {
  outer <- findInterval(t, c(-Inf, x$RObj$keyframes$time))
  inner <- outer -1
  if(outer==1)
    return(x$RObj$keyframes$radius[1])
  if(inner==length(x$RObj$keyframes))
    return(x$RObj$keyframes$radius[length(x$RObj$keyframes)])
  (x$RObj$keyframes$radius[outer]+x$RObj$keyframes$radius[inner])/2
}
