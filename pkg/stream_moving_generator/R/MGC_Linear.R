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

add_keyframe <- function(x, time, variance = 1, density = 1, center, cluster = NA) 
  UseMethod("add_keyframe")
get_keyframes <- function(x) 
  UseMethod("get_keyframes")
remove_keyframe <- function(x, time) 
  UseMethod("remove_keyframe")

MGC_Linear_refClass <- setRefClass("MGC_Linear", 
                            fields = list(
                              keyframes = "data.frame",
                              dimension = "numeric"
                            ), 
                            
                            methods = list(
                              initialize = function(d = 2) {
                                keyframes  <<- data.frame(time =  numeric(0),variance = list(), density = numeric(0), cluster = numeric(0), points = list())
                                .self
                              }
                              
                            ),
)

MGC_Linear_refClass$methods(
  add_keyframe = function(t,v,d,p,c) {
    dimension <<- length(c)
    keyframes <<- rbind(keyframes,data.frame(time=t,variance=I(list(v)),density=d,cluster=c,points=I(list(p))))
    keyframes <<- keyframes[with(keyframes, order(time)), ]
  },
  get_points = function(time) {
    attributes <- get_attribute(time,c("points","variance"))
    MASS::mvrnorm(1, mu=unlist(attributes[[1]]), Sigma=diag(unlist(attributes[[2]]),dimension))
  },
  get_attributes = function(time) {
    get_attribute(time,c("cluster","density"))
  },
  get_attribute = function(time,attributes) {
    do.call(rbind, lapply(time,function(t){
      outer <- findInterval(t, c(-Inf, keyframes$time))
      inner <- outer -1
      if(outer==1) {
        return((lapply(attributes,function(attribute){
          get(attribute,keyframes)[1]
        })))
      }
      if(inner==nrow(keyframes)) {
        return((lapply(attributes,function(attribute){
          get(attribute,keyframes)[nrow(keyframes)]
        })))
      }
      return((lapply(attributes,function(attribute){
        if(attribute == "cluster") return(get(attribute,keyframes)[inner])
        (unlist(get(attribute,keyframes)[inner])-unlist(get(attribute,keyframes)[outer]))/(keyframes$time[inner]-keyframes$time[outer])*(t-keyframes$time[inner])+unlist(get(attribute,keyframes)[inner]) 
      })))
    }))  
  }
)

### creator    
MGC_Linear<- function(dimension = 2) {
  
  desc <- "Linear Moving Generator Cluster"
  
  
  structure(list(description = desc,
                 RObj = MGC_Linear_refClass$new(d = dimension)),
            class = c("MGC_Linear","MGC"))
}

add_keyframe.MGC_Linear <- function(x, time, variance = 1, density = 1, center, cluster = NA) {
  x$RObj$keyframes <- x$RObj$keyframes[which(x$RObj$keyframes$time!=time),]
  x$RObj$add_keyframe(time,variance,density, center, cluster)
}

get_keyframes.MGC_Linear <- function(x) {
  x$RObj$keyframes
}

remove_keyframe.MGC_Linear <- function(x, time) {
  x$RObj$keyframes <- x$RObj$keyframes[which(x$RObj$keyframes$time!=time),]
}

print.MGC_Linear <- function(x, ...) {
  cat(paste(x$description, " (", paste(class(x), collapse=", "), ")", '\n', sep=""))
  cat(paste('With', nrow(x$RObj$keyframes), 'keyframes', 'in', x$RObj$dimension, 'dimensions', '\n'))
}