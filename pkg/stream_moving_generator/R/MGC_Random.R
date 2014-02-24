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

MGC_Random_refClass <- setRefClass("MGC_Random", 
                                     fields = list(
                                       start = "numeric",
                                       current = "numeric",
                                       variance = "numeric",
                                       density = "numeric",
                                       randomness = "numeric",
                                       dimension = "numeric"
                                     ), 
                                     
                                     methods = list(
                                       initialize = function(s,v,d,r) {
                                         start  <<- s
                                         current <<- s
                                         density <<- d
                                         variance <<- v
                                         randomness <<- r
                                         dimension <<- length(s)
                                         .self
                                       }
                                       
                                     ),
)

MGC_Random_refClass$methods(
  get_attributes = function(time) {
    data.frame(cluster=NA,density)
  },
  get_points = function(time) {
    if(time == 1) current <<- start
    current <<- current + runif(length(current), -randomness, randomness)
    MASS::mvrnorm(1, mu=current, Sigma=diag(variance,dimension))
  }
)

### creator    
MGC_Random<- function(start,variance,density,randomness) {
  
  desc <- "Randoma Moving Generator Cluster"
  
  
  structure(list(description = desc,
                 RObj = MGC_Random_refClass$new(start,variance,density,randomness)),
            class = c("MGC_Random","MGC"))
}