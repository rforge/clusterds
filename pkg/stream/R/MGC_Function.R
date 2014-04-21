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

MGC_Function_refClass <- setRefClass("MGC_Function", 
                                   fields = list(
                                     variance = "list",
                                     points = "list",
                                     density = "function",
                                     dimension = "numeric",
                                     cluster = "ANY"
                                   ), 
                                   
                                   methods = list(
                                     initialize = function(v,d,p,c) {
                                       variance <<- v
                                       density <<- d
                                       points <<- p
                                       cluster <<- c
                                       dimension <<- length(points)
                                       .self
                                     }
                                     
                                   ),
)

MGC_Function_refClass$methods(
  get_attributes = function(time) {
    x <- data.frame(cluster,density(time))
    colnames(x) <- c("cluster","density")
    x
  },
  get_points = function(time) {
    MASS::mvrnorm(1, mu=unlist(lapply(points,function(x){x(time)})), Sigma=diag(unlist(lapply(variance,function(x){x(time)})),dimension))
  }
)

### creator    
MGC_Function<- function(density, variance, center, cluster = NA) {
  
  desc <- "Functional Moving Generator Cluster"
  
  if(class(variance) == "function")
    variance <- list(variance)
    
  structure(list(description = desc,
                 RObj = MGC_Function_refClass$new(v = variance, d = density, p = center, c = cluster)),
            class = c("MGC_Function","MGC"))
}