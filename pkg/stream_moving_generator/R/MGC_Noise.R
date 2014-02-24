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

MGC_Noise_refClass <- setRefClass("MGC_Noise", 
                                   fields = list(
                                     density = "numeric",
                                     range = "numeric"
                                   ), 
                                   
                                   methods = list(
                                     initialize = function(d,r) {
                                       density  <<- d
                                       range <<- r
                                       .self
                                     }
                                     
                                   ),
)

MGC_Noise_refClass$methods(
  get_attributes = function(time) {
    x <- matrix(c(0,density),ncol=2,byrow=TRUE)
    colnames(x) <- c("cluster","density")
    x
  },
  get_points = function(time) {
    unlist(lapply(seq(1, length(range), by=2),function(x){
      runif(1, range[x], range[x+1])
    }))
  }
)

### creator    
MGC_Noise<- function(density, range) {
  
  desc <- "Noise Moving Generator Cluster"
  
  structure(list(description = desc,
                 RObj = MGC_Noise_refClass$new(density, range)),
            class = c("MGC_Noise","MGC"))
}