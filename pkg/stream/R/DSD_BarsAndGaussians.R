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


DSD_BarsAndGaussians <- function(noise = 0) {
  # creating the DSD object
  l <- list(description = "Bars and Gaussians",
    d = 2,
    k = 4,
    noise = noise)
  class(l) <- c("DSD_BarsAndGaussians","DSD_R","DSD")
  l
}

get_points.DSD_BarsAndGaussians <- function(x, n=1, assignment = FALSE,...) {
  ### gaussians at (3,2.5) and (3,-2.5)
  ### bars at (-3,2.8) and (-3,-2.8)
  
  a <- sample(c(NA_integer_, 1:4), n, 
    prob=c(x$noise, 3/8*(1-x$noise), 1/8*(1-x$noise), 
      3/8*(1-x$noise), 1/8*(1-x$noise)), replace=TRUE)
  
  dat <- as.data.frame(t(sapply(a, FUN=function(type) {
    ### noise  
    if(is.na(type)) p <- runif(2, -6,6)
    else {
      ### Gaussian 1
      if(type==1) {
        cen <- c(3,2)
        p <- rnorm(2) + cen
      }
      
      ### Gaussian 2
      if(type==2) {
        cen <- c(3,-2)
        p <- rnorm(2) + cen
      }
      
      ### bar 1
      if(type==3) {
        cen <- c(-3, 1.3)
        hight <- 2
        width <- 5
        p <- c(runif(1, -width/2, width/2), runif(1, -hight/2, hight/2)) + cen
      }
      
      ### bar 2
      if(type==4) {
        cen <- c(-3, -1.3)
        hight <- 2
        width <- 5
        p <- c(runif(1, -width/2 , width/2), runif(1, -hight/2, hight/2)) + cen
      }
    }
    
    p
  })))
  
  
  if(assignment) attr(dat, "assignment") <- a
  
  names(dat) <- c("x","y")
  
  dat
}
