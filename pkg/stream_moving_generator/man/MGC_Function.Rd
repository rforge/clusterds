\name{MGC_Function}
\alias{MGC_Function}
\title{Moving Generator Cluster: Function}

\description{
  Creates an evolving gaussian cluster for a \code{DSD_MG} who's behavior is determined by functions. 
}

\usage{
  MGC_Function(variance, density, center, cluster = NA)
}

\arguments{
  \item{variance}{a list of functions or a single function that gives the variance of the cluster}
  \item{density}{a function of time that gives the density of the cluster}
  \item{center}{a list of functions that detail the center of the cluster}
  \item{cluster}{the name of the cluster}
}

\details{
  This allows for a creation of a Moving Generator Cluster that is defined by functions of time. The variance can either be a list of functions, one for every dimension, or it can be a single function so that a user can generate circular gaussian clusters. The number of functions that dictate the center of the gaussian cluster must also be equal to the number of dimensions.
  
  The cluster attribute is useful if a user wishes to combine multiple \code{MGC}s to create a single macro cluster. To accomplish this, the user would simply assign each of the \code{MGC}s the same cluster name.
}

\seealso{
  \code{\link{MGC_Linear}}, \code{\link{MGC_Noise}}, \code{\link{MGC_Random}} for details on other \code{MGC} objects.
  \code{\link{DSD_MG}} for details on how to use it within a \code{DSD}
}


\examples{
  dsd <- DSD_MG()
  
  #Example of a list of variances. Allows for elliptical clusters.
  c1 <- MGC_Function(list(function(t){1},function(t){100}),function(t){25},list(function(t){t},function(t){t}))
  
  #Example of a single function variance. Allows for circular gaussian clusters.
  c2 <- MGC_Function(function(t){.1*t},function(t){25},list(function(t){sin(t/10)*50+50},function(t){t}))
  
  add_cluster(dsd,c1)
  add_cluster(dsd,c2)
  
  \dontrun{
    animate_data(dsd,5000,xlim=c(0,100),ylim=c(0,100))
  }
}
