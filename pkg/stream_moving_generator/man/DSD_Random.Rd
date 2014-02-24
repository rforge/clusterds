\name{MGC_Random}
\alias{MGC_Random}
\title{Moving Generator Cluster: Random}

\description{
  Creates an evolving gaussian cluster for a \code{DSD_MG} who's behavior is random. 
}
  
  \usage{
  MGC_Random(start,variance,density,randomness) 
  }
  
  \arguments{
  \item{start}{the starting center of the randomly moving gaussian cluster}
  \item{variance}{the static variance of the cluster}
  \item{density}{the static density of the cluster}
  \item{randomness}{The maximum amount the cluster will move during one time step}
  }
  
  \details{
  This allows for the creation of a Moving Generator Cluster that moves randomly.
  }
  
  \seealso{
  \code{\link{MGC_Function}}, \code{\link{MGC_Noise}}, \code{\link{MGC_Linear}} for details on other \code{MGC} objects.
  \code{\link{DSD_MG}} for details on how to use it within a \code{DSD}
  }
  
  
  \examples{
  dsd <- DSD_MG()
  
  c1 <- MGC_Random(c(50,50),1,1,1)
  c2 <- MGC_Random(c(50,50),1,1,1)
  
  add_cluster(dsd,c1)
  add_cluster(dsd,c2)
  
  \dontrun{
  animate_data(dsd,2500,xlim=c(0,100),ylim=c(0,100))
  }
  }
  