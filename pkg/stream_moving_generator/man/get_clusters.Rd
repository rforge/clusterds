\name{get_clusters}
\alias{get_clusters}
\title{Gets Moving Generator Cluster}

\description{
  Gets the \code{MGC}s from a \code{DSD_MG}. 
}
  
  \usage{
  get_clusters(x) 
  }
  
  \arguments{
  \item{x}{the \code{DSD_MG} object}
  }
  
  \details{
  This allows a user to get the \code{MGC}s from a \code{DSD_MG}.
  }
  
  \seealso{
  \code{\link{DSD_MG}} for details on how to use it with a \code{DSD}
  }
  
  
  \examples{
  dsd <- DSD_MG()
  
  c1 <- MGC_Random(c(50,50),1,1,1)
  
  #adding noise
  c2 <- MGC_Noise(1,c(-20,120,-20,120))
  
  add_cluster(dsd,c1)
  add_cluster(dsd,c2)
  
  get_clusters(dsd)
  
  \dontrun{
  animate_data(dsd,2500,xlim=c(0,100),ylim=c(0,100))
  }
  }
  