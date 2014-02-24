\name{MGC_Noise}
\alias{MGC_Noise}
\title{Moving Generator Cluster: Noise}

\description{
  Creates noise for a \code{DSD_MG}. 
}
  
  \usage{
  MGC_Noise(density, range) 
  }
  
  \arguments{
  \item{density}{The density of the noise}
  \item{range}{a numeric vector that consists of the range of the noise}
  }
  
  \details{
  This allows for the creation of noise within a \code{DSD_MG}.
  }
  
  \seealso{
  \code{\link{DSD_MG}} for details on how to use it within a \code{DSD}
  }
  
  
  \examples{
  dsd <- DSD_MG()
  
  c1 <- MGC_Random(c(50,50),1,1,1)
  
  #adding noise
  c2 <- MGC_Noise(1,c(-20,120,-20,120))
  
  add_cluster(dsd,c1)
  add_cluster(dsd,c2)
  
  \dontrun{
  animate_data(dsd,2500,xlim=c(0,100),ylim=c(0,100))
  }
  }
  