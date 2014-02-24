\name{remove_cluster}
\alias{remove_cluster}
\title{Remove Moving Generator Cluster}

\description{
  Removes a \code{MGC} to a \code{DSD_MG}. 
}
  
  \usage{
  remove_cluster(x, i) 
  }
  
  \arguments{
  \item{x}{the \code{DSD_MG} object}
  \item{i}{the index of the \code{MGC} that will be removed}
  }
  
  \details{
  This allows a user to remove a \code{MGC} from a \code{DSD_MG}.
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
  
  #removes noise
  remove_cluster(dsd,2)
  
  \dontrun{
  animate_data(dsd,2500,xlim=c(0,100),ylim=c(0,100))
  }
  }
  