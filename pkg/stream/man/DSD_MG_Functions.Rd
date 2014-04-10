\name{DSD_MG Functions}
\alias{add_cluster}
\alias{get_clusters}
\alias{remove_cluster}

\title{
  DSD_MG Cluster Functions
}
\description{
  Allows for the addition and removal of clusters within a DSD_MG.
}
\usage{
  add_cluster(x, c)
  get_clusters(x) 
  remove_cluster(x, i)
}

\arguments{
  \item{x}{A \code{DSD_MG} object.}
  \item{c}{The cluster that should be added to the \code{DSD_MG} object.}
  \item{i}{The index of the cluster that should be removed from the \code{DSD_MG} object.}
}
  
  
\seealso{
  \code{\link{DSD_MG}} for more details on how to use a \code{DSD_MG}.
}

  \examples{
    
    dsd <- DSD_MG()
    
    c1 <- MGC_Random(1,1,c(50,50),1)
    
    #adding noise
    c2 <- MGC_Noise(1,rbind(c(-20,120),c(-20,120)))
    
    add_cluster(dsd,c1)
    add_cluster(dsd,c2)
    
    get_clusters(dsd)
    
    remove_cluster(dsd,1)
    
    get_clusters(dsd)
    
  }
  
  