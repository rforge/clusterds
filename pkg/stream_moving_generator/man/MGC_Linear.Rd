\name{MGC_Linear}
\alias{MGC_Linear}
\title{Moving Generator Cluster: Linear}

\description{
  Creates an evolving gaussian cluster for a \code{DSD_MG} who's behavior is determined by several keyframes. 
}
  
  \usage{
  MGC_Linear(dimension = 2) 
  }
  
  \arguments{
  \item{dimension}{the dimension of the \code{MGC}}
  }
  
  \details{
  This allows for a creation of a Moving Generator Cluster that is defined by several different keyframes. The constructor for \code{MGC_Linear} merely creates a wrapper for several keyframes. To add a keyframe use \code{add_keyframe()}.
  }
  
  \seealso{
  \code{\link{add_keyframe}}, \code{\link{remove_keyframe}} for deatils on how to add or remove keyframes.
  \code{\link{MGC_Function}}, \code{\link{MGC_Noise}}, \code{\link{MGC_Random}} for details on other \code{MGC} objects.
  \code{\link{DSD_MG}} for details on how to use it within a \code{DSD}
  }
  
  
  \examples{
  dsd <- DSD_MG()
  
  c1 <- MGC_Linear()
  c2 <- MGC_Linear()
  add_keyframe(c1,1,10,10,c(0,0),cluster=1)
  add_keyframe(c1,50,10,10,c(50,50),cluster=1)
  add_keyframe(c1,100,10,10,c(50,100),cluster=1)
  add_keyframe(c2,1,10,10,c(0,0),cluster=1)
  add_keyframe(c2,50,10,10,c(50,50),cluster=2)
  add_keyframe(c2,100,10,10,c(100,50),cluster=2)

  add_cluster(dsd,c1)
  add_cluster(dsd,c2)
  
  \dontrun{
  animate_data(dsd,2500,xlim=c(0,100),ylim=c(0,100))
  }
  }
  