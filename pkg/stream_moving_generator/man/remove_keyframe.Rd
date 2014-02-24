\name{remove_keyframe}
\alias{remove_keyframe}
\title{Remove Keyframe to Moving Generator Cluster: Linear}

\description{
  Removes a keyframe to a \code{MGC_Linear}. 
}

\usage{
  remove_keyframe(x, time)
}

\arguments{
  \item{x}{\code{MGC_Linear} object}
  \item{time}{the timestamp for the keyframe that will be removed}
}

\details{
  This allows for the removal of a keframe within a \code{MGC_Linear}.
}

\seealso{
  \code{\link{MGC_Linear}}
}


\examples{
  dsd <- DSD_MG()
  
  c1 <- MGC_Linear()
  add_keyframe(c1,1,10,10,c(0,0),cluster=1)
  add_keyframe(c1,50,10,10,c(50,50),cluster=1)
  add_keyframe(c1,100,10,10,c(50,100),cluster=1)
  
  remove_keyframe(c1,50)

  add_cluster(dsd,c1)
  
  \dontrun{
  animate_data(dsd,2500,xlim=c(0,100),ylim=c(0,100))
  }
}
