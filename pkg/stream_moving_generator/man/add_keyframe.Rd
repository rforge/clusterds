\name{add_keyframe}
\alias{add_keyframe}
\title{Add Keyframe to Moving Generator Cluster: Linear}

\description{
  Adds a keyframe to a \code{MGC_Linear}. 
}

\usage{
  add_keyframe(x, time, variance = 1, density = 1, center, cluster = NA)
}

\arguments{
  \item{x}{\code{MGC_Linear} object}
  \item{time}{the timestamp for the keyframe}
  \item{variance}{the variance at the given timestamp}
  \item{density}{the density at the given timestamp}
  \item{center}{the center at the given timestamp}
  \item{cluster}{the name of the cluster}
}

\details{
  This allows for the creation of a keframe within a \code{MGC_Linear}. If a keyframe already exists at a given timestamp then the old keyframe will be overridden.
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

  add_cluster(dsd,c1)
  
  \dontrun{
  animate_data(dsd,2500,xlim=c(0,100),ylim=c(0,100))
  }
}
