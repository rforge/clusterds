\name{MGC_Linear Functions}
\alias{add_keyframe}
\alias{get_keyframes}
\alias{remove_keyframe}

\title{
  MGC_Linear Keyframe Functions
}
\description{
  Allows for the addition and removal of keframes within an MGC_Linear.
}
\usage{
  add_keyframe(x, time, variance = 1, density = 1, center, cluster = NA, reset = FALSE)
  get_keyframes(x) 
  remove_keyframe(x, time)
}

\arguments{
  \item{x}{An \code{MGC_Linear} object.}
  \item{time}{The time stamp the keyframe should be located or which keyframe should be removed.}
  \item{variance}{The variance of the object at a particular keyframe. Can either be a single number or a list with a length equal to the cluster's dimensionality.}
  \item{density}{The density of the cluster at a particular keyframe.}
  \item{center}{The location of the cluster at a particular keyframe. Must be a list with a length equal to the cluster's dimensionality.}
  \item{cluster}{The name of the cluster.}
  \item{reset}{If the stream should reset to the beginning at the keyframe.}
}


\seealso{
  \code{\link{MGC_Linear}} for more details on how to use these functions.
}

\examples{
  
  dsd <- DSD_MG()
  
  c1 <- MGC_Linear()
  add_keyframe(c1,1,10,10,c(0,0),cluster=1)
  add_keyframe(c1,50,10,10,c(50,50),cluster=1)
  add_keyframe(c1,100,10,10,c(50,100),cluster=1,reset=TRUE)
  
  get_keyframes(c1)
  
  remove_keyframe(c1,50)

  add_cluster(dsd,c1)
  
  \dontrun{
  animate_data(dsd,2500,xlim=c(0,100),ylim=c(0,100))
  }
  
}

