\name{get_keyframes}
\alias{get_keyframes}
\title{Get Keyframes from Moving Generator Cluster: Linear}

\description{
  Gets the keyframes from a \code{MGC_Linear}. 
}

\usage{
  get_keyframes(x)
}

\arguments{
  \item{x}{\code{MGC_Linear} object}
}

\details{
  This allows for a user to get all of the keyframes from a \code{MGC_Linear}.
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
  
  get_keyframes(c1)
}
