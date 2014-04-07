\name{DSD_MG}
\alias{DSD_MG}
\alias{DSD_MovingGenerator}
\alias{MovingGenerator}
\title{DSD Moving Generator}

\description{
  Creates an evolving DSD that consists of several \code{MGC}s.
}

\usage{
  DSD_MG(dimension = 2, ...)
}

\arguments{
  \item{dimension}{the dimension of the DSD object}
  \item{...}{initial set of \code{MGC}s}
}

\details{
  This DSD is able to generate complex datasets that are able to evolve over a period of time.
  Its behavior is determined by the \code{MGC}s it is composed of. 
}

\seealso{
  \code{\link{MGC_Function}}, \code{\link{MGC_Linear}}, \code{\link{MGC_Noise}}, \code{\link{MGC_Random}} for details on the different \code{MGC} objects.
}


\examples{
  c1 <- MGC_Linear()
  c2 <- MGC_Linear()
  add_keyframe(c1,1,10,10,c(0,0),cluster=1)
  add_keyframe(c1,50,10,10,c(50,50),cluster=1)
  add_keyframe(c1,100,10,10,c(50,100),cluster=1)
  add_keyframe(c2,1,10,10,c(0,0),cluster=1)
  add_keyframe(c2,50,10,10,c(50,50),cluster=2)
  add_keyframe(c2,100,10,10,c(100,50),cluster=2)
  
  dsd <- DSD_MG(2,c1,c2)
  
  tnn <- DSC_tNN(alpha=.5,r=10,lambda=0.1)
  
  \dontrun{
    tnnPurity <- animate_cluster(tnn,dsd,n=2500,type="macro",xlim=c(0,100),ylim=c(0,100),evaluationMethod="purity",horizon=100)
  }
}
