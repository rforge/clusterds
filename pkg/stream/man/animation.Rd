\name{animation}
\alias{animate}
\alias{animate_data}
\alias{animate_cluster}
\title{Animates the plotting of a DSD and the clustering process}

\description{
Generates an animation of a data stream or a data steam clustering.
}

\usage{
animate_data(dsd, n=1000, interval=.1, 
    pointInterval=100, horizon=5*pointInterval,...)
animate_cluster(dsc, dsd, macro=NULL, n=1000, interval=.1, 
    pointInterval=100, horizon=5*pointInterval, 
    evaluationMethod=NULL,...)
}

\arguments{
  \item{dsd}{a DSD object}
  \item{dsc}{a DSC object}
  \item{macro}{a \code{DSC_macro} object used for reclustering when performing evaluations.}
  \item{n}{the number of points to be plotted}
  \item{interval}{the time interval between each frame}
  \item{pointInterval}{the number of new points needed before a new frame is created}
  \item{horizon}{the max number of data points to be shown at a time}
  \item{evaluationMethod}{the evaluation method that should be graphed below the animation}
  \item{...}{extra arguments for plot.}
}

\details{
Animations are recorded using the library animation and can be replayed and
  saved in various formats (see Examples section below).
}

\seealso{
  \code{\link{evaluate_cluster}} for stream evaluation without animation.
  See \code{\link[animation]{ani.replay}} for replaying and saving animations.
}


\examples{
dsd <- DSD_GaussianMoving()
animate_data(dsd, 500,  xlim=c(-.2,1.2), ylim=c(-.2,1.2), 
    pointInterval=20)

\dontrun{
### animations can be replayed with the animation package
library(animation)
ani.replay()
  
### animations can also be saved as HTML, animated gifs, etc.
saveHTML(ani.replay())  
  
### Note: type is a parameter passed on to plot (for DSC)
dsc <- DSC_tNN(.1, alpha=.2, lambda=.03, decay_interval=1, minweight=.1)

reset_stream(dsd)
animate_cluster(dsc, dsd, n=500, interval=.01,
       pointInterval=10, xlim=c(-.5,1.5),ylim=c(-.5,1.5), 
       type="macro")

reset_stream(dsd)
animate_cluster(dsc, dsd, n=500, interval=.01,
       pointInterval=10, xlim=c(-.5,1.5),ylim=c(-.5,1.5), 
       type="macro", evaluationMethod="crand")
}
}
