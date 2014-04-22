\name{animation}
\alias{animate}
\alias{animation}
\alias{animate_data}
\alias{animate_cluster}
\title{Animates the plotting of a DSD and the clustering process}

\description{
Generates an animation of a data stream or a data steam clustering.
}

\usage{
animate_data(dsd, n=1000, interval=.1, 
    pointInterval=100, horizon=5*pointInterval, ...)
animate_cluster(dsc, dsd, macro=NULL, n=1000, interval=.1, 
    pointInterval=100, horizon=5*pointInterval, 
    evaluationMethod=NULL, evaluationType="micro", evaluationAssign="micro", 
    ...)
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
  \item{evaluationType}{evaluate "micro" or "macro"-clusters?}
  \item{evaluationAssign}{assign new points to the closest "micro" or "macro"-cluster
    to calculate the evaluation measure.}
  \item{...}{extra arguments for plot.}
}

\details{
Animations are recorded using the library animation and can be replayed
  (which gives a smoother experience since the is no more computation done) and
  saved in various formats (see Examples section below).
}

\seealso{
  \code{\link{evaluate_cluster}} for stream evaluation without animation.
  See \code{\link[animation]{ani.replay}} for replaying and saving animations.
}


\examples{
dsd <- DSD_Benchmark(1)
animate_data(dsd, 1200,  xlim=c(-.2,1.2), ylim=c(-.2,1.2), 
    pointInterval=20)

\dontrun{
### animations can be replayed with the animation package
library(animation)
animation::ani.options(interval=.1) ## change speed
ani.replay()
  
### animations can also be saved as HTML, animated gifs, etc.
saveHTML(ani.replay())  
  
### animate the clustering process 
reset_stream(dsd)
dsc <- DSC_tNN(r=.2, lambda=.1, shared_density=TRUE,
  alpha=.2)

animate_cluster(dsc, dsd, n=1200, interval=.1, pointInterval=10, 
  # the following are arguments for plot
  xlim=c(-.5,1.5), ylim=c(-.5,1.5), type="shared")

### animate the clustering process with evaluation
reset_stream(dsd)
dsc <- DSC_tNN(r=.2, lambda=.1, shared_density=TRUE,
  alpha=.2)  

animate_cluster(dsc, dsd, n=1200, interval=.1, pointInterval=10, 
  evaluationMethod="crand", evaluationType="macro", evaluationAssign="micro",
  xlim=c(-.5,1.5),ylim=c(-.5,1.5), type="shared")
}
}
