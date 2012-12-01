\name{data_animation}
\alias{data_animation}
\title{Animates the plotting of a DSD}

\description{
Generates several plots at different time intervals for a \code{DSD}. Afterwards it generates
an animation of the data points.
}

\usage{
data_animation(dsd,n=1,interval=.1, horizon=500, pointInterval=100, outdir=NULL,...)
}

\arguments{
  \item{dsd}{a DSD object}
  \item{n}{the number of points to be plotted}
  \item{interval}{the time interval between each frame}
  \item{horizon}{the max number of data points to be shown at a time}
  \item{pointInterval}{the number of new points needed before a new frame is created}
  \item{outdir}{the location of where the gif should be saved}
  \item{...}{extra arguments for plot.}
}

\details{
It generates several plots and strings them together to form an animation. This is particularly
useful to see the behaviour of moving data sets. If no \code{outdir} is specified
the function will animate the progress while clustering.
}


\seealso{
	\code{\link{cluster_animation}}
}

\examples{
dsd <- DSD_GaussianMoving()
data_animation(dsd, 500)
}
