\name{DSD_Gaussian_Moving}
\alias{DSD_Gaussian_Moving}
\title{DataStreamData: Moving data stream}
\description{
A DataStreamData that generates a moving data stream. It has two clusters and a third that moves
between them.
}

\usage{
DSD_Gaussian_Moving(t=10, n=20)
}

\arguments{
  \item{t}{The number of turns or steps taken for the moving cluster to move from the
  	bottom left hand corner to the top right hand corner.}
  \item{n}{The number of points used for each cluster during step or turn.}
}

\details{
\code{DSD_Gaussian_Moving} is a DSD generator for stream data. 
It has been implemented entirely in R, so there is no computational 
overhead with communicating to the Java Runtime Interface (JRI) or 
native C code. An important characteristic of \code{DSD_Gaussian_Moving} 
is that the data stream models data that changes. This makes it
 an ideal DSD for examining 
the temporal structure of data streams for drastic changes such as 
the splitting or merging of clusters. 

\code{DSD_Gaussian_Moving} generates 2 dimensionality data in 3
different clusters. Two of the clusters are static and are located at opposite
corners. The third cluster traverses the two. The number of steps it takes for
the third cluster to complete its journey is determined by \code{t}.

This DSD does not loop. To reuse the DSD the user must call the \code{reset_stream}
function.

}

\value{
Returns a \code{DSD_Gaussian_Moving} object which is a list of the defined
params. The params are either passed in from the function or
created internally. They include: 

	\item{description}{A brief description of the DSD object.}
	\item{k}{The number of clusters.}
	\item{d}{The number of dimensions.}
}
	   
\seealso{
	\code{\link{DSData}}
}

\examples{
# create data stream with three clusters in 2D
dsd <- DSD_Gaussian_Moving()

# plotting the data
plot_animation(dsd,600)


}
