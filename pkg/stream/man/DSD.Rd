\name{DSD}
\alias{DSD}
\title{Data Stream Data Generator Base Class}
\description{Abstract base class for all DSD (Data Stream Data
Generator) classes.}

\details{
The \code{DSD} class cannot be instantiated, but it serves
as a base class from which all DSD objects inherit. There is
only one function that needs to be implemented
for a new \code{DSD} subclass:
\code{get_points()}. \code{DSD} also provides
\code{print()} and \code{plot()}.
}

\seealso{
	\code{\link{animate_data}},
	\code{\link{animate_cluster}},
  \code{\link{cluster}},
  \code{\link{DSD_ReadStream}},
	\code{\link{evaluate}},
  \code{\link{get_points}},
	\code{\link[stream]{plot}},
	\code{\link{recluster}},
  \code{\link{write_stream}}
}

\examples{
# create data stream with three clusters in 3-dimensional space 
dsd <- DSD_Gaussians(k=3, d=3)

# get points from stream
get_points(dsd, n=5)  

# get points with true cluster assignment
p <- get_points(dsd, n=5, assignment=TRUE)
attr(p, "assignment")
  
# plotting the data (scatter plot matrix, first and third dimension, and first
#  two principal components)
plot(dsd)
plot(dsd, dim=c(1,3))
plot(dsd, method="pc")
}