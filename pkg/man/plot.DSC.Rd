\name{plot}
\alias{plot}
\title{plot}

\description{

}

\usage{
	plot(x, ..., method="pairs") # where x is a DSC object
}

\arguments{
	\item{x}{The DSC object that the data will be pulled from to plot.}
	\item{...}{Additional parameters that will be passed to the base 	
			   \code{plot()} function.}
	\item{method}{}
}

\details{
}

\seealso{
	\code{\link{DSClusterer}},
	\code{\link{DSC_CobWeb}},
	\code{\link{DSC_ClusTree}},
	\code{\link{DSC_StreamKM}},
	\code{\link{DSC_DenStream}},
	\code{\link{DSC_Clustream}}
}

\examples{
# setting up the objects
dsc <- DSC_DenStream()
dsd <- DSD_MOA(k=3, d=2)
cluster(dsc, dsd, 2000)

# plotting the data
plot(dsc, col="red", pch=3, method="pairs")
plot(dsc, col="red", pch=3, method="plot")
plot(dsc, col="red", pch=3, method="pc")
}
