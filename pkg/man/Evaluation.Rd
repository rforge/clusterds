\name{get_evaluation}
\alias{get_evaluation}
\title{get_evaluation}

\description{
	Gets evaluation measures for micro or macro clusters from a DSC object given the original DSD object.
}

\usage{
	get_evaluation(dsc, dsd, method, n = 1000)
}

\arguments{
	\item{dsc}{The DSC object that the evaluation measure is being requested from.}
	\item{dsd}{The DSD object that holds the initial training data for the DSC.}
	\item{method}{The requested measure.}
	\item{n}{The number of data points being requested.}
}

\details{
	\code{get_evaluation} competes different evaluation measures. The \code{measure} can be set to: \code{"f1"}, \code{"recall"}, \code{"precision"}, \code{"numCluster"}, \code{"numClasses"}, \code{"ssq"}, \code{"rand"}, and \code{"confusionMatrix"}. By default, \code{measure} is set to \code{"f1"}.

}

\value{
	\code{get_evaluation} returns the value of the requested measure. With the exception of a confusion matrix an integer is returned. A confusion matrix is returned as a sparse matrix.
}

\examples{
	#Create stream
	dsd <- DSD_Gaussian_Static(k=3, d=2)
	
	d <- get_points(dsd,1000, assignment = TRUE)
	
	#Create cluster
	x <-DSC_tNN(, threshold=.4)
	cluster(x, dsd, 1000)
	
	#Evaluates for the f1 score
	get_evaluation(x,dsd,"f1")
}
