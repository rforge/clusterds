\name{cluster_evaluation}
\alias{cluster_evaluation}
\title{Evaluate Clusterings while Clustering}

\description{
	Gets evaluation measures for micro or macro-clusters from a DSC object
	given the original DSD object over a span of time.
}

\usage{
	cluster_evaluation(dsc, dsd, macro=NULL, method, n = 1000, 
	    type=c("auto", "micro", "macro"), assign="micro", pointInterval=100)
}

\arguments{
	\item{dsc}{The DSC object that the evaluation measure is being requested from.}
	\item{dsd}{The DSD object that holds the initial training data for the DSC.}
	\item{macro}{The macro DSC object that the evaluation measure is being requested from.}
	\item{method}{The requested measure.}
	\item{n}{The number of data points being clustered.}
	\item{type}{Use micro- or macro-clusters for evaluation. Auto
	        used the class of dsc to decide.}
	\item{assign}{Assign points to micro or macro-clusters? In case 
	assignment is done to micro-clusters while type is macro, micro-cluster
	assignments are translated to macro-cluster assignments using 
	information on what micro-clusters constitute each macro-cluster.}
	\item{pointInterval}{The interval between each evaluation score.}
}

\details{
	This function will perform clustring on the data. At every point interval, the evaluation
	score will be computed and returned to the user. This allows the user to see the evaluation
	measures change over time.
	
	To better understand how evaluation functions take a look at \code{get_evaluation} 
}

\seealso{
	\code{\link{get_evaluation}}
}

\value{
    \code{cluster_evaluation} returns a vector of objects of class \code{stream_eval}
    which is a numeric vector of the values of the requested measures
    and two attribures, \code{"type"} and
    \code{"assign"}, to see at what level the evaluation was done.
    
}

\examples{
# Create stream
dsd <- DSD_GaussianStatic(k=3, d=2)

micro <- DSC_DenStream()
macro <- DSC_KmeansW(2)

cluster_evaluation(micro, dsd, macro, c("f1","rand"), assign="macro")
}
