\name{get_evaluation}
\alias{get_evaluation}
\title{Evaluate Clusterings}

\description{
	Gets evaluation measures for micro or macro clusters from a DSC object given the original DSD object.
}

\usage{
	get_evaluation(dsc, dsd, method, n = 1000, 
	    type=c("auto", "micro", "macro"))
}

\arguments{
	\item{dsc}{The DSC object that the evaluation measure is being requested from.}
	\item{dsd}{The DSD object that holds the initial training data for the DSC.}
	\item{method}{The requested measure.}
	\item{n}{The number of data points being requested.}
	\item{type}{Use micro- or macro-clusters for evaluation. Auto
	        used the class of dsc to decide.}

}

\details{
	\code{get_evaluation} competes different evaluation measures. The
	\code{measure} can be set to: 
	\code{"numCluster"} (number of clusters), 
	\code{"numClasses"} (number of classes),
	\code{"f1"} (F1 measure), 
	\code{"recall"},
	\code{"precision"}, 
	\code{"fpr"} (false positive rate),
	\code{"ssq"} (sum of squares), 
	\code{"jaccard"} (Jaccard index), 
	\code{"rand"} (Rand index),
	\code{"rand_HA"} (Hubert and Arabie's adjusted Rand index),
	\code{"rand_MA"} (Morey and Agresti's adjusted Rand index),
	\code{"rand_FM"} (Fowlkes and Mallows's index).
}

\value{
	\code{get_evaluation} returns the value of the requested measure. 
	%With the exception of a confusion matrix an integer is returned. A confusion matrix is returned as a sparse matrix.
}

\examples{
# Create stream
dsd <- DSD_GaussianStatic(k=3, d=2)

# Create cluster
x <-DSC_Kmeans(2)
cluster(x, dsd, 1000)

# Evaluates for the f1 and rand score
get_evaluation(x, dsd, c("f1","rand"))

# Get all avaialable measures 
get_evaluation(x, dsd)
}
