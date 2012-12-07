\name{get_evaluation}
\alias{get_evaluation}
\alias{print.stream_eval}
\title{Evaluate Clusterings}

\description{
	Gets evaluation measures for micro or macro-clusters from a DSC object
	given the original DSD object.
}

\usage{
	get_evaluation(dsc, dsd, method, n = 1000, 
	    type=c("auto", "micro", "macro"), assign="micro", pointInterval=NULL)
}

\arguments{
	\item{dsc}{The DSC object that the evaluation measure is being requested from.}
	\item{dsd}{The DSD object that holds the initial training data for the DSC.}
	\item{method}{The requested measure.}
	\item{n}{The number of data points being requested.}
	\item{type}{Use micro- or macro-clusters for evaluation. Auto
	        used the class of dsc to decide.}
	\item{assign}{Assign points to micro or macro-clusters? In case 
	assignment is done to micro-clusters while type is macro, micro-cluster
	assignments are translated to macro-cluster assignments using 
	information on what micro-clusters constitute each macro-cluster.}
	\item{pointInterval}{If set, it will cluster the DSC on the given interval and then perform
		the evaluation method(s) each time until n is depleted}
}

\details{
	For evaluation, each data points are assigned
	to its nearest cluster (using Euclidean distance to the cluster
	centers). Then
	for each cluster the majority class is determined. Based on 
	the majority class and the class of all assigned points several 
	evaluation measures are available:
	\itemize{
		\item	\code{"numCluster"} (number of clusters), 
		\item	\code{"numClasses"} (number of classes),
		\item	\code{"f1"} (F1 measure), 
		\item	\code{"recall"},
		\item	\code{"precision"}, 
		\item	\code{"purity"}, 
		\item	\code{"fpr"} (false positive rate),
		\item	\code{"ssq"} (sum of squares), 
		\item	\code{"jaccard"} (Jaccard index), 
		\item	\code{"rand"} (Rand index),
		\item	\code{"rand_HA"} (Hubert and Arabie's adjusted Rand index),
		\item	\code{"rand_MA"} (Morey and Agresti's adjusted Rand index),
		\item	\code{"rand_FM"} (Fowlkes and Mallows's index).
	}

Many measures are the average over all clusters. For example, purity is
the average purity over all clusters.

For \code{DSC_Micro} objects, data points are assigned to micro-clusters
and then each micro-cluster is evaluated. For \code{DSC_Macro} objects, 
data points by default (\code{assign="micro"}) also assigned to
micro-clusters, but these assignments are translated to macro-clusters.
The evaluation is here done for macro-clusters. This is important when 
macro-clustering is done with algorithms which do not create spherical
clusters (e.g, hierarchical clustering with single-linkage or DBSCAN) and
this assignment to the macro-clusters directly (i.e., their center) 
does not make sense.

Using \code{type} and \code{assign}, the user can select how to assign data
points and ad what level (micro or macro) to evaluate.
}

\value{
    \code{get_evaluation} returns an object of class \code{stream_eval}
    which is a numeric vector of the values of the requested measures
    and two attribures, \code{"type"} and
    \code{"assign"}, to see at what level the evaluation was done.
    
}

\examples{
# Create stream
dsd <- DSD_GaussianStatic(k=3, d=2)

micro <- DSC_DenStream()
cluster(micro, dsd, 1000)
# Evaluate micro-clusters
get_evaluation(micro, dsd, c("f1","rand"))

macro <- DSC_KmeansW(2)
recluster(macro, micro)
# Evaluate macro-clusters by assigning to micro-clusters
get_evaluation(macro, dsd, c("f1","rand"))
# Evaluate macro-clusters by assigning to macro-clusters
# This gives a similar result as the micro-cluster assignment since
# the real clusters are spherical (multidimensional Gaussians).
get_evaluation(macro, dsd, c("f1","rand"), assign="macro")
}
