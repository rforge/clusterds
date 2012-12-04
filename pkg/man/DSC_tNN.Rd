\name{DSC_tNN}
\alias{DSC_tNN}
\title{Threshold Nearest Neighbor (tNN) Data Stream Clustering Algorithm}
\description{
    Implements the tNN (threshold Nearest Neighbor) data stream algorithm.}

\usage{
	DSC_tNN(r = 0.2, k = NULL, lambda = 0.01, minweight = 0.1,
                 noise = 0, alpha = 0.4, measure = "Euclidean", macro =
                 TRUE)
}

\arguments{
	\item{r}{The threshold in the nearest neighborhood algorithm.}
	\item{k}{The number of macro clusters to be returned if macro is true.}
	\item{lambda}{The lambda used in the fading function.}
	\item{minweight}{The minimum number of weight a micro-cluster
	    needs to have.}
	\item{noise}{The amount of noise that should be removed while clustering.}
	\item{alpha}{The minimum proportion of shared points between to clusters to warrant 
		combining them.}
	\item{measure}{The measure used to calculate cluster proximity 
	(see package \pkg{proxy}).}
	\item{macro}{A flag that indicates if the macro clusters should be computed.}
}

\details{
The threshold Nearest Neighbor algorithm checks for each new data point in the
incoming stream, if it is below the threshold value of dissimilarity value of
any existing micro-clusters, and if so, merges the point with the 
micro-cluster. Otherwise, a new micro-cluster is created to accommodate 
the new data point.
}

\value{
An object of class \code{DSC_tNN} (subclass of  \code{DSC}, \code{DSC_R}). 
Depending on whether or not the \code{macro} flag
is set, the class list also includes  \code{DSC_Micro} or  \code{DSC_Macro}.
}

\references{ 
M.H. Dunham, Y. Meng, J. Huang (2004): Extensible Markov Model, In: ICDM '04:
Proceedings of the Fourth IEEE International Conference on Data Mining, pp. 371-374.

M. Hahsler, M. H. Dunham (2010): rEMM: Extensible Markov Model for Data
Stream Clustering in R, Journal of Statistical Software, 35(5), 1-31, URL \url{http://www.jstatsoft.org/v35/i05/}
}

\seealso{
	\code{\link{DSC}},
	\code{\link{DSC_Micro}},
	\code{\link{DSC_Macro}}
}

\examples{
# Cassini
dsd <- DSD_mlbenchGenerator("cassini")

# tNN has a built in micro and micro-clusterer
tnn <- DSC_tNN(k=3, alpha=.08)
cluster(tnn, dsd, 500)

# see macro-clusters
get_centers(tnn)

# see micro-clusters
head(get_centers(tnn, type="micro"))

# plot macro-clusters
plot(tnn, dsd)
# plot micro-clusters
plot(tnn, dsd, type="micro")

# evaluate first using macro and then using micro-clusters
get_evaluation(tnn, dsd, "rand")
get_evaluation(tnn, dsd, "rand", type ="micro")
}
