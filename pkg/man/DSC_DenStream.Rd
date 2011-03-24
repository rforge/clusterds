\name{DSC_DenStream}
\alias{DSC_DenStream}
\title{DataStreamClusterer: DenStream}
\description{Creates a new DataStreamClusterer with the DenStream algorithm}
\usage{
DSC_DenStream(epsilon = 0.1, minPoints = 10, lambda = 0.006, beta = 0.001, mu = 1, initPoints = 1000)
}
\arguments{
  \item{epsilon}{-e (defines the epsilon neighborhood, range: 0 to 1)}
  \item{minPoints}{-p (min. num. points a cluster must have)}
  \item{lambda}{-l (range: 0 to 1)}
  \item{beta}{-b (range: 0 to 1)}
  \item{mu}{-m (range: 0 to max(double))}
  \item{initPoints}{-i (number of points to use for initialization)}
}
\details{
DSC_DenStream is based on the DenStream algorithm. DenStream is a density 
based clustering algorithm that summarizes the incoming data points into micro-clusters 
based upon the frequency of data points in a particular location within a defined boundary 
(i.e., their density). DenStream includes a fading function that causes the weight of 
data points to decrease exponentially as t increases. This means that temporal structures 
within the data stream are also visible using this algorithm. 

%%TODO: why doesn't \item work here??
The parameters are defined as follows:
 \code{epsilon}
	{refers to the neighborhood in which the density of each micro-cluster is 
	 calculated (in other words, the radius).}
 \code{minPoints}
	{is the minimum number of points a micro-cluster must have.}
 \code{lambda}
	{alters the fading function: the larger \code{lambda}, the lower the 
	 weight of the historic data.}
 \code{beta}
	{is the outlier threshold, and is used in conjunction with \code{mu}}
 \code{mu}
	{the minimum weight a micro-cluster must have within the \code{epsilon} 
	 neighborhood to be considered a core-micro-cluster (as opposed to an 
	 outlier or a potential-micro-cluster).}
 \code{initPoints}
	{is the number of data points to initialize the algorithm with.}
}
\value{
	\item{description}{The name of the algorithm in the DSC object.}
	\item{options}{The CLI params defined when creating the DSC object.}
	\item{javaObj}{The underlying Java object associated with DSC_MOA objects.}
}
\references{
Cao F, Ester M, Qian W, Zhou A (2006). "Density-Based Clustering over 
an Evolving Data Stream with Noise." In "Proceedings of the 2006 SIAM International 
Conference on Data Mining," pp 326-337. SIAM. 

Bifet A, Holmes G, Pfahringer B, Kranen P, Kremer H, Jansen T, Seidl T (2010). 
"MOA: Massive Online Analysis, a Framework for Stream Classification and 
Clustering." In "Journal of Machine Learning Research (JMLR) Workshop and Conference Proceedings,"
}

%%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%%}
\examples{
dsc <- DSC_DenStream()
dsd <- DSD_Gaussian_Static()
cluster(dsc, dsd, 5000)
plot(dsc)
}
