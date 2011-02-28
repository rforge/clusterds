\name{DSC_DenStream}
\alias{DSC_DenStream}
\title{DataStreamClusterer: DenStream}
\description{Creates a new DataStreamClusterer with the DenStream algorithm}
\usage{
DSC_DenStream(epsilon = 0.1, minPoints = 10, lambda = 0.006, beta = 0.001, mu = 1, initPoints = 10000)
}
\arguments{
  \item{epsilon}{-e (defines the epsilon neighborhood, range: 0 to 1)}
  \item{minPoints}{-p (min. num. points a cluster must have)}
  \item{lambda}{-l (range: 0 to 1)}
  \item{beta}{-b (range: 0 to 1)}
  \item{mu}{-m (range: 0 to max(double))}
  \item{initPoints}{-i (number of points to use for initialization)}
}
%%\details{}
\value{
	\item{description}{The name of the algorithm in the DSC object.}
	\item{options}{The CLI params defined when creating the DSC object.}
	\item{javaObj}{The underlying Java object associated with DSC_MOA objects.}
}
%%\references{ } TODO: reference the original literature
%%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%%}
\examples{
dsc <- DSC_DenStream()
dsd <- DSD_Static()
cluster(dsc, dsd, 100000)
plot(dsc)
}
