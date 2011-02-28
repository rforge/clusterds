\name{DSC_Clustream}
\alias{DSC_Clustream}
\title{DataStreamClusterer: Clustream}
\description{Creates a new DataStreamClusterer with the Clustream algorithm}
\usage{
DSC_Clustream(timeWindow = 1000, maxNumKernels = 100)
}
\arguments{
  \item{timeWindow}{Defines the time window to be used in Clustream}
  \item{maxNumKernels}{Defines the maximum number of kernels used in Clustream}
}
\details{}
\value{
	\item{description}{The name of the algorithm in the DSC object.}
	\item{options}{The CLI params defined when creating the DSC object.}
	\item{javaObj}{The underlying Java object associated with DSC_MOA objects.}
}
%% \references{ } TODO: need to reference the Clustream paper
%%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%%}
\examples{
dsc <- DSC_Clustream()
dsd <- DSD_Static()
cluster(dsc, dsd, 100000)
plot(dsc)
}
