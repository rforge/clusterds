\name{DSC_CobWeb}
\alias{DSC_CobWeb}
\title{DataStreamClusterer: CobWeb}
\description{Creates a new DataStreamClusterer with the CobWeb algorithm}
\usage{
DSC_CobWeb(acuity = 1, cutoff = 0.002, randomSeed = 1)
}
\arguments{
  \item{acuity}{}
  \item{cutoff}{}
  \item{randomSeed}{}
}
\details{}
\value{
	\item{description}{The name of the algorithm in the DSC object.}
	\item{options}{The CLI params defined when creating the DSC object.}
	\item{javaObj}{The underlying Java object associated with DSC_MOA objects.}
}
%%\references{ } TODO: need to reference the original paper
%%\author{ ~~who you are~~}
%%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%%}
\examples{
dsc <- DSC_CobWeb()
dsd <- DSD_Static()
cluster(dsc, dsd, 100000)
plot(dsc)
}
