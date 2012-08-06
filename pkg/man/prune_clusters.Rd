\name{merge_DSC}
\alias{merge_DSC}
\title{merge_DSC}

\description{
Merges multiple \code{DSC}s into a single \code{DSC_Static}.
}

\usage{
	merge_DSC(...)
}

\arguments{
	\item{...}{The \code{DSC}s to be merged.}
}

\details{
This method is for combining multiple \code{DSC}s. This is useful if clustering is performed in a
distributed manner and needs to be combined at the end.
}

\value{
A \code{DSC_Static} is returned. It possesses the merged centers, the merged
microclusters, and the weights of the returned centers.
}

\seealso{
	\code{\link{DSC_Static}}
}

\examples{
# 3 clusters with 5% noise
dsd <- DSD_Gaussian_Static(k=3, noise=0.05)

dsc1 <- DSC_tNN(threshold=0.05, lambda=.01)
dsc2 <- DSC_tNN(threshold=0.05, lambda=.01)
cluster(dsc1, dsd, 500)
cluster(dsc1, dsd, 500)
dsc1
dsc2

# prune the data and collect the DSC_Static
dsc <- merge_DSC(dsc1,dsc2)

dsc
}
