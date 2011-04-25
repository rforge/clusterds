\name{DSClusterer}
\alias{DSClusterer}
\title{DSClusterer}
\description{The abstract base class for all DataStreamClusterer (DSC) classes.}

\details{
The \code{DSClusterer} class cannot be instantiated, but it serves as a base class
from which all DSC objects inherit from. 

\code{DSClusterer} provides several generic functions that can operate on all
DSC subclasses: \code{get_centers()}, which returns the centers of the
micro-clusters generated; \code{nclusters()}, which returns the number of
micro-clusters generated; \code{print()}, which prints general data about the DSC;
and \code{plot()}, which will plot the centers of the micro-clusters.
}

\seealso{
	\code{\link{get_centers}},
	\code{\link{nclusters}},
	\code{\link{DSC_CobWeb}},
	\code{\link{DSC_ClusTree}},
	\code{\link{DSC_StreamKM}},
	\code{\link{DSC_DenStream}},
	\code{\link{DSC_CluStream}},
	\code{\link{DSData}}
}
