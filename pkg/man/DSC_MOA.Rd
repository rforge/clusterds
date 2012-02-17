\name{DSC_MOA}
\alias{DSC_MOA}
\title{DSC_MOA}

\description{
An abstract class that inherits from the base DSC class, 
DSClusterer. All DSC objects that use underlying MOA code will inherit
this class because they have a specific way of operating.
}

\details{
\code{DSC_MOA} classes operate in a different way in that the centers of the
micro-clusters have to be extracted from the underlying Java object. This is
done by using rJava to perform method calls directly in the JRI and unpackaging the 
multi-dimensional Java array into a local R data type.
}

\references{
MOA: Massive Online Analysis, a Framework for Stream Classification and Clustering
Albert Bifet, Geoff Holmes, Bernhard Pfahringer, Philipp Kranen, Hardy Kremer, Timm Jansen, Thomas Seidl.
Journal of Machine Learning Research (JMLR) Workshop and Conference Proceedings.
Volume 11: Workshop on Applications of Pattern Analysis (2010).
}

\seealso{
	\code{\link{DSClusterer}},
%	\code{\link{DSC_CobWeb}},
	\code{\link{DSC_ClusTree}},
%	\code{\link{DSC_StreamKM}},
	\code{\link{DSC_DenStream}},
	\code{\link{DSC_CluStream}},
	\code{\link{DSData}}
}
