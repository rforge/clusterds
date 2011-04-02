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
done by performing method calls directly in the JRI and unpackaging the 
multi-dimensional Java array into a local R data type.
}

\seealso{
	\code{\link{DSClusterer}},
	\code{\link{DSC_CobWeb}},
	\code{\link{DSC_ClusTree}},
	\code{\link{DSC_StreamKM}},
	\code{\link{DSC_DenStream}},
	\code{\link{DSC_Clustream}},
	\code{\link{DSData}}
}
