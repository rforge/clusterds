\name{DSData}
\alias{DSData}
\title{DSData}
\description{The abstract base class for all DataStreamData classes.}

\details{
The \code{DSData} class cannot be instantiated, but it serves as a base class
from which all DSD objects inherit from. There is only one function that needs
to be implemented: \code{get_points()}. \code{DSData} also provides a generic
\code{print()} function that displays basic information about the class.

In addition to this function, each
DSD class also needs a constructor specific to that class. The links below
contain various DSD classes that have been implemented.
}

\seealso{
	\code{\link{DSClusterer}},
	\code{\link{DSD_Gaussian_Static}},
%%	\code{\link{DSD_Gaussian_Dynamic}},
	\code{\link{DSD_ReadStream}},
	\code{\link{DSD_DataFrame}},
}
