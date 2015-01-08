\name{saveMOA}
\alias{save}
\alias{saveMOA}
\alias{load}
\title{Save streamMOA Models to Disk}
\description{
    streamMOA models are Java objects and cannot be saved using the default R 
  mechanism, but have to be serialized first. saveMOA() takes care of this.
    }

\usage{
saveMOA(object, file, ...)
}
\arguments{
  \item{object}{an object of class DSC_MOA.}
  \item{file}{file name.}
  \item{...}{further arguments are passed on to save().}
}

\details{
  Clusterings in MOA are Java objects. Standard R save and load do only store 
  R objects and pointers to Java objects will be lost. \pkg{rJava} provides 
  \code{.jcache()} to serialize Java objects into a cache in R which is correctly
  saved and loaded. This can be manually done via \code{.jcache(x$javaObj)} where
  \code{x} is the DSV_MOA object.
  Note that the cache will increase memory usage and needs 
  to be updated before saving the model. \code{saveMOA()} provides a convenient
  function which creates/updates the cache automatically before saving the model
  using \code{saveRDS()}.
}

\seealso{
	\code{\link{saveRDS}},
	\code{\link{readRDS}},
	\code{\link[rJava]{.jcache}}
}

\examples{
# 3 clusters with 5% noise
stream <- DSD_Gaussians(k=3, d=2, noise=.05)

# Cluster with DenStream  
denstream <- DSC_DenStream(epsilon = .1)
update(denstream, stream, 500)
denstream

# Save model. This creates/updates an R cache of the model first. Just using
# save() does not work!
saveMOA(denstream, file="denstream.rds")  
rm(denstream)
  
  
# Restore from disk  
denstream <- readRDS(file="denstream.rds")
denstream

# Clean up
unlink("denstream.rds")
}
