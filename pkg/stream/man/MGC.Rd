\name{MGC}
\alias{MGC}
\alias{MGC_Random}
\alias{MGC_Function}
\alias{MGC_Linear}
\alias{MGC_Noise}

\title{
Moving Generator Cluster
}
\description{
Creates an evolving cluster for a \code{DSD_MG}. 
}
\usage{
MGC_Linear() 
MGC_Function(variance, density, center, cluster = NA)
MGC_Random(variance, density, center, randomness) 
MGC_Noise(density, range)
}

\arguments{
  \item{variance}{The variance of the cluster. For \code{MGC_Function}, this attribute is a function. Otherwise, the variance of the cluster is a static number or a list that has a length equal to the cluster's dimensionality.}
  \item{center}{A list that defines the center of the cluster. The list should have a length equal to the dimensionality. For \code{MGC_Function}, this list consists of functions that define the movement of the cluster. For \code{MGC_Random}, this attribute defines the beginning location for the \code{MGC} before it begins moving.}
  \item{density}{The density of the cluster. For \code{MGC_Function}, this attribute is a function and defines the density of a cluster at a given timestamp.}
  \item{cluster}{The name of the cluster. The cluster attribute is useful if a user wishes to combine multiple \code{MGC}s to create a single macro cluster. To accomplish this, the user would simply assign each of the \code{MGC}s the same cluster name.}
  \item{randomness}{The maximum amount the cluster will move during one time step.}
  \item{range}{The area in which the noise should appear.}
}
\details{
An \code{MGC} describes a single cluster for use within an \code{DSD_MG}. There are currently four different \code{MGC}s that allow a user to express many different behaviors within a single data stream.

An \code{MGC_Linear} creates an evolving gaussian cluster for a \code{DSD_MG} who's behavior is determined by several keyframes.

An \code{MGC_Function} allows for a creation of a \code{DSD_MG} that is defined by functions of time.

An \code{MGC_Random} allows for a creation of a \code{DSD_MG} that moves randomly.

An \code{MGC_Noise} allows for a creation of noise within a \code{DSD_MG}.

}


\seealso{
\code{\link{DSD_MG}} for details on how to use an \code{MGC} within a \code{DSD}
}
\examples{
  #Example of several MGC_Randoms
  dsd <- DSD_MG()
  c1 <- MGC_Random(c(50,50),1,1,1)
  c2 <- MGC_Random(c(50,50),1,1,1)
  
  add_cluster(dsd,c1)
  add_cluster(dsd,c2)
  \dontrun{animate_data(dsd,2500,xlim=c(0,100),ylim=c(0,100))}
  
  
  #Example of several MGC_Functions
  dsd <- DSD_MG()
  c1 <- MGC_Function(list(function(t){1},function(t){100}),function(t){25},list(function(t){t},function(t){t}))
  c2 <- MGC_Function(function(t){.1*t},function(t){25},list(function(t){sin(t/10)*50+50},function(t){t}))
  
  add_cluster(dsd,c1)
  add_cluster(dsd,c2)
  \dontrun{ animate_data(dsd,5000,xlim=c(0,100),ylim=c(0,100))}
  
  
  #Example of several MGC_Linears
  dsd <- DSD_MG()
  c1 <- MGC_Linear()
  c2 <- MGC_Linear()
  add_keyframe(c1,1,10,10,c(0,0),cluster=1)
  add_keyframe(c1,50,10,10,c(50,50),cluster=1)
  add_keyframe(c1,100,10,10,c(50,100),cluster=1)
  add_keyframe(c2,1,10,10,c(0,0),cluster=1)
  add_keyframe(c2,50,10,10,c(50,50),cluster=2)
  add_keyframe(c2,100,10,10,c(100,50),cluster=2)

  add_cluster(dsd,c1)
  add_cluster(dsd,c2)
  \dontrun{animate_data(dsd,2500,xlim=c(0,100),ylim=c(0,100))}
  
  
  #Example of an MGC_Noise
  dsd <- DSD_MG()
  c1 <- MGC_Random(1,1,c(50,50),1)
  c2 <- MGC_Noise(1,rbind(c(-20,120),c(-20,120)))
  
  add_cluster(dsd,c1)
  add_cluster(dsd,c2)
  \dontrun{animate_data(dsd,2500,xlim=c(0,100),ylim=c(0,100))}
}

