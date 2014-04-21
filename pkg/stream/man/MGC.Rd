\name{MGC}
\alias{MGC}
\alias{MGC_Random}
\alias{MGC_Function}
\alias{MGC_Noise}
\alias{MGC_Linear}
\alias{add_keyframe}
\alias{get_keyframes}
\alias{remove_keyframe}


\title{Moving Generator Cluster}
\description{
Creates an evolving cluster for a \code{DSD_MG}. 
}
\usage{
MGC_Function(density, variance, center, cluster = NA)
MGC_Random(density, variance, center, randomness = 1 ) 
MGC_Noise(density, range)

MGC_Linear() 
add_keyframe(x, time, density = 1, variance = 1, center, cluster = NA, 
  reset = FALSE)
get_keyframes(x) 
remove_keyframe(x, time)
}

\arguments{
  \item{variance}{The variance of the cluster. For \code{MGC_Function}, this attribute is a function. Otherwise, the variance of the cluster is a static number or a list that has a length equal to the cluster's dimensionality.}
  \item{center}{A list that defines the center of the cluster. The list should have a length equal to the dimensionality. For \code{MGC_Function}, this list consists of functions that define the movement of the cluster. For \code{MGC_Random}, this attribute defines the beginning location for the \code{MGC} before it begins moving.}
  \item{density}{The density of the cluster. For \code{MGC_Function}, this attribute is a function and defines the density of a cluster at a given timestamp.}
  \item{cluster}{The name of the cluster. The cluster attribute is useful if a user wishes to combine multiple \code{MGC}s to create a single macro cluster. To accomplish this, the user would simply assign each of the \code{MGC}s the same cluster name.}
  \item{randomness}{The maximum amount the cluster will move during one time step.}
  \item{range}{The area in which the noise should appear.}
  \item{x}{An object of class \code{MGC_Linear}.}
  \item{time}{The time stamp the keyframe should be located or which keyframe should be removed.}
  \item{reset}{Should the cluster reset to the first keyframe (time 0) after
this keyframe is finished?}
}

\details{
An \code{MGC} describes a single cluster for use within an \code{DSD_MG}. There are currently four different \code{MGC}s that allow a user to express many different behaviors within a single data stream.

An \code{MGC_Linear} creates an evolving gaussian cluster for a \code{DSD_MG} who's behavior is determined by several keyframes. Keyframes can be added and removed.

An \code{MGC_Function} allows for a creation of a \code{DSD_MG} that is defined by functions of time.

An \code{MGC_Random} allows for a creation of a \code{DSD_MG} that moves randomly.

An \code{MGC_Noise} allows for a creation of noise within a \code{DSD_MG}.

}


\seealso{
\code{\link{DSD_MG}} for details on how to use an \code{MGC} within a \code{DSD}
}
\examples{
### Example of several MGC_Randoms
dsd <- DSD_MG(dimension=2,
  MGC_Random(density=1, variance=1, center=c(50,50)),
  MGC_Random(density=1, variance=1, center=c(50,50))
)
  
\dontrun{animate_data(dsd,2500,xlim=c(0,100),ylim=c(0,100), horizon=100)}
  
  
### Example of several MGC_Functions
dsd <- DSD_MG(dim = 2)

### cluster moving from bottom-left to top-right increasing variance
c1 <- MGC_Function(
  density = function(t){100},
  variance = function(t){.1*t},
  center = c(function(t){t}, function(t){t})
  )

### cluster moiving in a circle
c2 <- MGC_Function(
  density = function(t){25},
  variance = function(t){5},
  center= c(function(t){sin(t/10)*50+50}, function(t){cos(t/10)*50+50})
)
  
add_cluster(dsd,c1)
add_cluster(dsd,c2)
\dontrun{animate_data(dsd,10000,xlim=c(0,100),ylim=c(0,100), horizon=100)}
  
### Example of several MGC_Linears: A single cluster splits at time 50 into two.
### Note how the cluster label for c2 changes!
dsd <- DSD_MG(dim = 2)
c1 <- MGC_Linear()
add_keyframe(c1, time=1,  dens=10, var=10, center=c(0,0),   cluster=1)
add_keyframe(c1, time=50, dens=10, var=10, center=c(50,50), cluster=1)
add_keyframe(c1, time=100,dens=10, var=10, center=c(50,100),cluster=1)
add_cluster(dsd,c1)

c2 <- MGC_Linear()
add_keyframe(c2, time=1,  dens=10, var=10, center=c(0,0),   cluster=1)
add_keyframe(c2, time=50, dens=10, var=10, center=c(50,50), cluster=2)
add_keyframe(c2, time=100,dens=10, var=10, center=c(100,50),cluster=2)
add_cluster(dsd,c2)

\dontrun{animate_data(dsd,2500,xlim=c(0,100),ylim=c(0,100), horiz=100)}
  
### two fixed and a moving cluster  
dsd <- DSD_MG(dim = 2)
c1 <- MGC_Linear()
add_keyframe(c1, time = 0, dens=1, var=.01, center=c(0,0))
add_cluster(dsd, c1)
  
c2 <- MGC_Linear()
add_keyframe(c2, time = 0, dens=1, var=.01, center=c(1,1))
add_cluster(dsd, c2)

c3 <- MGC_Linear()
add_keyframe(c3, time = 0, dens=1, var=.01, center=c(0,0))
add_keyframe(c3, time = 1000, dens=1, var=.01, center=c(1,1))
add_keyframe(c3, time = 2000, dens=1, var=.01, center=c(0,0), reset=TRUE)
add_cluster(dsd, c3)

noise <- MGC_Noise(dens=.1, range=rbind(c(-.2,1.2),c(-.2,1.2)))
add_cluster(dsd, noise)
  
\dontrun{animate_data(dsd, 3.1*4000, xlim=c(-.2,1.2), ylim=c(-.2,1.2), horiz=200)}
}

