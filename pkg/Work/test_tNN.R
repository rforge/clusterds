library("stream")

#source("../R/AAA.R")
#source("../R/cluster.R")
#source("../R/tNN.R")

## create stream
#dsd <- DSD_Static(k=2, d=2, mu=rbind(c(0,0),c(1,1)))
dsd <- DSD_Static(k=3, d=2)

d <- getPoints(dsd, 1000, assignment = TRUE)

plot(d, col=attr(d, "assignment"))
pairs(d, col=attr(d, "assignment"))

## cluster
x <- DSC_tNN(, threshold=.4)

cluster(x, dsd, 1000)

pairs(get_centers(x))

nclusters(x)
plot(x)

# check if the clusters are on top of the points
plot(getPoints(dsd, 1000), col="grey")
points(getCenters(x), col="red", pch=3)

