# DSClusterer - DataStreamClusterer interface
# all DSC classes have these methods
# and an additional function to create the DSC

getCenters <- function(x, ...) UseMethod("getCenters")

nclusters <- function(x) nrow(getCenters(x))

print.DSC <- function(x, ...) {
    cat(paste('DSC - Data Stream Clusterer:', x$description, '\n'))
    ### FIXME: print number of clusters and other stats
}

plot.DSC <- function(x, ...) plot(getCenters(x), ...)

