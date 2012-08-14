prune_clusters <- function(dsc, threshold=.05, prob=TRUE) {

	 w <- get_weights(dsc, scale=NULL)
	 
	 o <- order(w)
	 
	 if(prob)
	 	o <- o[w[o] > quantile(w,prob=threshold)]
	 else
	 	o <- head(o,length(o)*(1-threshold))

	 DSC_Static(get_centers(dsc)[o,],get_microclusters(dsc),w[o])
}
