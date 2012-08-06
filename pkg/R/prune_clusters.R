prune_clusters <- function(dsc,prop=.05) {

	 w <- get_weights(dsc,scale=NULL)
	 
	 o <- order(w)
	 
	o <- o[w[o] > quantile(w[o],prob=(1-prop))]

	 DSC_Static(get_centers(dsc)[o,],get_microclusters(dsc),w[o])
}