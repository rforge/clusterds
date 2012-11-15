### combine several DSD clusterings (only the centers)

merge_DSC <- function(...) {
	if('DSC_Macro' %in% class(list(...)[[1]])) {
    	centers <- do.call("rbind",lapply(list(...),
		    function(x) get_macroclusters(x) ))
	} else { centers = NULL }
    microclusters <- do.call("rbind",lapply(list(...),
		    function(x) get_microclusters(x) ))
    weights <- unlist(lapply(list(...),
		    function(x) get_weights(x) ))

    DSC_Static(centers, microclusters, weights)
}
