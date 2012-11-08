### combine several DSD clusterings (only the centers)

merge_DSC <- function(...) {
    centers <- do.call("rbind",lapply(list(...),
		    function(x) get_centers(x) ))
    microclusters <- do.call("rbind",lapply(list(...),
		    function(x) get_microclusters(x) ))
    weights <- unlist(lapply(list(...),
		    function(x) get_weights(x) ))

    DSC_Static(centers, microclusters, weights)
}
