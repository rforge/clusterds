DSD_mlbench <- function(method,...) {

    methods <- c("2dnormals","cassini","circle","cuboids","friedman1","friedman2","friedman3","hypercube",
    "peak","ringnorm","shapes","simplex","smiley","spirals","threenorm","twonorm","waveform","xor")
	
	m <- pmatch(tolower(method),tolower(methods)) #finds index of partial match in array of methods
	if(is.na(m)) stop("DSD_mlbench: Invalid datagen")
	
	# creating the DSD object
    l <- list(description = paste("mlbench",method),
    	method = method,
    	variables = list(...)
    	)
    class(l) <- c("DSD_mlbench","DSD_R","DSD")
    l
}

get_points.DSD_mlbench <- function(x, n=1, assignment = FALSE,...) {
	
	if(is.null(unlist(x$variables)))
		d <- do.call(paste("mlbench.",x$method,sep=""),list(n))
	else
		d <- do.call(paste("mlbench.",x$method,sep=""),list(n,unlist(x$variables)))
		
	rand <- sample(1:n,n,replace=F)
	
	dat <- d$x[rand,]
	
	if(assignment) {
		attr(dat,"assignment")<-d$classes[rand]
	}
	
	data.frame(dat)
}

print.DSD_mlbench <- function(x, ...) {
    cat(paste('DSD - Data Stream Datasource:', x$description, '\n'))
}
