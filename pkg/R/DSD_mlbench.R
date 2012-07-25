DSD_mlbench <- function(method,...) {

	# creating the DSD object
    l <- list(description = paste("mlbench",method),
    	method = method,
    	variables = list(...)
    	)
    class(l) <- c("DSD_mlbench","DSD_R","DSD")
    l
}

get_points.DSD_mlbench <- function(x, n=1, assignment = FALSE,...) {
    methods <- c("2dnormals","cassini","circle","cuboids","friedman1","friedman2","friedman3","hypercube",
    "peak","ringnorm","shapes","simplex","smiley","spirals","threenorm","twonorm","waveform","xor")
	
	m <- pmatch(tolower(x$method),tolower(methods)) #finds index of partial match in array of methods
	
	if(length(x$variables)<=0) {
		if(m == 1)
			d <- mlbench.2dnormals(n)
		else if(m == 2)
			d <- mlbench.cassini(n)
		else if(m == 3)
			d <- mlbench.circle(n)
		else if(m == 4)
			d <- mlbench.cuboids(n)
		else if(m == 5)
			d <- mlbench.friedman1(n)
		else if(m == 6)
			d <- mlbench.friedman2(n)
		else if(m == 7)
			d <- mlbench.friedman3(n)
		else if(m == 8)
			d <- mlbench.hypercube(n)
		else if(m == 9)
			d <- mlbench.peak(n)
		else if(m == 10)
			d <- mlbench.ringnorm(n)
		else if(m == 11)
			d <- mlbench.shapes(n)
		else if(m == 12)
			d <- mlbench.simplex(n)
		else if(m == 13)
			d <- mlbench.smiley(n)
		else if(m == 14)
			d <- mlbench.spirals(n)
		else if(m == 15)
			d <- mlbench.threenorm(n)
		else if(m == 16)
			d <- mlbench.twonorm(n)
		else if(m == 17)
			d <- mlbench.waveform(n)
		else if(m == 18)
			d <- mlbench.xor(n)
		else
			stop("Invalid datagen")
		}
	else {
			if(m == 1)
			d <- mlbench.2dnormals(n,unlist(x$variables))
		else if(m == 2)
			d <- mlbench.cassini(n,unlist(x$variables))
		else if(m == 3)
			d <- mlbench.circle(n,unlist(x$variables))
		else if(m == 4)
			d <- mlbench.cuboids(n,unlist(x$variables))
		else if(m == 5)
			d <- mlbench.friedman1(n,unlist(x$variables))
		else if(m == 6)
			d <- mlbench.friedman2(n,unlist(x$variables))
		else if(m == 7)
			d <- mlbench.friedman3(n,unlist(x$variables))
		else if(m == 8)
			d <- mlbench.hypercube(n,unlist(x$variables))
		else if(m == 9)
			d <- mlbench.peak(n,unlist(x$variables))
		else if(m == 10)
			d <- mlbench.ringnorm(n,unlist(x$variables))
		else if(m == 11)
			d <- mlbench.shapes(n,unlist(x$variables))
		else if(m == 12)
			d <- mlbench.simplex(n,unlist(x$variables))
		else if(m == 13)
			d <- mlbench.smiley(n,unlist(x$variables))
		else if(m == 14)
			d <- mlbench.spirals(n,unlist(x$variables))
		else if(m == 15)
			d <- mlbench.threenorm(n,unlist(x$variables))
		else if(m == 16)
			d <- mlbench.twonorm(n,unlist(x$variables))
		else if(m == 17)
			d <- mlbench.waveform(n,unlist(x$variables))
		else if(m == 18)
			d <- mlbench.xor(n,unlist(x$variables))
		else
			stop("Invalid datagen")
		}	
		
	
	rand <- sample(1:n,n,replace=F)
	
	dat <- d$x[rand,]
	
	if(assignment) {
		attr(dat,"assignment")<-d$classes[rand]
	}
	
	dat
}

print.DSD_mlbench <- function(x, ...) {
    cat(paste('DSD - Data Stream Datasource:', x$description, '\n'))
}
