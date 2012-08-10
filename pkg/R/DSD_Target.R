DSD_Target <- function() {



    # creating the DSD object
    l <- list(description = "Target Shape",
	    d = 2,
	    k = 2)
    class(l) <- c("DSD_Target","DSD_R","DSD")
    l
}

get_points.DSD_Target <- function(x, n=1, assignment = FALSE,...) {
    ## ball
	sd <- .05
	nc <- n/2 + 1
	datb <- cbind(x=rnorm(nc, sd=sd),y=rnorm(nc, sd=sd))

	## ring
	nr <- n/2 + 1
	sdr <- .01
	avgr <- .3
	r <- avgr+rnorm(nr, sd=sdr)
	angle <- runif(nr, 0,2*pi)

	datr <- cbind(x=cos(angle)*r, y=sin(angle)*r)

	dat <- rbind(datb, datr)
	
	rand <- sample(1:n,n,replace=F)
	
	dat <- dat[rand,]
	
	if(assignment) {
		ab <- rep(1,nc)
		ar <- rep(2,nr)
		attr(dat,"assignment")<-c(ab,ar)[rand]
	}
	
	data.frame(dat)
}
