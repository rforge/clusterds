DSD_BarsAndGaussians <- function(noise = 0) {



    # creating the DSD object
    l <- list(description = "Bars and Gaussians",
	    d = 2,
	    k = 4,
	    noise = noise)
    class(l) <- c("DSD_BarsAndGaussians","DSD_R","DSD")
    l
}

get_points.DSD_BarsAndGaussians <- function(x, n=1, assignment = FALSE,...) {
    ### gaussians at (3,2.5) and (3,-2.5)
### bars at (-3,2.8) and (-3,-2.8)

	
	df <- data.frame()
        a <- numeric()

	for(i in 1:n) {
	
	type <- sample(0:4, 1, prob=c(x$noise,.5*(1-x$noise),.25*(1-x$noise),.5*(1-x$noise),.25*(1-x$noise)))
	
	if(type==0) {
		p <- runif(2, -6,6)
	}
	
	if(type==1) {
		### gaussian
		cen <- c(3,2.5)
		p <- rnorm(2)
		p <- p+cen
	}
	
	if(type==2) {
		### gaussian
		cen <- c(3,-2.5)
		p <- rnorm(2)
		p <- p+cen
	}
	
	if(type==3) {
		### bar
		cen <- c(-3, 2.8)
		hight <- 5
		p <- c(runif(1, -1,1), runif(1, -hight/2, hight/2))
		p <- p+cen
	}
	
	if(type==4) {
		### bar
		cen <- c(-3, -2.8)
		hight <- 5
		p <- c(runif(1, -1,1), runif(1, -hight/2, hight/2))
		p <- p+cen
	}
	
	if(assignment) {
		if(type==0) a <- c(a,as.integer(NA))
		else a <- c(a,type)
	}
	
	df <- rbind(df,p)
	
	}
	
	
	if(assignment) {
		attr(df,"assignment")<-a
	}
	
	names(df) <- c("x","y")
	
	df
}
