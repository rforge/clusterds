DSD_Gaussian_Moving <- function(t = 10,n = 20) {

    state <- new.env()
    assign("counter", 1L, envir = state)

	k <- 3
	

##simulate data
mu <- cbind(
   x = c(0,0,1),
   y = c(0,0,1)
)

#sd_rho <- cbind(
#       x = c(0.2, 0.15, 0.05),
#       y = c(0.1, 0.04, 0.03),
#       rho = c(0, 0.7, 0.3)
#)

sd_rho <- cbind(
       x = c(0.2, 0.2, 0.2),
       y = c(0.1, 0.1, 0.1),
       rho = c(0.3, 0.3, 0.3)
)

Sigma <- lapply(1:nrow(sd_rho), FUN = function(i) rbind(
         c(sd_rho[i,"x"]^2, sd_rho[i, "rho"]*sd_rho[i,"x"]*sd_rho[i,"y"]),
         c(sd_rho[i,"rho"]*sd_rho[i,"x"]*sd_rho[i,"y"],sd_rho[i,"y"]^2))) 

sequence <- c(1,2,3)

EMMsim_sequence <- rep(sequence, n)

library("MASS")

sequ <- c()
ds <- data.frame()

# make a for loop
for (jj in 1:t){
    mu[2,"x"] = mu[2,"x"] + 1/t
    mu[2,"y"] = mu[2,"y"] + 1/t 
    EMMsim <- t(sapply(EMMsim_sequence, FUN = function(i)
		    mvrnorm(1, mu=mu[i,], Sigma=Sigma[[i]])))


    sequ <- append(sequ, EMMsim_sequence)
    ds <- rbind(ds, EMMsim)

}



    # creating the DSD object
    l <- list(description = "Gaussian Moving Stream",
	    strm = ds,
	    state = state,
	    d = ncol(ds),
	    k = k,
	    loop = FALSE,
	    assignment = sequ)
    class(l) <- c("DSD_Gaussian_Moving","DSD_R","DSD")
    l
}

get_points.DSD_Gaussian_Moving <- function(x, n=1, assignment = FALSE,...) {
    n <- as.integer(n)
   
    if(x$state$counter > nrow(x$strm)) {
	if(x$loop) x$state$counter <- 1L
	else stop("The stream is at its end!")
    }

    n_left <- nrow(x$strm) - x$state$counter + 1L
    
    if(n_left < n && !x$loop) stop("Not enought data points left in stream!")

    if(n_left >= n) {
	### regular case
	d <- x$strm[x$state$counter:(x$state$counter + n -1L),]
	if(assignment) {a <- x$assignment[x$state$counter:(x$state$counter + n -1L),]}
	x$state$counter <- x$state$counter + n
    }else{
	### we need to loop!


	# take what is left and reset counter
	d <- x$strm[x$state$counter:nrow(x$strm),] 
	if(assignment) {a <- x$assignment[x$state$counter:nrow(x$strm),]}
	togo <- n-n_left
	x$state$counter <- 1L

	while(togo > 0L) {
	    n_left <- nrow(x$strm) - x$state$counter + 1L

	    if(n_left < togo) {
		# take the whole stream
		d <- rbind(d, x$strm)
		if(assignment) a <- rbind(a,x$assignment)
		togo <- togo - n_left
	    }else{
		# take the rest
		d <- rbind(d, x$strm[1:(x$state$counter+togo-1),])
		if(assignment) {a <- rbind(a, x$assignment[1:(x$state$counter+togo-1),])}
		x$state$counter <- x$state$counter + togo
		togo <- 0L
	    }
	}
    }

	if(assignment) {attr(d,"assignment")<- a}

    d
}

print.DSD_Gaussian_Moving <- function(x, ...) {
    NextMethod() # calling the super classes print()
    pos <- x$state$counter
    if (pos>nrow(x$strm)) 
	if (!x$loop) pos <- "'end'" else pos <- 1
    cat(paste('Contains', nrow(x$strm), 
		    'data points, currently at position', pos, 
		    'loop is', x$loop, '\n'))
}
