DSD_GaussianMoving <- function(t = 10,n = 20) {

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
       x = c(0.08, 0.08, 0.08),
       y = c(0.08, 0.08, 0.08),
       rho = c(0, 0, 0)
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
    class(l) <- c("DSD_GaussianMoving","DSD_Wrapper","DSD_R","DSD")
    l
}
