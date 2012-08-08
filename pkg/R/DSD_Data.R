DSD_Data <- function(data, loop=FALSE) {

    state <- new.env()
    assign("counter", 1L, envir = state)

	methods <- c("BostonHousing", "BostonHousing2", "BreastCancer", "DNA", "Glass", "Ionosphere", "LetterRecognition", "Ozone", "PimaIndiansDiabetes", "Satellite", "Servo", "Shuttle", "Sonar", "Soybean", "Vehicle", "Vowel", "Zoo", "HouseVotes84")
	
	m <- pmatch(tolower(data),tolower(methods)) #finds index of partial match in array of methods
	
	if(m == 1) {
		data(BostonHousing)
		d <- BostonHousing
		a <- NULL
	}
	else if(m == 2) {
		data(BostonHousing2)
		d <- BostonHousing2
		a <- NULL
	}
	else if(m == 3) {
		data(BreastCancer)
		d <- BreastCancer[,2:10]
		a <- BreastCancer[,11]
	}
	else if(m == 4) {
		data(DNA)
		d <- DNA[,1:180]
		a <- DNA[,181]
		levels(a)<-1:3
		a <- as.numeric(a)
	}
	else if(m == 5) {
		data(Glass)
		d <- Glass[,1:9]
		a <- Glass[,10]
	}
	else if(m == 6) {
		data(Ionosphere)
		d <- Ionosphere[,1:34]
		a <- as.numeric(Ionosphere[,35])
	}
	else if(m == 7) {
		data(LetterRecognition)
		d <- LetterRecognition[,2:17]
		a <- as.numeric(LetterRecognition[,1])
	}
	else if(m == 8) {
		data(Ozone)
		d <- Ozone
		a <- NULL
	}
	else if(m == 9) {
		data(PimaIndiansDiabetes)
		d <- PimaIndiansDiabetes[,1:8]
		a <- as.numeric(PimaIndiansDiabetes[,9])
	}
	else if(m == 10) {
		data(Satellite)
		d <- Satellite[,1:36]
		a <- as.numeric(Satellite[,37])
	}
	else if(m == 11) {
		data(Servo)
		d <- Servo[,1:4]
		d[,1] <- as.numeric(d[,1])
		d[,2] <- as.numeric(d[,2])
		a <- Servo[,5]
	}
	else if(m == 12) {
		data(Shuttle)
		d <- Shuttle[,1:9]
		a <- as.numeric(Shuttle[,10])
	}
	else if(m == 13) {
		data(Sonar)
		d <- Sonar[,1:60]
		a <- as.numeric(Sonar[,61])
	}
	else if(m == 14) {
		data(Soybean)
		d <- Soybean[,2:36]
		a <- as.numeric(Soybean[,1])
	}
	else if(m == 15) {
		data(Vehicle)
		d <- Vehicle[,1:18]
		a <- as.numeric(Vehicle[,19])
	}
	else if(m == 16) {
		data(Vowel)
		d <- Vowel[,1:10]
		a <- as.numeric(Vowel[,11])
	}
	else if(m == 17) {
		data(Zoo)
		d <- Zoo[,1:16]
		d[,1] <- as.numeric(d[,1])
		d[,2] <- as.numeric(d[,2])
		d[,3] <- as.numeric(d[,3])
		d[,4] <- as.numeric(d[,4])
		d[,5] <- as.numeric(d[,5])
		d[,6] <- as.numeric(d[,6])
		d[,7] <- as.numeric(d[,7])
		d[,8] <- as.numeric(d[,8])
		d[,9] <- as.numeric(d[,9])
		d[,10] <- as.numeric(d[,10])
		d[,11] <- as.numeric(d[,11])
		d[,12] <- as.numeric(d[,12])
		d[,14] <- as.numeric(d[,14])
		d[,15] <- as.numeric(d[,15])
		d[,16] <- as.numeric(d[,16])
		a <- as.numeric(Zoo[,17])
	}
	else if(m == 18) {
		data(HouseVotes84)
		d <- matrix(0,nrow(HouseVotes84),ncol(HouseVotes84))
		d[which(is.na(HouseVotes84[,2:17]))]<--1
		d[which(HouseVotes84[,2:17]=='n')]<-0
		d[which(HouseVotes84[,2:17]=='y')]<-1
		a <- rep(0,nrow(HouseVotes84))
		a[which(HouseVotes84[,1]=='democrat')] <- 1
	}
	else
		stop("Invalid data")
	

	# creating the DSD object
    l <- list(description = paste("mlbench",data),
	    strm = d,
	    state = state,
	    d = ncol(d),
	    k = length(unique(a)),
	    loop = loop,
	    assignment = a)
    class(l) <- c("DSD_Wrapper","DSD_R","DSD")
    l
}


get_points.DSD_Data <- function(x, n=1, assignment = FALSE,...) {
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
	if(assignment && !is.null(a)) {a <- x$assignment[x$state$counter:(x$state$counter + n -1L)]}
	x$state$counter <- x$state$counter + n
    }else{
	### we need to loop!


	# take what is left and reset counter
	d <- x$strm[x$state$counter:nrow(x$strm),] 
	if(assignment && !is.null(a)) {a <- x$assignment[x$state$counter:nrow(x$strm)]}
	togo <- n-n_left
	x$state$counter <- 1L

	while(togo > 0L) {
	    n_left <- nrow(x$strm) - x$state$counter + 1L

	    if(n_left < togo) {
		# take the whole stream
		d <- rbind(d, x$strm)
		if(assignment && !is.null(a)) a <- rbind(a,x$assignment)
		togo <- togo - n_left
	    }else{
		# take the rest
		d <- rbind(d, x$strm[1:(x$state$counter+togo-1),])
		if(assignment && !is.null(a)) {a <- rbind(a, x$assignment[1:(x$state$counter+togo-1)])}
		x$state$counter <- x$state$counter + togo
		togo <- 0L
	    }
	}
    }

	if(assignment && !is.null(a)) {attr(d,"assignment")<- a}

    d
}

print.DSD_Data <- function(x, ...) {
    NextMethod() # calling the super classes print()
    pos <- x$state$counter
    if (pos>nrow(x$strm)) 
	if (!x$loop) pos <- "'end'" else pos <- 1
    cat(paste('Contains', nrow(x$strm), 
		    'data points, currently at position', pos, 
		    'loop is', x$loop, '\n'))
}
