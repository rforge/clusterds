DSD_Data <- function(data, loop=FALSE) {

    state <- new.env()
    assign("counter", 1L, envir = state)

	datas <- c("BostonHousing", "BostonHousing2", "BreastCancer", "DNA", "Glass", "Ionosphere", "LetterRecognition", "Ozone", "PimaIndiansDiabetes", "Satellite", "Servo", "Shuttle", "Sonar", "Soybean", "Vehicle", "Vowel", "Zoo", "HouseVotes84")
	
	m <- pmatch(tolower(data),tolower(datas)) #finds index of partial match in array of datas
	
	
	if(is.na(m))
		stop("DSD_Data: Invalid data name")
	
	if(m == 1) {
		do.call("data",list(data))
		d <- BostonHousing
		a <- NULL
	}
	else if(m == 2) {
		do.call("data",list(data))
		d <- BostonHousing2
		a <- NULL
	}
	else if(m == 3) {
		do.call("data",list(data))
		d <- BreastCancer[,2:10]
		a <- BreastCancer[,11]
	}
	else if(m == 4) {
		do.call("data",list(data))
		d <- DNA[,1:180]
		a <- DNA[,181]
		levels(a)<-1:3
		a <- as.numeric(a)
	}
	else if(m == 5) {
		do.call("data",list(data))
		d <- Glass[,1:9]
		a <- Glass[,10]
	}
	else if(m == 6) {
		do.call("data",list(data))
		d <- Ionosphere[,1:34]
		a <- as.numeric(Ionosphere[,35])
	}
	else if(m == 7) {
		do.call("data",list(data))
		d <- LetterRecognition[,2:17]
		a <- as.numeric(LetterRecognition[,1])
	}
	else if(m == 8) {
		do.call("data",list(data))
		d <- Ozone
		a <- NULL
	}
	else if(m == 9) {
		do.call("data",list(data))
		d <- PimaIndiansDiabetes[,1:8]
		a <- as.numeric(PimaIndiansDiabetes[,9])
	}
	else if(m == 10) {
		do.call("data",list(data))
		d <- Satellite[,1:36]
		a <- as.numeric(Satellite[,37])
	}
	else if(m == 11) {
		do.call("data",list(data))
		d <- Servo[,1:4]
		d[,1] <- as.numeric(d[,1])
		d[,2] <- as.numeric(d[,2])
		a <- Servo[,5]
	}
	else if(m == 12) {
		do.call("data",list(data))
		d <- Shuttle[,1:9]
		a <- as.numeric(Shuttle[,10])
	}
	else if(m == 13) {
		do.call("data",list(data))
		d <- Sonar[,1:60]
		a <- as.numeric(Sonar[,61])
	}
	else if(m == 14) {
		do.call("data",list(data))
		d <- Soybean[,2:36]
		a <- as.numeric(Soybean[,1])
	}
	else if(m == 15) {
		do.call("data",list(data))
		d <- Vehicle[,1:18]
		a <- as.numeric(Vehicle[,19])
	}
	else if(m == 16) {
		do.call("data",list(data))
		d <- Vowel[,1:10]
		a <- as.numeric(Vowel[,11])
	}
	else if(m == 17) {
		do.call("data",list(data))
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
		do.call("data",list(data))
		d <- matrix(0,nrow(HouseVotes84),ncol(HouseVotes84))
		d[which(is.na(HouseVotes84[,2:17]))]<--1
		d[which(HouseVotes84[,2:17]=='n')]<-0
		d[which(HouseVotes84[,2:17]=='y')]<-1
		a <- rep(0,nrow(HouseVotes84))
		a[which(HouseVotes84[,1]=='democrat')] <- 1
	}
	

	# creating the DSD object
    l <- list(description = paste("mlbench",data),
	    strm = d,
	    state = state,
	    d = ncol(d),
	    k = length(unique(a)),
	    loop = loop,
	    assignment = a)
    class(l) <- c("DSD_Data","DSD_Wrapper","DSD_R","DSD")
    l
}
