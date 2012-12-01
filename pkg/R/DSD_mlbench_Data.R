DSD_mlbench_Data <- function(data, loop=FALSE, random=FALSE, scale = FALSE) {

    state <- new.env()
    assign("counter", 1L, envir = state)

	datas <- c("BostonHousing", "BostonHousing2", "BreastCancer", "DNA", "Glass", "Ionosphere", "LetterRecognition", "Ozone", "PimaIndiansDiabetes", "Satellite", "Servo", "Shuttle", "Sonar", "Soybean", "Vehicle", "Vowel", "Zoo", "HouseVotes84")
	
	m <- pmatch(tolower(data),tolower(datas)) #finds index of partial match in array of datas
	
	
	if(is.na(m))
		stop("DSD_Data: Invalid data name")
	
	if(m == 1) {
		do.call("data",list(datas[m]))
		d <- BostonHousing
		a <- NULL
	}
	else if(m == 2) {
		do.call("data",list(datas[m]))
		d <- BostonHousing2
		a <- NULL
	}
	else if(m == 3) {
		do.call("data",list(datas[m]))
		d <- BreastCancer[,2:10]
		a <- as.numeric(BreastCancer[,11])
	}
	else if(m == 4) {
		do.call("data",list(datas[m]))
		d <- DNA[,1:180]
		a <- DNA[,181]
		levels(a)<-1:3
		a <- as.numeric(a)
	}
	else if(m == 5) {
		do.call("data",list(datas[m]))
		d <- Glass[,1:9]
		a <- Glass[,10]
	}
	else if(m == 6) {
		do.call("data",list(datas[m]))
		d <- Ionosphere[,1:34]
		a <- as.numeric(Ionosphere[,35])
	}
	else if(m == 7) {
		do.call("data",list(datas[m]))
		d <- LetterRecognition[,2:17]
		a <- as.numeric(LetterRecognition[,1])
	}
	else if(m == 8) {
		do.call("data",list(datas[m]))
		d <- Ozone
		a <- NULL
	}
	else if(m == 9) {
		do.call("data",list(datas[m]))
		d <- PimaIndiansDiabetes[,1:8]
		a <- as.numeric(PimaIndiansDiabetes[,9])
	}
	else if(m == 10) {
		do.call("data",list(datas[m]))
		d <- Satellite[,1:36]
		a <- as.numeric(Satellite[,37])
	}
	else if(m == 11) {
		do.call("data",list(datas[m]))
		d <- Servo[,1:4]
		d[,1] <- as.numeric(d[,1])
		d[,2] <- as.numeric(d[,2])
		a <- Servo[,5]
	}
	else if(m == 12) {
		do.call("data",list(datas[m]))
		d <- Shuttle[,1:9]
		a <- as.numeric(Shuttle[,10])
	}
	else if(m == 13) {
		do.call("data",list(datas[m]))
		d <- Sonar[,1:60]
		a <- as.numeric(Sonar[,61])
	}
	else if(m == 14) {
		do.call("data",list(datas[m]))
		d <- Soybean[,2:36]
		a <- as.numeric(Soybean[,1])
	}
	else if(m == 15) {
		do.call("data",list(datas[m]))
		d <- Vehicle[,1:18]
		a <- as.numeric(Vehicle[,19])
	}
	else if(m == 16) {
		do.call("data",list(datas[m]))
		d <- Vowel[,1:10]
		a <- as.numeric(Vowel[,11])
	}
	else if(m == 17) {
		do.call("data",list(datas[m]))
		d <- Zoo[,1:16]
		a <- as.numeric(Zoo[,17])
	}
	else if(m == 18) {
		do.call("data",list(datas[m]))
		d <- matrix(0,nrow(HouseVotes84),ncol(HouseVotes84))
		d[which(is.na(HouseVotes84[,2:17]))]<--1
		d[which(HouseVotes84[,2:17]=='n')]<-0
		d[which(HouseVotes84[,2:17]=='y')]<-1
		a <- rep(0,nrow(HouseVotes84))
		a[which(HouseVotes84[,1]=='democrat')] <- 1
	}
	
	complete <- complete.cases(d)
	
	a <- a[complete]
	d <- d[complete,]
	
	
	if(random) {
		rand <- sample(1:length(a),length(a),replace=F)
		a <- a[rand]
		d <- d[rand,]
	}

	d <- apply(d,2,as.numeric)
	a <- as.numeric(a)

	if(scale)
		d <- scale(d)

	# creating the DSD object
    l <- list(description = paste("mlbench",data),
	    strm = data.frame(d),
	    state = state,
	    d = ncol(d),
	    k = length(unique(a)),
	    loop = loop,
	    assignment = a)
    class(l) <- c("DSD_Data","DSD_Wrapper","DSD_R","DSD")
    l
}
