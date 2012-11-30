DSD_Data <- function(data, loop=FALSE, random=FALSE, scale = FALSE) {

    datas <- c("BostonHousing", "BostonHousing2", "BreastCancer", 
	    "DNA", "Glass", "Ionosphere", "LetterRecognition", 
	    "Ozone", "PimaIndiansDiabetes", "Satellite", "Servo", 
	    "Shuttle", "Sonar", "Soybean", "Vehicle", "Vowel", "Zoo", 
	    "HouseVotes84")

    if(missing(data)) {
	cat("Available data sets:\n")
	print(datas)
	return(invisible(datas))
    }

    #finds index of partial match in array of datas
    m <- pmatch(tolower(data),tolower(datas)) 
    if(is.na(m)) stop("DSD_Data: Invalid data name")


    state <- new.env()
    assign("counter", 1L, envir = state)

    data(list=datas[m], package="mlbench", envir=environment())
    x <- get(datas[m])

    if(m == 1 || m == 2 || m == 8) {## BostonHousing, BostonHousing2, Ozone 
	d <- x
	a <- NULL
    }
    else if(m == 3) { ## BreastCancer
	d <- x[,2:10]
	a <- as.numeric(x[,11])
    }
    else if(m == 4) { ## DNA
	d <- x[,1:180]
	a <- x[,181]
	levels(a)<-1:3
	a <- as.numeric(a)
    }
    else if(m == 5) { ## Glass
	d <- x[,1:9]
	a <- x[,10]
    }
    else if(m == 6) { ## Ionosphere
	d <- x[,1:34]
	a <- as.numeric(x[,35])
    }
    else if(m == 7) { ## LetterRecognition
	d <- x[,2:17]
	a <- as.numeric(x[,1])
    }
    else if(m == 9) { ## PimaIndiansDiabetes
	d <- x[,1:8]
	a <- as.numeric(x[,9])
    }
    else if(m == 10) { ## Satellite
	d <- x[,1:36]
	a <- as.numeric(x[,37])
    }
    else if(m == 11) { ## Servo
	d <- x[,1:4]
	d[,1] <- as.numeric(d[,1])
	d[,2] <- as.numeric(d[,2])
	a <- x[,5]
    }
    else if(m == 12) { ## Shuttle
	d <- x[,1:9]
	a <- as.numeric(x[,10])
    }
    else if(m == 13) { ## Sonar
	d <- x[,1:60]
	a <- as.numeric(x[,61])
    }
    else if(m == 14) { ## Soybean
	d <- x[,2:36]
	a <- as.numeric(x[,1])
    }
    else if(m == 15) { ## Vehicle
	d <- x[,1:18]
	a <- as.numeric(x[,19])
    }
    else if(m == 16) { ## Vowel
	d <- x[,1:10]
	a <- as.numeric(x[,11])
    }
    else if(m == 17) { ## Zoo
	d <- x[,1:16]
	a <- as.numeric(x[,17])
    }
    else if(m == 18) { ## HouseVotes84
	d <- matrix(0,nrow(x),ncol(x))
	d[which(is.na(x[,2:17]))]<--1
	d[which(x[,2:17]=='n')]<-0
	d[which(x[,2:17]=='y')]<-1
	a <- rep(0,nrow(x))
	a[which(x[,1]=='democrat')] <- 1
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
