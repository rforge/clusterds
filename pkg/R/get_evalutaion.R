#get dsd, dsc and n

cluster_evaluation <- function(dsc, dsd, macro=NULL, method, 
	n=1000, type=c("auto", "micro", "macro"), assign="micro", pointInterval=100) {
	
	evaluations <- data.frame()
	for(i in 1:(n/pointInterval)) {
		points <- get_points(dsd,pointInterval,assignment=TRUE)
		wrapper <- DSD_Wrapper(points,0,loop=TRUE,assignment=attr(points,"assignment"))
		cluster(dsc,wrapper,pointInterval)
		if(is.null(macro)) {
			evaluations <- rbind(evaluations,get_evaluation(dsc,dsd,method,pointInterval,type,assign))
		} else {
			recluster(macro,dsc)
			evaluations <- rbind(evaluations,get_evaluation(macro,dsd,method,pointInterval,type,assign))
		}
	}
	
	evaluations
	
}

get_evaluation <- function (dsc, dsd, method, n = 1000, 
	type=c("auto", "micro", "macro"), assign="micro") {

    methods <- c(
	    "numClusters","numClasses",
	    "precision", "recall", "F1",
	    "purity", "fpr",
	    "SSQ",
	    "Euclidean", "Manhattan", "Rand", "cRand",
	    "NMI", "KP", "angle", "diag", "FM", "Jaccard", "PS")

    if(missing(method)) method <- methods
    else method <- methods[pmatch(tolower(method),tolower(methods))] 

    ### figure out type
    type <- get_type(dsc, type)

    c <- get_centers(dsc, type=type) 

    if(nrow(c)<1) {
	warning("No centers available!")
	return(0)
    }
   	
    d <- get_points(dsd, n, assignment = TRUE)
    actual <- attr(d, "assignment")
	
    predict <- get_assignment(dsc,d, type=assign)

    ### translate micro to macro cluster ids if necessary
    if(type=="macro" && assign=="micro") predict <- microToMacro(dsc, predict)
    else if (type!=assign) stop("type and assign are not compatible!")

    ### make points assigned to unassigned micro-clusters noise (0)
    predict[is.na(predict)] <- 0L

    ### remove noise
    noise <- which(is.na(actual))
    if(length(noise)>0) {
		predict <- predict[-noise]
		actual <- actual[-noise]
		d <- d[-noise,]
    }
    
    e <- sapply(method, function(x) evaluate(x, predict, actual, d, c))
    structure(e, type=type, assign=assign, class="stream_eval")

}

print.stream_eval <-  function(x, ...) {
    cat("Evaluation results for ", attr(x, "type"),"-clusters.\n", sep="")
    cat("Points were assigned to ", attr(x, "assign"),"-clusters.\n\n", sep="")
    names <- names(x)
    x <- as.numeric(x)
    names(x) <- names 
    print(x)
}

evaluate <- function(method, predict, actual, points, centers) {
	#make a vector of all of the methods and then do a lot of if statements
	methods <- c(
		"numClusters","numClasses",
		"precision", "recall", "F1",
		"purity", "fpr",
		"SSQ",
		"Euclidean", "Manhattan", "Rand", "cRand",
		"NMI", "KP", "angle", "diag", "FM", "Jaccard", "PS")

	
	#finds index of partial match in array of methods
	method <- methods[pmatch(tolower(method),tolower(methods))] 
	
	if(is.na(method)){
		stop("Invalid measure.")
	}
	
	switch(method,
		numClusters  = numClusters(actual, predict),
		numClasses  = numClasses(actual, predict),
		
		precision   = precision(actual, predict),
		recall	    = recall(actual, predict),
		F1	    = f1(actual, predict),
		
		purity	    = precision(actual, predict),
		fpr	    = recall(actual, predict),
	
		SSQ	    = ssq(points,centers),
		
		Euclidean   = clue_agreement(predict, actual, "euclidean"),
		Manhattan   = clue_agreement(predict, actual, "manhattan"),
		Rand	    = clue_agreement(predict, actual, "rand"),
		cRand	    = clue_agreement(predict, actual, "crand"),
		NMI	    = clue_agreement(predict, actual, "NMI"),
		KP	    = clue_agreement(predict, actual, "KP"),
		angle	    = clue_agreement(predict, actual, "angle"),
		diag	    = clue_agreement(predict, actual, "diag"),
		FM	    = clue_agreement(predict, actual, "FM"),
		Jaccard	    = clue_agreement(predict, actual, "jaccard"),
	#	purity	    = clue_agreement(predict, actual, "purity"),
		PS	    = clue_agreement(predict, actual, "PS")
	)
}

### helper
colMax <- function(x, which=FALSE) {
    if(!which) apply(x, 2, FUN=function(y) max(y))
    else {
	apply(x, 2, FUN=function(y) which.max(y))
    }
}

f1 <- function(actual, predict) {
	precision <- precision(actual, predict)
	recall <- recall(actual, predict)
	(2*precision*recall)/(precision+recall)
}

recall <- function(actual, predict) {
    confusion <- table(actual, predict)
    relevant <- rowSums(confusion)

    mean(colMax(confusion)/relevant[colMax(confusion, which=TRUE)])
}

precision <- function(actual, predict) {
    confusion <- table(actual, predict)
    
    mean(colMax(confusion)/colSums(confusion))

}

numClusters <- function(actual, predict) {
    length(unique(predict))
}

numClasses <- function(actual, predict) {
    length(unique(actual))
}

ssq <- function(points, centers) {
	dist <- dist(points,centers)
	mindistance <- apply(dist, 1, min)
	sum(mindistance)
}

clue_agreement <- function(predict, actual, method) {
    predict <- as.cl_hard_partition(predict)
    actual <- as.cl_hard_partition(actual)
    as.numeric(cl_agreement(cl_ensemble(predict, actual), method=method))
}

### silhouette <- function(d,c,assignment = NULL) {}

### this would need package Matrix
#get_confusionMatrix <- function(d,c,predict) {
#	#Get the actual class
#	actual <- attr(d, "assignment")
#	
#	actual[is.na(actual)]<- 0
#	
#	if(0 %in% actual)
#		actual <- actual + 1
#	
#	result <- cbind(actual,predict)
#	#Compute the sparse matrix
#	confusion <- sparseMatrix(i = result[,1],j = result[,2], x = 1)
#	confusion
#}
