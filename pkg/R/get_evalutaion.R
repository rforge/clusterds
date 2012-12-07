#get dsd, dsc and n

get_evaluation <- function (dsc, dsd,
	method, n = 1000, 
	type=c("auto", "micro", "macro"), assign="micro") {

    if(missing(method)) method <- c("numCluster","numClasses", "f1","recall",
	    "precision", "purity" ,"fpr","ssq","jaccard",
	    "rand","rand_HA","rand_MA","rand_FM")

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
	methods <- c("f1","recall","precision","numCluster","numClasses","fpr","ssq","rand","jaccard","rand_HA","rand_MA","rand_FM", "purity")

	
	m <- pmatch(tolower(method),tolower(methods)) #finds index of partial match in array of methods
	
	if(is.na(m)){
		stop("Invalid measure.")
	}
	
	if(m == 1)
		x <- f1(actual, predict)
	else if(m == 2)
		x <- recall(actual, predict)
	else if(m == 3)
		x <- precision(actual, predict)
	else if(m == 4)
		x <- numCluster(actual, predict)
	else if(m == 5)
		x <- numClasses(actual, predict)
	else if(m == 6)
		x <- fpr(actual, predict)
	else if(m == 7)
		x <- ssq(points,centers)
	else if(m == 8)
		x <- rand(predict,actual)
	else if(m == 9)
		x <- jaccard(predict,actual)
	else if(m == 10)
		x <- HA(predict,actual)
	else if(m == 11)
		x <- MA(predict,actual)
	else if(m == 12)
		x <- FM(predict,actual)
	else if(m == 13) ### purity is precision
		x <- precision(actual, predict)
	else
		stop(paste(method,"is not a valid evaluation method."))
	x
}

f1 <- function(actual, predict) {
	precision <- precision(actual, predict)
	recall <- recall(actual, predict)
	f1 <- (2*precision*recall)/(precision+recall)
	mean(f1)
}

recall <- function(actual, predict) {
    confusion <- table(actual, predict)
    confusion <- cbind(confusion,0)
	relativeDocs <- apply(confusion[,],1, FUN = sum)
	recall <- apply(confusion[,],2,function(x) if(relativeDocs[which.max(x)]) max(x)/relativeDocs[which.max(x)] else 0)
	mean(recall[1:length(recall)-1])
}

precision <- function(actual, predict) {
    confusion <- table(actual, predict)
    confusion <- cbind(confusion,0)
	precision <- apply(confusion[,],2,function(x) if(sum(x)>0)max(x)/sum(x) else 0)
	mean(precision[1:length(precision)-1])
}

fpr <- function(actual, predict) {
    confusion <- table(actual, predict)
    confusion <- cbind(confusion,0)
	relativeDocs <- apply(confusion[,],1, FUN = sum)
	recall <- apply(confusion[,],2,function(x) if(sum(relativeDocs[which(x != max(x))])) (sum(x)-max(x))/sum(relativeDocs[which(x != max(x))]) else 0)
	mean(recall[1:length(recall)-1])
}

numCluster <- function(actual, predict) {
    confusion <- table(actual, predict)
	ncol(confusion)
}

numClasses <- function(actual, predict) {
    confusion <- table(actual, predict)
	nrow(confusion)
}

ssq <- function(points,centers) {
	dist <- dist(points,centers)
	mindistance <- apply(dist, 1, min)
	sum(mindistance)
}

rand <- function(predict,actual) {
	as.numeric(adjustedRand(predict,actual,"Rand"))
}

HA <- function(predict,actual) {
	as.numeric(adjustedRand(predict,actual,"HA"))
}

MA <- function(predict,actual) {
	as.numeric(adjustedRand(predict,actual,"MA"))
}

FM <- function(predict,actual) {
	as.numeric(adjustedRand(predict,actual,"FM"))
}

jaccard <- function(predict,actual) {
	as.numeric(adjustedRand(predict,actual,"Jaccard"))
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
