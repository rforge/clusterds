#get dsd, dsc and n

get_evaluation <- function (dsc,dsd,
	method = c("recall","precision", "f1", "ssq", "rand",
		"numCluster", "numClasses"),
	n = 1000, macro = TRUE) {
	
    d <- get_points(dsd, n, assignment = TRUE)
    c <- get_microclusters(dsc)
    
    predict <- get_assignment(dsc,d)
    actual <- attr(d, "assignment")
    
    if(length(c) < 1){
	warning("No Clusters available!")
	ret <-  rep(NA, length(method))
	names(ret) <- method
	return(ret)
    } 

    confusion <- table(actual, predict)


    sapply(method, function(x) evaluate(d, c, x, confusion, predict, dsc))
}

evaluate <- function(d, c, method, confusion, assignment, dsc) {
	#make a vector of all of the methods and then do a lot of if statements
	methods <- c("f1","recall","precision","numCluster","numClasses","fpr","ssq","rand")
	
	if(nrow(c) > 0) {
		m <- pmatch(tolower(method),tolower(methods)) #finds index of partial match in array of methods
		if(m < 7) {
			if(m == 1)
				x <- f1(confusion)
			if(m == 2)
				x <- recall(confusion)
			if(m == 3)
				x <- precision(confusion)
			if(m == 4)
				x <- numCluster(confusion)
			if(m == 5)
				x <- numClasses(confusion)
			if(m == 6)
				x <- fpr(confusion)
		} else {
			if(m == 7)
				x <- ssq(d,c,assignment)
			if(m == 8)
				x <- rand(d,c,assignment)
		}
	} else
		x = -1
	x
}

f1 <- function(confusion) {
	precision <- precision(confusion)
	recall <- recall(confusion)
	f1 <- (2*precision*recall)/(precision+recall)
	mean(f1)
}

recall <- function(confusion) {
	relativeDocs <- apply(confusion[,],1, FUN = sum)
	recall <- apply(confusion[,],2,function(x) if(relativeDocs[which.max(x)]) max(x)/relativeDocs[which.max(x)] else 0)
	mean(recall)
}

precision <- function(confusion) {
	precision <- apply(confusion[,],2,function(x) if(sum(x)>0)max(x)/sum(x) else 0)
	mean(precision)
}

fpr <- function(confusion) {
	relativeDocs <- apply(confusion[,],1, FUN = sum)
	recall <- apply(confusion[,],2,function(x) if(sum(relativeDocs[which(x != max(x))])) (sum(x)-max(x))/sum(relativeDocs[which(x != max(x))]) else 0)
	mean(recall)
}

numCluster <- function(confusion) {
	ncol(confusion)
}

numClasses <- function(confusion) {
	nrow(confusion)
}

ssq <- function(d,c,assignment = NULL) {
	dist = dist(d,c)
	#Find the minimum distance and save the class
	predict = apply(dist, 1, min)
	#if(!is.null(assignment))
	#	predict <- unlist(lapply(predict, function(y) assignment[y]))
	sum(predict)
}

rand <- function(d,c,assignment = NULL) {
	dist <- dist(d,c)
	#Find the minimum distance and save the class
	predict <- apply(dist, 1, which.min)
	if(!is.null(assignment))
		predict <- unlist(lapply(predict, function(y) assignment[y]))
	#Get the actual class
	actual <- attr(d, "assignment")
	predictPairs <- unlist(do.call(rbind,lapply(predict,function(x) lapply(predict,function(y) x==y))))
	actualPairs <- unlist(do.call(rbind,lapply(actual,function(x) lapply(actual,function(y) x==y))))
	
	ab <- sum(predictPairs == actualPairs)
	rand <- ab/length(predictPairs)
	rand
}

silhouette <- function(d,c,assignment = NULL) {}

get_confusionMatrix <- function(d,c,predict) {
	#Get the actual class
	actual <- attr(d, "assignment")
	
	actual[is.na(actual)]<- 0
	
	if(0 %in% actual)
		actual <- actual + 1
	
	result <- cbind(actual,predict)
	#Compute the sparse matrix
	confusion <- sparseMatrix(i = result[,1],j = result[,2], x = 1)
	confusion
}
