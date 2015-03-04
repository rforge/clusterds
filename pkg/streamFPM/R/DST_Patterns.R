

#implement summary() function for patterns
#count of patterns
#10 most frequent patterns

#attribute for patterns that has list of all items in the set

#function that returns vector of counts

#function for top ten?

#maybe just make patterns a vector?



#FIXME as itemsets
#as itemMatrix
as.itemSets = function(patterns, ...) {
  #is <- new("itemsets", items=as(unclass(patterns), "itemMatrix"), quality=data.frame(support=attr(patterns, "counts")))
  is <- new("itemsets", items=as(attr(patterns, "sets"), "itemMatrix"), quality=data.frame(support = unclass(patterns)))
}


print.DST_Patterns = function(x, ...) {
  cat("Class:", paste(class(x), collapse=", "), "\n") 
  cat("Set of", length(x), "Patterns", "\n")
  
}

topN.DST_Patterns = function(x, n = 10) {
  if(length(x) < n)
    sort(x, decreasing = TRUE)[1:length(x)]
  else
    sort(x, decreasing = TRUE)[1:n]
}


summary.DST_Patterns <- function(x) {
  cat("Set of", length(x), "patterns \n")
  cat("Top 10 patterns", "\n")
  if(length(x) < 10)
    sort(x, decreasing = TRUE)[1:length(x)]
  else
    sort(x, decreasing = TRUE)[1:10]
  
}

getSets.DST_Patterns <- function(x) {
  attr(x, "sets")
}

as.vector.DST_Patterns <- function(x, ...) {
  unclass(x)
  attr(x, "sets") <- NULL
  x
}