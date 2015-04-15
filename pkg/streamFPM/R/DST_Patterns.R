

#implement summary() function for patterns
#count of patterns
#10 most frequent patterns

#attribute for patterns that has list of all items in the set

#function that returns vector of counts

#maybe just make patterns a vector?



#FIXME as itemsets
#as itemMatrix
as.itemsets = function(patterns, ...) {
  #is <- new("itemsets", items=as(unclass(patterns), "itemMatrix"), quality=data.frame(support=attr(patterns, "counts")))
  new("itemsets", items=as(attr(patterns, "sets"), "itemMatrix"),
      quality=data.frame(support = unclass(patterns / attr(patterns, "total_transactions") )))
}


print.Patterns <- function(x, ...) {
  cat("Class:", paste(class(x), collapse=", "), "\n") 
  cat("Set of", length(x), "Patterns", "\n")
  
}

topN <- function(x, n = 10) {
  if(length(x) < n)
    sort(x, decreasing = TRUE)[1:length(x)]
  else
    sort(x, decreasing = TRUE)[1:n]
}

summary.Patterns <- function(x) {
  cat("Set of", length(x), "patterns \n")
  cat("Top 10 patterns", "\n")
  if(length(x) < 10)
    sort(x, decreasing = TRUE)[1:length(x)]
  else
    sort(x, decreasing = TRUE)[1:10]
  
}

getSets <- function(x) {
  attr(x, "sets")
}

as.vector.Patterns <- function(x, ...) {
  unclass(x)
  attr(x, "sets") <- NULL
  attr(x, "decode_table") <- NULL
  attr(x, "total_transactions") <- NULL
  attr(x, "error") <- NULL
  x
}
