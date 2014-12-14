

#FIXME as itemsets
#as itemMatrix
as.itemSets = function(patterns, ...) {
  is <- new("itemsets", items=as(unclass(patterns), "itemMatrix"), quality=data.frame(support=attr(patterns, "counts")))
}


print.DST_Patterns = function(x, ...) {
  cat("Set of Patterns", "\n")
  cat("Class:", paste(class(x), collapse=", "), "\n") 
}
