
get_patterns <- function(dst, ...) {
  UseMethod("get_patterns")
  get_patterns.default <- function(dst, ...){
    stop(gettextf("get_patterns not implemented for classs '%s'.", paste(class(dst), collapse=", ")))
  }
  
}


print.DST_Pattern = function(x, ...) {
  cat(paste(x$description), '\n')
  cat("Class:", paste(class(x), collapse=", "), "\n") 
}