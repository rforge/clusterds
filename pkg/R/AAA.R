### helper for doing things in blocks

.make_block <- function(n, block) {
    if(n<block) return(n)
    
    b <- rep(block, times=as.integer(n/block))
    if(n%%block) b<- c(b, n%%block)
    b
}
