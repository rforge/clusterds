### internal helper to figure out type

get_type <- function(x, type = c("auto", "micro", "macro")) {
    type <- match.arg(type)
    if(type=="auto") {
	if("DSC_Macro" %in% class(x)) type <- "macro"
	else type <- "micro"
    }
    type
}
