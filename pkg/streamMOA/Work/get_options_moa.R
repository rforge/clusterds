library(stream)
library(rJava)

clusterer <- .jnew("moa/clusterers/denstream/DenStream")
options <- .jcall(clusterer, "Lmoa/options/Options;", "getOptions")


s <- .jcall(options, "Ljava/lang/String;", "getHelpString")
cat(s)
