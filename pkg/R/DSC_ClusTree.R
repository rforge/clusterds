# ClusTree (anytime clustering) options:
# IntOption("horizon", 'h', "Range of the window.", 1000)
# IntOption("maxHeight", 'H', "The maximal height of the tree", 8)

#Reclustering: suggests EM or k-means

DSC_ClusTree <- function(horizon=1000, maxHeight=8, negLambda=NULL) {

  # error checking
  if (maxHeight < 0) {
    stop("invalid maxHeight")
  }

  if (horizon < 0) {
    stop("invalid horizon")
  }

  paramList <- list(h=horizon,
                    H=maxHeight)

  # converting the param list to a cli string to use in java
  cliParams <- convert_params(paramList)
  
  # initializing the clusterer
  clusterer <- .jnew("moa/clusterers/clustree/ClusTree")
  options <- .jcall(clusterer, "Lmoa/options/Options;", "getOptions")
  .jcall(options, "V", "setViaCLIString", cliParams)
  .jcall(clusterer, "V", "prepareForUse")

  if(!is.null(negLambda))
  	.jfield(clusterer,"negLambda")<-negLambda

  # initializing the R object
  l <- list(description = "ClusTree",
            options = cliParams,
            javaObj = clusterer)

  class(l) <- c("DSC_ClusTree","DSC_MOA","DSC_Micro","DSC")
  l  
}
