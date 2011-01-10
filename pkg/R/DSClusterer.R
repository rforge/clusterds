# DSClusterer - DataStreamClusterer interface
# all DSC classes have these methods
# and an additional function to create the DSC

convertParams <- function(paramList=list()) {
  length <- length(paramList)
  if (length == 0)
    stop("invalid param list")

  cliParams <- ""

  for (i in 1:length) {
    cliParams <- paste(cliParams, "-", names(paramList[i]), " ", paramList[[i]], " ", sep="")
  }

  # removing the trailing space
  cliParams <- substr(cliParams, 1, nchar(cliParams)-1)
}
