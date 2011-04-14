# denstream options:
# -e epsilon 	0.01 (defines the epsilon neighborhood, range: 0 to 1)
# -p minPoints 	10 (min. num. points a cluster must have)
# -l lambda	0.006 (range: 0 to 1)
# -b beta	0.001 (range: 0 to 1)
# -m mu		1 (range: 0 to max(double))
# -i initPoints	10000 (number of points to use for initialization)
# -M		false (evaluate micro clustering flag)
DSC_DenStream <- function(epsilon=0.1, minPoints=10, lambda=0.006, beta=0.001, mu=1, initPoints=1000) {
  if (epsilon <= 0 || epsilon >= 1)
    stop("invalid epsilon, range: 0 to 1 exclusive")

  if (minPoints < 10)
    stop("invalid minPoints, must be > 0")

  if (lambda <= 0 || lambda >= 1)
    stop("invalid lambda, range: 0 to 1 exclusive")

  if (beta <= 0 || beta >= 1)
    stop("invalid beta, range: 0 to 1 exclusive")

  if (mu <= 0)
    stop("invalid mu, must be > 0")

  if (initPoints < 0)
    stop("invalid initPoints, must be > 0")

  paramList <- list(e = epsilon,
                    p = minPoints,
                    l = lambda,
                    b = beta,
                    m = mu,
                    i = initPoints)

  # converting the param list to a cli string to use in java
  cliParams <- convert_params(paramList)

  # initializing the clusterer
  clusterer <- .jnew("moa/clusterers/denstream/DenStream")
  options <- .jcall(clusterer, "Lmoa/options/Options;", "getOptions")
  .jcall(options, "V", "setViaCLIString", cliParams)
  .jcall(clusterer, "V", "prepareForUse")

  # initializing the R object
  l <- list(description = "DenStream",
            options = cliParams,
            javaObj = clusterer)

  class(l) <- c("DSC_DenStream","DSC_MOA","DSC")
  l
}
