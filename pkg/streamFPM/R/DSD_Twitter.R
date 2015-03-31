#data stream generator

#want stream data
#each point will be integers or strings
#start with integers

#class of type DSD_Transactions

# A new DSD class (e.g., myDSD) needs the following:
## 1. a constructor function. myDSD <- function(PARAMETERS) which
## returns an object with the class  c("DSD_myDSD","DSD_R","DSD")
## 2. get_points.myDSD <- function(x, n=1, ...)


#setSize is the number of items to pull from,
#maxTransactionSize is the largest transaction you can have
#prob = probability for each item, other options include rexp(setSize)
#size = size for each individual transaction
DSD_Transactions_Twitter <- function(consumer_key, consumer_secret, RegisteredOAuthCredentials = NULL, search_term,
                                     desired_count, since = NULL, until = NULL, sinceID = NULL,
                                     parser = function(text) strsplit(gsub("[^[:alnum:][:space:]#]", "", text), " ")[[1]]) {
  cred <- NULL
  if (!is.null(RegisteredOAuthCredentials)) {
    cred <- RegisteredOAuthCredentials
    registerTwitterOAuth(cred)
  }
  else {
    cred <- OAuthFactory$new(consumerKey=consumer_key,
                             consumerSecret=consumer_secret,
                             requestURL='https://api.twitter.com/oauth/request_token',
                             accessURL='https://api.twitter.com/oauth/access_token',
                             authURL='https://api.twitter.com/oauth/authorize')
    
    cred$handshake()
    registerTwitterOAuth(cred)
  }
  
  if(is.null(since) && !is.null(until)){
    stop("Must have a specify a 'since' value also")
  }
  
  state <- new.env()
  assign("position", 1L, envir = state)
  assign("numberOfTweets", 0, envir = state)
  assign("tweets", list(), envir = state)
  assign("since", since, envir = state)
  assign("until", until, envir = state)
  assign("highestId", sinceID, envir = state)
  
  #FIXME for windows need .pem file
  
  #example for manual creation of transaction object
  
  
  # creating the DSD object
  l <- list(description = "Twitter Transaction Data Stream",
            cred=cred,
            searchTerm = search_term,
            tweets = NULL,
            desiredCount = desired_count,
            state = state,
            parser = parser)
  
  class(l) <- c("DSD_Transactions_Twitter", "DSD_Transactions", "DSD_List", "DSD_R","DSD")
  
  l
  
}


#n = number of transactions
#x = DSD object
get_points.DSD_Transactions_Twitter <- function(x, n=1, assignment = FALSE,...) {
  ### gaussians at (3,2.5) and (3,-2.5)
  ### bars at (-3,2.8) and (-3,-2.8)
  
  if(x$state$position >= x$state$numberOfTweets) {
    
    if(!is.null(x$state$since)) {
      x$state$tweets <- searchTwitter(x$searchTerm, n=x$desiredCount, since = x$state$since, until = x$state$until)
    }
    else {
      x$state$tweets <- searchTwitter(x$searchTerm, n=x$desiredCount)
    }
    
    x$state$numberOfTweets <- length(x$state$tweets)
    x$state$highestId <-  x$state$tweets[[1]]$id
    #changed because because last tweet in list is first by timestamp
    x$state$position <- length(x$state$tweets)
    if(x$state$numberOfTweets == 0) {
      stop('No tweets found for these search parameters.')
    }
  }
  
  
  a <- vector("list", n)
  for (i in 1:n) {
    
    if(x$state$position >= 1L) {
      
      #text_without_puncuation <- gsub("[^[:alnum:][:space:]#]", "", x$state$tweets[[x$state$position]]$text)
      
      #a[[i]] <- strsplit(text_without_puncuation, " ")[[1]]
      a[[i]] <- x$parser(x$state$tweets[[x$state$position]]$text)
      
      x$state$position <- x$state$position - 1L
    }
    else {
      a[[i]] <- NULL
      warning("No more tweets avaliable in stream")
    }

  }
  
  return(a)
  
}


#FIXME print function
