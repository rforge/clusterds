#class of type DSD_Transactions

#setup_twitter_oauth(consumer_key = consumer_key, consumer_secret = consumer_secret)

#setSize is the number of items to pull from,
#maxTransactionSize is the largest transaction you can have
#prob = probability for each item, other options include rexp(setSize)
#size = size for each individual transaction
DSD_Transactions_Twitter <- function(consumer_key = NULL, consumer_secret = NULL, access_token = NULL, access_secret =NULL, search_term,
                                     desired_count, since = NULL, until = NULL, sinceID = NULL, maxID = NULL,
                                     language = NULL, geocode = NULL, resultType = NULL, strip_retweets = FALSE,
                                     parser = function(text) unique(strsplit(gsub("[^[:alnum:][:space:]#]", "", text), " ")[[1]])  ) {

  if(!is.null(consumer_key) && !is.null(consumer_secret)){
    setup_twitter_oauth(consumer_key = consumer_key, consumer_secret = consumer_secret,
                        access_token = access_token, access_secret = access_secret)
  }
  else{
    warning("consumer_key or consumer_secret parameters not set: Assuming setup_twitter_oauth() has already been called")
  }

  
  if(is.null(since) && !is.null(until)){
    stop("Must have a specify a 'since' value also")
  }
  
  state <- new.env()
  assign("position", 0L, envir = state)
  assign("numberOfTweets", 0, envir = state)
  assign("tweets", list(), envir = state)
  assign("since", since, envir = state)
  assign("until", until, envir = state)
  assign("lang", language, envir = state)
  assign("highestId", sinceID, envir = state)
  assign("sinceID", sinceID, envir = state)
  assign("maxID", maxID, envir = state)
  assign("geocode", geocode, envir = state)
  assign("resultType", resultType, envir = state)
  assign("no_retweets", strip_retweets, envir = state)
  
  #FIXME for windows need .pem file
  
  #example for manual creation of transaction object
  
  
  # creating the DSD object
  l <- list(description = "Twitter Transaction Data Stream",
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
  
  if(x$state$position < 1L) {
    
    if(!is.null(x$state$since)) {
      x$state$tweets <- searchTwitter(x$searchTerm, n=x$desiredCount, since = x$state$since, until = x$state$until,
                                      lang = x$state$lang, sinceID = x$state$sinceID, maxID = x$state$maxID,
                                      geocode = x$state$geocode, resultType = x$state$resultType)
    }
    else {
      x$state$tweets <- searchTwitter(x$searchTerm, n=x$desiredCount, lang = x$state$lang)
    }
    
    if (x$state$no_retweets) {
      x$state$tweets <- strip_retweets(x$state$tweets, strip_manual = TRUE, strip_mt = TRUE)
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
