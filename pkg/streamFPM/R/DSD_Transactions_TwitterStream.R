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
DSD_Transactions_TwitterStream <- function(consumer_key, consumer_secret, RegisteredOAuthCredentials = NULL, search_term = "", timeout = 10, language = "en",
                                           parser = function(text) unique(strsplit(gsub("[^[:alnum:][:space:]#]", "", text), " ")[[1]])  ) {
  
  if (!is.null(RegisteredOAuthCredentials)) {
    cred <- RegisteredOAuthCredentials
  }
  else {
    cred <- OAuthFactory$new(consumerKey=consumer_key,
                             consumerSecret=consumer_secret,
                             requestURL='https://api.twitter.com/oauth/request_token',
                             accessURL='https://api.twitter.com/oauth/access_token',
                             authURL='https://api.twitter.com/oauth/authorize')
    
    cred$handshake()
  }
  
  state <- new.env()
  assign("position", 1L, envir = state)
  assign("numberOfTweets", 0, envir = state)
  assign("tweets", list(), envir = state)
  
  #FIXME for windows need .pem file
  
  #example for manual creation of transaction object
  
  # creating the DSD object
  l <- list(description = "Twitter Transaction Data Stream",
            cred=cred,
            searchTerm = search_term,
            lang = language,
            tweets = NULL,
            timeout = timeout,
            state = state,
            parser = parser)
  
  class(l) <- c("DSD_Transactions_TwitterStream", "DSD_Transactions", "DSD_List", "DSD_R","DSD")
  
  l
  
}


#n = number of transactions
#x = DSD object
get_points.DSD_Transactions_TwitterStream <- function(x, n=1, assignment = FALSE, blocking = TRUE, ...) {
  ### gaussians at (3,2.5) and (3,-2.5)
  ### bars at (-3,2.8) and (-3,-2.8)
  
  
  #FIXME: add in error checking
  if(x$state$position >= x$state$numberOfTweets) {
    if(x$searchTerm == "") {
      tweetsSample <- sampleStream( file.name="", timeout=x$timeout, oauth=x$cred)
      tweetsSample.df <- parseTweets(tweetsSample)
      if(x$lang == "") {
        x$state$tweets <-  tweetsSample.df[, c("text")]
      }
      else {
        x$state$tweets <-  tweetsSample.df[tweetsSample.df$lang == x$lang, c("text")]
      }
      
    }
    else {
      if(x$lang == "") {
        tweetsSample <- filterStream( file.name="", timeout=x$timeout, oauth=x$cred, track = x$searchTerm)
        tweetsSample.df <- parseTweets(tweetsSample)
        x$state$tweets <-  tweetsSample.df[, c("text")]
      }
      else {
        tweetsSample <- filterStream( file.name="", timeout=x$timeout, oauth=x$cred, track = x$searchTerm, language = x$lang)
        tweetsSample.df <- parseTweets(tweetsSample)
        x$state$tweets <-  tweetsSample.df[, c("text")]
      }
    }
    x$state$numberOfTweets <- length(x$state$tweets)
    x$state$position <- 1L
  }
  
  a <- vector("list", n)
  for (i in 1:n) {
    if(x$state$position <= x$state$numberOfTweets) {
      #FIXME: suddenly this stopped working? $text on the end specifically
      #text_without_puncuation <- gsub("[^[:alnum:][:space:]#]", "", x$state$tweets[[x$state$position]]$text)  
      
      
      #text_without_puncuation <- gsub("[^[:alnum:][:space:]#]", "", x$state$tweets[[x$state$position]])  
      #a[[i]] <- unique(strsplit(text_without_puncuation, " ")[[1]])
      
      
      a[[i]] <- x$parser(x$state$tweets[[x$state$position]])
      x$state$position <- x$state$position + 1
    }
    else {
      a[[i]] <- NULL
      warning("No more tweets avaliable in stream")
    }
    
  }
  
  return(a)
  
}

writeStream.DST_Transactiosn_TwitterStream <- function(x, n=1, append = TRUE) {
  tweets <- get_points.DSD_Transactions_TwitterStream(x, n)
  save(tweets, file = "TwitterStream.RData")
}




#FIXME print function
