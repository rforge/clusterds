\name{DSD_Transactions_TwitterStream}
\alias{DSD_Transactions_TwitterStream}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Twitter Stream transaction generator
}
\description{
A datastream generator that uses the streamR package to create transactions by sampling the twitter stream
}
\usage{
DSD_Transactions_TwitterStream <- function(consumer_key, consumer_secret,
                                  RegisteredOAuthCredentials = NULL, search_term = "", 
                                  timeout = 10, language = "en",
                                  parser = function(text) strsplit(gsub("[^[:alnum:][:space:]#]", "", text), " ")[[1]])
  
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{consumer_key}{
  The consumer key, provided by the twitter api
}
  \item{consumer_secret}{
  The consumer secret key, also provided by the twitter api
}
  \item{RegisteredOAuthCredentials}{
  Optional. The user can pass already registered twitter credentials using the \pkg{ROAuth} package. If the user does not pass this argument, it will attempt to register the twitter credentials using the consumer key and secret.
}
  \item{search_term}{
    If the user specifies a search term, it will return only tweets that contain that term.
  }
  \item{timeout}{
    How long (in seconds) the data stream generator will sample the twitter stream when it runs out of stored tweets.
  }
  \item{language}{
    When a language code is specified, all of which can be found in the twitter API documentation, it will only return tweets in that language. The default is 'en', or english.
  }
  \item{parser}{
    The user can specify their own function for parsing tweets into individual words. The function must accept a string and return a list of strings. The default function removes punctuation except hashtags and splits the string by spaces.
  }
}
\details{
DSD_Transactions_TwitterStream creates a dsd that generates transactions from tweets sampled from the Twitter stream in real time, meaning unlike DSD_Transactions_Twitter, it receives a sampling of tweets that are happening right when the get_points() is called, rather than an archive of tweets from the last several hours or days
}
\value{
Returns a \code{DSD_Transactions_TwitterStream} 
object (subclass of 
\code{DSD_Transactions}, \code{DSD}) which is a list of the defined
params. The params are either passed in from the function or
created internally
}
\references{
https://dev.twitter.com/rest/public/search
http://cran.r-project.org/web/packages/streamR/streamR.pdf
}
\author{
Derek Phanekham
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
#create Twitter stream generator
\dontrun{
key <- "************************"
secret <- "**********************"

dsd <- DSD_Transactions_TwitterStream(consumer_key = key, consumer_secret = secret, search_term="#stream", timeout=10)

#create with already registered credentials

library(ROAuth)

cred <- OAuthFactory$new(consumerKey=key,
                         consumerSecret=secret,
                         requestURL='https://api.twitter.com/oauth/request_token',
                         accessURL='https://api.twitter.com/oauth/access_token',
                         authURL='https://api.twitter.com/oauth/authorize')
cred$handshake()
                         
dsd <- DSD_Transactions_TwitterStream(consumer_key = key, consumer_secret = secret, 
  RegisteredOAuthCredentials = cred, search_term="#stream", timeout=10)
                         

#get a transaction from the datastream
transaction <- get_points(dsd)

transaction

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
