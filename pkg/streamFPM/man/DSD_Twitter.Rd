\name{DSD_Transactions_Twitter}
\alias{DSD_Transactions_Twitter}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Twitter Data Stream Generator
}
\description{
A data stream generator that gathers and returns tweets using the Twitter REST API through the \pkg{twitteR} package. This API has access to an archive of tweets from the last several days.
}
\usage{
DSD_Transactions_Twitter(consumer_key = NULL, consumer_secret = NULL,
                         access_token = NULL, access_secret =NULL,
                         search_term, desired_count, since = NULL,
                         until = NULL, sinceID = NULL, maxID = NULL,
                         language = NULL, geocode = NULL, resultType = NULL,
                         strip_retweets = FALSE, 
                         parser = function(text) 
                            unique(tolower(strsplit(
                            gsub(
                            "[^[:alnum:][:space:]#]", "", text), " ")[[1]]))  )

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{consumer_key}{
  The consumer key, provided by the twitter api
}
  \item{consumer_secret}{
  The consumer secret key, provided by the twitter api
}
  \item{access_token}{
  The access token, provided by the twitter api
}
  \item{access_secret}{
  The access secret key, provided by the twitter api
}
  \item{search_term}{
    If the user specifies a search term, it will return only tweets that contain that term.
  }
  \item{desired_count}{
    The desired number of tweets to fetch whenever the stream is empty
  }
  \item{language}{
    A language code. When a language code is specified, all of which can be found in the twitter API documentation, it will only return tweets in that language.
  }
    \item{since}{
    A date in the format: '01-31-2015'. It will only retrieve tweets since this date
  }
    \item{until}{
    A date in the format: '01-31-2015'. Must be used in conjunction with \code{since}. It will only retrieve tweets up until this date.
  }
     \item{sinceID}{
    A character ID in the form '123456'. It will only retrieve tweets with IDs higher than this value.
  }
    \item{maxID}{
    The user can specify their own function for parsing tweets into individual words. The function must accept a string and return a list of strings. The default function removes punctuation except hashtags and splits the string by spaces.
  }
    \item{geocode}{
    The user can specify their own function for parsing tweets into individual words. The function must accept a string and return a list of strings. The default function removes punctuation except hashtags and splits the string by spaces.
  }
    \item{resultType}{
    The user can specify their own function for parsing tweets into individual words. The function must accept a string and return a list of strings. The default function removes punctuation except hashtags and splits the string by spaces.
  }
    \item{strip_retweets}{
    If this parameter is set to TRUE, only original tweets will be parsed. 
  }
  \item{parser}{
    The user can specify their own function for parsing tweets into individual words. The function must accept a string and return a list of strings. The default function removes punctuation except hashtags and splits the string by spaces.
  }
}
\details{
DSD_Transactions_TwitterStream creates a DSD that generates transactions from tweets retrieved from an archive of tweets from the past several days. To get current tweets in real time, use \code{DSD_Transactions_TwitterStream}.
}
\value{
Returns a \code{DSD_Transactions_Twitter} 
object (subclass of 
\code{DSD_Transactions}, \code{DSD}) which is a list of the defined
params. The params are either passed in from the function or
created internally
}
\references{
https://dev.twitter.com/rest/public/search
}
\author{
Derek Phanekham
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{DSD_Transactions}}
}
\examples{

\dontrun{

#create Twitter data stream generator using a key and secret from the Twitter API
consumer_key <- "************************"
consumer_secret <- "**********************"
access_token <- "************************"
access_secret <- "**********************"

twitterDSD <- DSD_Transactions_Twitter(consumer_key, consumer_secret,
  access_token, access_secret, search_term = "SMU", desired_count = 500,
  lang = "en", since = '2015-03-28', until = '2015-03-29')

#create with already registered credentials
library(twitteR)

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
  
twitterDSD <- DSD_Transactions_Twitter(search_term = "streamFPM", desired_count = 500,
lang = "en", since = '2015-03-28', until = '2015-03-29')

#get a transaction from the datastream
transaction <- get_points(twitterDSD)

transaction
}

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
