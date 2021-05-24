# set working directory
#setwd("c:/Users/Tao Chen/Dropbox/Z_Tao/T460s/Desktop/social_media_analytics_2020/week1/")
#install.packages("rtweet")
library(rtweet)

# How to retrieve tweets of a focal account
# You can get the latest 3200 tweets from each account
PPENEW<-get_timelines(c("3M","honeywell"),n=3200)
#Save the data
write_as_csv(PPENEW, "PPEmanufacturer1118onlinetest.csv")

# How to retrieve tweets which mentions a particular keyword 
# Regular twitter users can retrieve messages in the past 7-9 days.
# The quote is 18000 every 15 minutes. 
# It will use online authorization. 
# Please make sure you login into your twitter account. 
vaccine <- search_tweets(
  "vaccine", lang="en",n = 2000, include_rts = FALSE
  )
write_as_csv(vaccine, "vaccine1118onlinetest.csv")

# How to retrieve tweets which mentions a particular brand
TWAT3M <- search_tweets(
  "@3M OR #3M", lang="en",n = 2000
  )

# How to retrieve older tweets. Need to apply for developer account.
# Please see my instruction on how to apply. 
# After you get authorization, you will get four secret codes.
# Do not give the codes to others. 

# Set up api keys and access tokens
api_key = "rBZN8CGiW997pHDVlXMm1Y37d"
api_secret_key = "wep3r0JChiqFUvbQB3XUOgzeWnQSV3hQCVjdUkeUlVVp85hzQm"
access_token <- "1118549407715209217-tYeoVjrSAcfKqNdR7YqDSLEF7x796C"
access_token_secret <- "zzXIUovFXxscNevD8sfa4NhncSRiOmx16zsk2eRJhbOxm"

## authenticate
token <- create_token(
  app = "Smart Analytics",
  consumer_key = api_key,
  consumer_secret = api_secret_key,
  access_token = access_token,
  access_secret = access_token_secret)

# 30 days:
rt.30day <- search_30day("vaccine", n=100, 
                         env_name = "dev1",
                         fromDate = "202010200000", 
                         toDate = "202011010000",
                         token=token)

# Full archive:
rt.full <- search_fullarchive("vaccine", n = 100, 
                              env_name = "dev2",
                              fromDate = "202001010000", 
                              toDate = "202003280000",
                              token=token)

