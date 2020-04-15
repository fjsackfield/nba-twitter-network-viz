library(httr)
library(base64enc)


create_btoken <- function() {
  
  appname <- "Network_Viz_App"
  key <- "gHvYsYgHeB9HylcWZ7LNDO91n"
  secret <- "iklKrD5R37eawVBrWmQB5OC8uz3LYQBgv8VzCiJDjxXSMxMH1V"
  
  # base64 encoding
  kands <- paste(key, secret, sep=":")
  base64kands <- base64encode(charToRaw(kands))
  base64kandsb <- paste("Basic", base64kands, sep=" ")
  
  # request bearer token
  resToken <- POST(url = "https://api.twitter.com/oauth2/token",
                   add_headers("Authorization" = base64kandsb, "Content-Type" = "application/x-www-form-urlencoded;charset=UTF-8"),
                   body = "grant_type=client_credentials")
  
  # get bearer token
  bearer <- content(resToken)
  bearerToken <- bearer[["access_token"]]
  bearerTokenb <- paste("Bearer", bearerToken, sep=" ")
  
  return(bearerTokenb)
  
}


