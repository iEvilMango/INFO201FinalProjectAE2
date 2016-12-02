library(jsonlite)
library(dplyr)

# Example requests:
# data <- APIRequest("track", "Sultans of Swing", "Dire Straits")
#   Gets the top 10 results of the track "Sultan of Swing" by artist "Dire Straits"
#
# data <- APIRequest("artist", "", "Dire Straits", 50)
#   Gets the top 50 results of the  artist "Dire Straits"
#
# data <- APIRequest("track", "Sultans of Swing", "")
#   Gets the top 10 results of the  track "Sultans of Swing"
#
# 
# 
# 
#

# Helper function to replace spaces with '%20'
SwapSpaces <- function(str){
  return(gsub(" ","%20",str))
}

# Method that sends the actual API request
# Has the api root and api key hard-coded in
# type: the formatted type string for the request (e.g. 'track.search?')
# request: the formatted request (e.g. &q_track=Sultans%20of%20Swing)
SendAPIRequest <- function(type, request, num.results){
  api.root = 'http://api.musixmatch.com/ws/1.1/'
  api.key = 'apikey=6e94e896d4f21b051076e5bb8679c724'
  page.size = paste0('&page_size=',num.results)
  final.request <- paste0(api.root,type,api.key,request,page.size)
  print(final.request)
  return(fromJSON(final.request))
}

# USE THIS METHOD
# query.type: the type of query that is being done (e.g. 'track', 'artist).
# song.name: the name of the song (blank if there is no name).
# artist.name: the name of the artist (blank if there is no artist).
# num.results: the number of results to return. The default (if left blank) is 10.
APIRequest <- function(query.type, song.name, artist.name, num.results=10){
  song.name = trimws(song.name)
  artist.name = trimws(artist.name)
  if(query.type == 'track'){
    search = paste(song.name,artist.name)
  }
  if(query.type == 'artist'){
    search = paste(artist.name)
  }
  type = paste0(query.type,'.search?')
  search.type = paste0('&q_',query.type,'=')
  query = paste0(search.type, SwapSpaces(search))
  return(SendAPIRequest(type, query, num.results))
}








