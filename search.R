library(jsonlite)
library(dplyr)
require(XML)

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

# Helper function to replace spaces with a character
# Parameter:
#   rep: the string to replace a space with. By default is set to "%20"
# Returns:
#   String that has all the spaces replaced by the given 'rep' value
SwapSpaces <- function(str, rep="%20"){
  return(gsub(" ",rep,str))
}

# Method that sends the actual API request
# Has the api root and api key hard-coded in
# Parameters:
#   type: the formatted type string for the request (e.g. 'track.search?')
#   request: the formatted request (e.g. &q_track=Sultans%20of%20Swing)
#   num.results: the number of results to return
# Returns:
#   JSON object from musixmatch with song data
SendAPIRequest <- function(type, request, num.results){
  api.root = 'http://api.musixmatch.com/ws/1.1/'
  api.key = 'apikey=6e94e896d4f21b051076e5bb8679c724'
  page.size = paste0('&page_size=',num.results)
  has.lyrics="&f_has_lyrics=1"
  # Add the url parts together
  final.request <- paste0(api.root,type,api.key,request,page.size,has.lyrics)
  return(fromJSON(final.request))
}

# Makes an API request based on the parameters to musixmatch
# Paramters:
#   query.type: the type of query that is being done (e.g. 'track', 'artist).
#   song.name: the name of the song (blank if there is no name).
#   artist.name: the name of the artist (blank if there is no artist).
#   num.results: the number of results to return. The default (if left blank) is 1.
# Returns:
#   JSON object from musixmatch with song data
APIRequest <- function(query.type, song.name, artist.name, num.results=1){
  # Pull any extra white space off of the song and artist entries
  song.name = trimws(song.name)
  artist.name = trimws(artist.name)
  
  if(query.type == 'track'){
    # If the query.type is 'track', both the song name and artist name are valid parts of the search
    search = paste(song.name,artist.name)
  }
  if(query.type == 'artist'){
    # If the query is for an artist, only the artist name is a valid search part
    search = paste(artist.name)
  }
  
  # Combine to form the parts of the url to pass to SendAPIRequest, and replace spaces with '%20'
  type = paste0(query.type,'.search?')
  search.type = paste0('&q_',query.type,'=')
  query = paste0(search.type, SwapSpaces(search))
  return(SendAPIRequest(type, query, num.results))
}

# Internal method for reducing redundancy.
# Checks if the given name is an empty string. If it is not empty, formats it and appends the given add to it.
# If name is equal to "", returns ""
# Paramters:
#   name: the string to check
#   add: the string to add to name, if name is not equal to ""
#   rep: the string to replaces spaces with (optional). Set to '%20' by default
# Returns:
#   The formatted name, or "" if name equals ""
LyricCheckHelper <- function(name, add, rep="%20"){
  name.part = ""
  if(name != ""){
    formatted = SwapSpaces(name, rep)
    name.part <- paste0(add,formatted)
  }
  return(name.part)
}

# Gets the lyrics for the specified song.
# If an artist name is given, will use the song by the artist
# Otherwise, uses first hit
# Parameters:
#   song.name: The name of the song. (e.g. "Sultans Of Swing")
#   artist.name (optional): The name of the artist. (e.g. "Dire Straits"). If no artist is given, will use the top hit.
# Returns:
#   Character array of the lyrics, with escape characters (/n, etc.) included. Also has the necessary copyright information at the end.
GetLyrics <- function(song.name, song.artist){
  
  # The base for the chartlyrics API
  base = 'http://api.chartlyrics.com/apiv1.asmx/'
  search.lyric = paste0(base,'SearchLyric?')
  
  # Format the song name to be added to search.lyric
  song = LyricCheckHelper(song.name,"song=")
  
  # If no artist is given, don't put anything about the artist in the request url
  artist.name = song.artist
  if(song.artist == ""){
    artist.name <- GetSongData(song.name)$artist_name
  }
  # Format the artist name to be added to search.lyric
  artist = LyricCheckHelper(artist.name, "artist=")
  
  # Combine all the parts of the API request url
  search <- paste0(search.lyric,artist,"&",song)
  
  # Get the xml data from chartlyrics, and convert it to a list()
  data <- xmlToList(xmlTreeParse(search))
  
  # Get the lyric.id, so we can actually get the lyric text from chartlyrics
  lyric.id = data$SearchLyricResult$LyricId
  # Get the checksum, which is needed to get the lyric text
  checksum  = data$SearchLyricResult$LyricChecksum
  
  # Create API request to get the actual lyric text
  search.words = paste0(base,"GetLyric?")
  # Get formatted id and checksum for the url
  search.id = LyricCheckHelper(lyric.id, "lyricId=")
  search.sum = LyricCheckHelper(checksum, "lyricCheckSum=")
  # Combine all the parts of the API request url
  full.search = paste0(search.words,search.id,"&",search.sum)
  # Get the full lyrics text data from charltlyrics and convert it to a list
  full.lyrics = xmlToList(xmlTreeParse(full.search))
  
  # Only return the lyrics. The other data is gotten from musixmatch
  return(full.lyrics$Lyric)
}

# Gets the actual string of the full lyrics for the song.
# Musixmatch only has 30% of the lyrics of each song availble through its api
# Uses chartlyrics to get 100% if the lyrics
# Takes in a song name, and gets the artist from GetSongData(song.name)$artist_name
# Parameters:
#   song.name: The name of the song.
# Returns:
#   list of String data (lyrics)
LyricHelperInternal <- function(song.name){
  
  # Set up the base for the API endpoint
  base = 'http://api.chartlyrics.com/apiv1.asmx/'
  search.lyric = paste0(base,'SearchLyric?')
  
  # Add the song name and artist name to the url
  song = LyricCheckHelper(song.name,"song=")
  # Get the right artist name from the song data
  artist.name <- GetSongData(song.name)$artist_name
  artist = LyricCheckHelper(artist.name, "artist=")
  
  # Add the song and artist parts to the url
  search <- paste0(search.lyric,artist,"&",song)
  
  # Make request, and turn xml response into a list()
  data <- xmlToList(xmlTreeParse(search))
  return(data)
}

# Internal method for getting song data
# Paramter:
#   song.name: The name of the song. (e.g. "Sultans Of Swing")
#   artist.name (optional): The name of the artist. (e.g. "Dire Straits"). If no artist is given, will use the top hit.
# Returns:
#   List of information. Useful values:
#     $track_length: Gives the length of the track (in seconds)
#     $album_name: Gives the album name of the track
#     $first_release_date: Gives the first release data (year-month-dayThour:minute:second)
#     $primary_genres: Gives the genres of the music, in the form of a list.
#         
GetSongData <- function(song.name, artist.name=""){
  
  # Format the song and artist strings to be put in the API request url
  song.search <- LyricCheckHelper(song.name, "&q_track=")
  artist.search <- LyricCheckHelper(artist.name, "&q_artist=")
  
  # Combine the formatted song and artist terms
  full.search <- paste0(song.search, artist.search)
  
  # Send the API request
  data <- SendAPIRequest("track.search?", full.search, 1)
  
  # Filter out the top few layers from the response that are not needed
  filtered.data <- data$message$body$track_list$track
  return(filtered.data)
}

# Gets the url for a youtube video from the keyword given
# Parameter:
#   video.search: the search query
# Return:
#   The id of the video on youtube
GetYouTubeVideoID <- function(video.search){
  
  # YouTube api key
  api.key = 'AIzaSyC5B_muf0KvmTYaGKAsOm0VHQ-VpTGIZik'
  # API endpoint for YouTube api
  base.url = 'https://www.googleapis.com/youtube/v3'
  
  # Replace spaces in video.search with '%20'
  formatted.video.search = SwapSpaces(video.search)
  
  # Create API request url
  search = paste0(base.url,"/search?part=snippet&q=",formatted.video.search,"&type=video&key=",api.key)
  
  # Make API request to get video data
  data <- fromJSON(search)
  
  # Get the id of the first video. This will be passed to an HTML <iframe> object for embedding.
  video.id <- data$items$id$videoId[[1]]
  
  return(video.id)
}

# Exampole url:
# https://www.youtube.com/watch?v=G2tWhjEbfSQ
# base = https://www.youtube.com/watch?
# v= (Video equals)
# G2tWhjEbfSQ video id


# Takes a data frame created by the GetSongData function and returns a dataframe with
# Released Date / Artist / Album Image URL / Popularity (from 0 - 100 in musixmatch) / Genre 
GetParsedData <- function(filtered.data){
  parsed.data <- filtered.data %>% 
                  select(first_release_date,
                         artist_name,
                         album_coverart_100x100,
                         track_rating
                        ) 
  genre = as.data.frame(filtered.data$primary_genres$music_genre_list)$music_genre$music_genre_name[1]
  if(typeof(genre) != "character" || genre == "") {
    genre = "not found"
  }
  parsed.data <- parsed.data %>%
                  mutate("genre" = genre)
  
  #%>%
  #                mutate("genre" = tryCatch(
  #                        {
  #                          return(as.data.frame(filtered.data$primary_genres$music_genre_list)
  #                                                $music_genre$music_genre_name[1])
  #                        },
  #                        error = function(e){
  #                          return("Not found")
  #                        })
   #                     )
  return(parsed.data)
}
