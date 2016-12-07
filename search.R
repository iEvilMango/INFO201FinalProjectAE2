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

# Helper function to replace spaces with '%20'
SwapSpaces <- function(str, rep="%20"){
  return(gsub(" ",rep,str))
}

# Method that sends the actual API request
# Has the api root and api key hard-coded in
# type: the formatted type string for the request (e.g. 'track.search?')
# request: the formatted request (e.g. &q_track=Sultans%20of%20Swing)
SendAPIRequest <- function(type, request, num.results){
  api.root = 'http://api.musixmatch.com/ws/1.1/'
  api.key = 'apikey=6e94e896d4f21b051076e5bb8679c724'
  page.size = paste0('&page_size=',num.results)
  has.lyrics="&f_has_lyrics=1"
  final.request <- paste0(api.root,type,api.key,request,page.size,has.lyrics)
  print(final.request)
  return(fromJSON(final.request))
}

# USE THIS METHOD
# query.type: the type of query that is being done (e.g. 'track', 'artist).
# song.name: the name of the song (blank if there is no name).
# artist.name: the name of the artist (blank if there is no artist).
# num.results: the number of results to return. The default (if left blank) is 10.
APIRequest <- function(query.type, song.name, artist.name, num.results=1){
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

# Internal method for reducing redundancy.
# Checks if the given name is an empty string. If it is not empty, formats it and appends the given add to it.
# If name is equal to "", returns ""
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
  base = 'http://api.chartlyrics.com/apiv1.asmx/'
  search.lyric = paste0(base,'SearchLyric?')
  song = LyricCheckHelper(song.name,"song=")
  artist.name = song.artist
  if(song.artist == ""){
    artist.name <- GetSongData(song.name)$artist_name
  }
  artist = LyricCheckHelper(artist.name, "artist=")
  search <- paste0(search.lyric,artist,"&",song)
  print(search)
  data <- xmlToList(xmlTreeParse(search))
  lyric.id = data$SearchLyricResult$LyricId
  checksum  = data$SearchLyricResult$LyricChecksum
  search.words = paste0(base,"GetLyric?")
  search.id = LyricCheckHelper(lyric.id, "lyricId=")
  search.sum = LyricCheckHelper(checksum, "lyricCheckSum=")
  full.search = paste0(search.words,search.id,"&",search.sum)
  print(full.search)
  full.lyrics = xmlToList(xmlTreeParse(full.search))
  return(full.lyrics$Lyric)
}

LyricHelperInternal <- function(song.name){
  base = 'http://api.chartlyrics.com/apiv1.asmx/'
  search.lyric = paste0(base,'SearchLyric?')
  song = LyricCheckHelper(song.name,"song=")
  artist.name <- GetSongData(song.name)$artist_name
  artist = LyricCheckHelper(artist.name, "artist=")
  search <- paste0(search.lyric,artist,"&",song)
  print(search)
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
  song.search <- LyricCheckHelper(song.name, "&q_track=")
  artist.search <- LyricCheckHelper(artist.name, "&q_artist=")
  full.search <- paste0(song.search, artist.search)
  data <- SendAPIRequest("track.search?", full.search, 1)
  filtered.data <- data$message$body$track_list$track
  return(filtered.data)
}

# Internal method for getting lyric data
# Paramter:
#   track.id: the musixmatch id of the track
# Returns:
#   Character array of the lyrics
GetLyricsData <- function(track.id){
  lyrics.search <- paste0("&track_id=",track.id)
  lyric.data <- SendAPIRequest("track.lyrics.get?",lyrics.search,1)
  return(lyric.data$message$body$lyrics$lyrics_body)
}

# Gets the url for a youtube video from the keyword given
# Parameter:
#   video.search: the search query
# Return:
#   The id of the video on youtube
GetYouTubeVideoID <- function(video.search){
  api.key = 'AIzaSyC5B_muf0KvmTYaGKAsOm0VHQ-VpTGIZik'
  base.url = 'https://www.googleapis.com/youtube/v3'
  formatted.video.search = SwapSpaces(video.search)
  search = paste0(base.url,"/search?part=snippet&q=",formatted.video.search,"&type=video&key=",api.key)
  data <- fromJSON(search)
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
