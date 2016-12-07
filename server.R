library(shiny)
library(shinythemes)
source("search.R")

# Given a youtube ID, returns a html tag representing either a youtube
# video with the given ID, or paragraph tag saying the ID cannot be correlated
# to a video
getYoutube <- function(youtubeID) {
  if(!is.null(youtubeID)) {
    return(tags$iframe(width="500",
                       height="250",
                       src = paste0('//www.youtube.com/embed/', youtubeID),
                       frameborder="0"
                      )
          )
  } else {
    return(tags$p("No video can be associated with the given title"))
  }
}

titles <- c("Alphabet Aerobics", # handling genre not found.
            "Yellow",
            "The Sounds of Silence",
            "Mr Brightside",
            "Wheels on the Bus", # handling lyric not found
            "Sultans of Swing",
            "Grand Theft Autumn",
            "The I In Lie", # handling lyric not found
            "Alphabet Aerobics",  # handling all the issues, nothing found.
            "The Scientist" # Handling date not found for metadata
            ) 
artists <- c("Blackalicious",
             "ColdPlay",
             "Simon & Garfunkel",
             "The Killers",
             "Raffi",
             "Dire Straits",
             "Fall Out Boy",
             "Patrick Stump",
             "Dire Straits",
             "ColdPlay"
             )
suggest <- data.frame(titles, artists)

shinyServer(
  function(input, output, session) {
    
    # returns a tag containing either a youtube video
    # associated with the given title and artist, or
    # a paragraph tag saying that won't work.
    output$youtube <- renderUI({
      return(
        getYoutube(
          GetYouTubeVideoID(
            paste0(
              input$title,
              input$artist
            )
          )
        )
      )
    })
    
    # The way this functions isn't ideal; it forces you to press
    # submit, as submit buttons make all other input wait until it's pressed 
    # see: 
    # https://groups.google.com/forum/#!topic/shiny-discuss/NQHvTCW2t2A/discussion
    observe({
      # Whenever the input is the randomize button being pressed,
      # randomly select a song from the suggest data frame,
      # and change the values of the two inputs to `show` 
      # song when you press submit twice.
      if(input$randomize) {
        choice <- suggest[sample(nrow(suggest), 1), ]
        updateTextInput(session,
                        inputId = "title", 
                        value = as.character(choice$titles))
        
        updateTextInput(session,
                        inputId = "artist", 
                        value = as.character(choice$artists))
      }
    })
      
      
    # returns tag representing `meta data` section for the current song: 
    # If data cannot be found, shows that.
    # Otherwise, shows information on
    # Release date
    # Artist
    # Genre
    # Popularity rating (taken from musixmatch)
    output$meta <- renderUI({
      output <- tags$div()
      
      # Get data from resources on the given song if possible;
      # if not, handle the error.
      data <- GetSongData(input$title, input$artist)
      if(typeof(data) == "NULL") { 
        return(tagAppendChild(output,
                              tags$p("Song meta data not found")))
      }
      data <- GetParsedData(data)
      tryCatch({
        # Get data regarding when the song was first released.
        first.release <- as.Date(data$first_release_date) %>%
                              format(format="%B %d, %Y")
        
        date <- tags$p(
                  paste0("Released: ", first.release)
                )
      
        # Add date to the output
        output <- tagAppendChild(output, date)
      }, error = function(e) {} ## In that case, just ignore the date.
      )
      # Add information regarding the artist, the genre, and the popularity
      # of the song
      output <- output %>% 
                  tagAppendChild(tags$p(
                    paste("Artist:", data$artist_name))
                  ) %>%
                  tagAppendChild(tags$p(
                    paste("Genre:", data$genre))
                  ) %>%
                  tagAppendChild(tags$p(
                    paste("Popularity rating (out of 100):", 
                          data$track_rating))
                  )
      return(output)
    })
    
    # Attempts to generate lyrics for the current song in
    # a single blockquote
    # if the current song's lyrics cannot be found, returns
    # the blockquote with only a paragraph explaining that inside of it.
    # otherwise, the blockquote will contain the lyrics to the song,
    # split by line and visually broken apart by br tags
    output$lyrics <- renderUI({
      block <- tags$blockquote()
      lyrics <- NULL
      
      # try to get lyrics if possible...
      tryCatch(
        {
          lyrics <- GetLyrics(input$title, input$artist) %>%
                      strsplit("\n")
        },  
        error = function(cond)
        {
          # returning here is useless as it' just breaks the tryCatch
          lyrics <- NULL
        }
      )
      
      # error check; if no lyrics have been found, handle that.
      if(is.null(lyrics)) {
        return(tagAppendChild(block, tags$p("Lyrics not found")))
      }
      
      # For every line in lyrics
      for (i in lyrics[[1]]) {
        # add it to the output, along with a br tag to skip to the next line.
        block <- block %>%
                  tagAppendChild(i) %>%
                  tagAppendChild(tags$br())
      }
      block <- block %>% 
                tagAppendChild(tags$br()) %>%
                tagAppendChild(tags$br()) %>%
                tagAppendChild(
                  tags$p("Courtesy of ChartLyrics -- if incorrect, pass that on to us on our github!"))
      
      return(block)
    })
  } ## End of server function
)