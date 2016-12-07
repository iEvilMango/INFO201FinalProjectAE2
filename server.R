library(shiny)
source("search.R")

getYoutube <- function(youtubeID) {
  if(youtubeID != "") {
    link = paste0('<iframe width="500" height="250" src="//www.youtube.com/embed/',
                  youtubeID,
                  '" frameborder="0" allowfullscreen></iframe>')
  } else {
    link = "No Video passed"
  }
  return(link);
}

shinyServer(
  function(input, output) {
    output$youtube <- renderUI({
      # Modify to get video id and pass that in.
      HTML(
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
    
    output$meta <- renderUI({
      output <- tags$div()
      
      data <- GetSongData(input$title, input$artist)
      if(typeof(data) == "NULL") { 
        return(tagAppendChild(output,
                              tags$p("Song meta data not found")))
      }
      data <- GetParsedData(data)
      
      first.release <- data$first_release_date
      first.release <- format(as.Date(first.release), format="%B %d, %Y")
      
      date <- tags$p(
                paste0("Released: ", first.release)
              )
      
      genre <- data$genre
      
      artist <- data$artist_name
      popularity <- data$track_rating
    
      output <- tagAppendChild(output, date)
      
      output <- tagAppendChild(output, 
                      tags$p(
                        paste("Artist:", artist)
                      )
                    )
      
      output <- tagAppendChild(output, 
                                tags$p(
                                  paste("Genre:", genre)
                                )
                             )
      
      output <- tagAppendChild(output, 
                                tags$p(
                                  paste("Popularity rating (out of 100):", popularity)
                                )
                              )
      return(output)
    })
    
    
    output$lyrics <- renderUI({
      block <- tags$blockquote()
      lyrics <- NULL;
      tryCatch({
        lyrics <- GetLyrics(input$title, input$artist) %>%
                      strsplit("\n")
      },  error = function(cond) {
        lyrics <- NULL;
      })
      if(is.null(lyrics)) {
        return(tagAppendChild(block, tags$p("Lyrics not found")))
      }
      for (i in lyrics[[1]]) {
        block <- block %>%
                  tagAppendChild(i) %>%
                  tagAppendChild(tags$br())
      }
      return(block)
    })
  } ## End of server function
)