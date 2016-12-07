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
      # Genre 
      output <- tags$div()
      data <- GetParsedData(
        GetSongData(input$title, input$artist)
      )
      first.release <- data$first_release_date
      first.release <- format(as.Date(first.release), format="%B %d, %Y")
      
      date <- tags$p(paste0("Released: ", first.release))
      
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
      
      lyrics <- GetLyrics(input$title, input$artist)
      
      lyrics <- strsplit(lyrics, "\n")
      block <- tags$blockquote()
      
      for (i in lyrics[[1]]) {
        block <- tagAppendChild(block, i)
        block <- tagAppendChild(block, tags$br())
      }
      return(block)
    })
  } ## End of server function
)