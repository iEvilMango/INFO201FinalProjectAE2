library(shiny)

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
      HTML(getYoutube(input$youtube))
    })
    
    output$lyrics <- renderUI({
      lyrics = c("Is she worth it, whatever this is",
              "It doesn’t feel right.",
              "Better think about your kids.",
              "I’m not putting up a fight.",
              "'Cause we could make love tonight,",
              "But you’re gonna hate yourself in the morning light,",
              "So just stop, breathe, count to three,",
              "Get your head right, right.",
              "I put the I in lie",
              "'Cause I’m a cheat, cheat, cheat"
              )
      # Replace lyrics above ^ with a vector containing the actual
      # song lyrics when possible. 
      block <- tags$blockquote()
      for (i in lyrics) {
        block <- tagAppendChild(block, i)
        block <- tagAppendChild(block, tags$br())
      }
      return(block)
    })
  } ## End of server function
)