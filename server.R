library(shiny)

getYoutube <- function(link) {
  if(link != "") {
    link = paste0('<iframe width="500" height="250" src="//', link,
           '" frameborder="0" allowfullscreen></iframe>')
  } else {
    link = "No Video passed"
  }
  return(link);
}

shinyServer(
  function(input, output) {
    output$youtube <- renderUI({
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
      block <- tags$blockquote()
      for (i in lyrics) {
        print(i)
        block <- tagAppendChild(block, i)
        block <- tagAppendChild(block, tags$br())
        print(block)
      }
      return(block)
    })
    
    
  } ## End of server function
)