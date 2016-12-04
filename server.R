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
      lyrics = "Is she worth it, whatever this is?
              It doesn’t feel right.
      Better think about your kids.
      I’m not putting up a fight.
      'Cause we could make love tonight,
      But you’re gonna hate yourself in the morning light,
      So just stop, breathe, count to three,
      Get your head right, right.
      I put the I in lie
      'Cause I’m a cheat, cheat, cheat
      I’m a cheat, cheat, cheat.
      And baby, bang bang, kiss kiss
      You and I got to put an end to this
      So we cheat, cheat, cheat
      I’m a cheat, cheat, cheat.
      If you’re unfaithful, put your hands in the air,
      Hands in the air, hands in the air,
      Like you’re under arrest with a guilty conscience
      Stick ‘em up if you’ve got a guilty conscience, yeah
      Yeah, yeah
      She married… "
      
      return()
    })
    
    
  } ## End of server function
)