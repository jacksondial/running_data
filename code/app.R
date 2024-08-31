# Runs App

RunningDataApp <- \(){
  setwd(here::here("code"))
  source("server.R")
  source("ui.R")
  
  shinyApp(ui = ui, server = server)
  
}

RunningDataApp()

