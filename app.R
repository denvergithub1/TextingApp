#Private Text App
#install.packages("shiny")
#install.packages("googlesheets4")
#install.packages("dplyr")
#install.packages("DT")
#install.packages("quarto")
#install.packages(c("shiny", "googlesheets4", "dplyr", "DT"))

library(shiny)
library(googlesheets4)
library(dplyr)
library(DT)
#library(quarto)

# Authentication.
gs4_auth(path = "...")  

# UI
ui <- fluidPage(
  titlePanel("Private Chat Room"),
  
  p(tags$b("📝 Sheet Format:"), "To save messages, must include columns: Contact | User | Text | Time"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("sheetURL", "Enter Google Sheet URL (optional):"),
      actionButton("loadSheet", "Load Sheet"),
      textInput("contactCode", "Enter Contact Code:"),
      textInput("userName", "Your Name (optional):"),
      textAreaInput("message", "Message:", placeholder = "Type your message here..."),
      actionButton("send", "Send Message"),
      actionButton("refresh", "Refresh Chat")
    ),
    
    mainPanel(
      DTOutput("chatTable"),
      tags$div(
        tags$style(HTML(".green-label { color: green; }")),
        HTML("<p class='green-label'>JSur Sponsors</p>")
      )
    )))


# SERVER
server <- function(input, output, session) {
  sheet_id <- reactiveVal(NULL)
  localMessages <- reactiveVal(data.frame(Contact = character(), User = character(), Text = character(), Time = character()))
  
  observeEvent(input$loadSheet, {
    req(input$sheetURL)
    id <- sub(".*?/d/([a-zA-Z0-9_-]+).*", "\\1", input$sheetURL)
    sheet_id(id)
    
    tryCatch({
      df <- read_sheet(ss = id, range = "TextingApp")
      localMessages(df)
      showNotification("Sheet loaded successfully!", type = "message")
    }, error = function(e) {
      showNotification("Failed to load sheet. Check URL & permissions.", type = "error")
    })
  })
  
  loadMessages <- function() {
    if (!is.null(sheet_id())) {
      tryCatch({
        df <- read_sheet(sheet_id())
        localMessages(df)
      }, error = function(e) {
        showNotification("Failed to load messages.", type = "error")
      })
    }
  }
  
  observeEvent(input$send, {
    req(input$contactCode, input$message)
    
    msg <- data.frame(
      Contact = input$contactCode,
      User = ifelse(input$userName == "", "Anonymous", input$userName),
      Text = input$message,
      Time = as.character(Sys.time())
    )
    
    # Add to Google Sheet if loaded properly
    if (!is.null(sheet_id())) {
      tryCatch({
        sheet_append(sheet_id(), msg)
        loadMessages()
      }, error = function(e) {
        showNotification("Failed to send to sheet. Message stored locally.", type = "warning")
        localMessages(rbind(localMessages(), msg))
      })
    } else {
      # Store locally only, this is the privacy/ no google sheet function
      localMessages(rbind(localMessages(), msg))
    }
  })
  
  observeEvent(input$refresh, {
    loadMessages()
  })
  
  output$chatTable <- renderDT({
    req(input$contactCode)
    df <- localMessages()
    df <- df %>% filter(Contact == input$contactCode) %>% arrange(desc(Time))
    datatable(df[, c("User", "Text", "Time")], options = list(pageLength = 10))
    
  })
}


shinyApp(ui, server)
