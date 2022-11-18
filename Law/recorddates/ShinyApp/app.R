require(shiny)
require(shinydashboard)
require(tidyverse)
require(data.table)
require(magrittr)
require(RJSONIO)
require(DT)

date_today <- Sys.Date()
date_prev <- date_today - 30
url_base <- paste0("https://listingmanager.nyse.com/api/shareholder-meetings/?format=json&issuer=&meeting_date_after=",date_prev,"&meeting_date_before=&published_after=&published_at=&published_before&page=%s")

############
# FUNCTION #
############

scrape_data <- function(url_base, pages){
  address_map <- lapply(1:pages, function(i){
    print(i)
    Sys.sleep(2)
    fromJSON(sprintf(url_base, i))$result
  })
  DT <- as.data.table(bind_rows(address_map))
  return(DT)
}

#############
## UI #######
#############


ui <- dashboardPage(
    ### SIDEBAR ###
    dashboardHeader(title = "Upcoming NYSE Record Dates"),
    dashboardSidebar(
      sidebarSearchForm(label = "Name", "searchText", "searchButton"),
      actionButton("refresh", label = "Refresh"),
      downloadButton("downloaddata", "Download"),
      br(),
      h5("This site lists upcoming record dates from the NYSE Proxy Rulings website. Clicking the refresh button pulls upcoming record dates posted in the last 30 days"),
      
      br(),
      br(),
      br(),

      h6("This application was developed by Scott Hirst and Ryan O'Dea, MS at Boston University")
    ),
    dashboardBody(DT::dataTableOutput("table"))
)


############
## SERVER ##
############


server <- function(input, output){
  returned_data <- eventReactive(input$refresh, {
    record_dates <- scrape_data(url_base, 7) %>%
      mutate(Issuer = issuer,
             Exchange = exchange,
             Type = type,
             `Record Date` = record_date,
             `Meeting Date` = meeting_date) %>%
      filter(record_date > date_today) %>% 
      select(Issuer, Exchange, Type, `Record Date`, `Meeting Date`)
  })
  
  output$table <- DT::renderDataTable(DT::datatable({
    withProgress(message = "Refreshing, Please wait...", {
      req(returned_data())
      data <- returned_data() 
    })
  }))
  
  output$downloaddata <- downloadHandler(
    filename = function(){
      paste0('data-', Sys.Date(), '.csv')
    },
    content = function(file){
      write.csv(returned_data(), file)
    }
  )
}


# Run the application 
shinyApp(ui = ui, server = server)
