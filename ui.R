library(shiny)

# Define UI for random distribution application 
shinyUI(pageWithSidebar(
    
    headerPanel("Quantitative Financial Analysis"),
    
    sidebarPanel(
        
        helpText("Select a stock or index to examine. 
                 (Data source: Yahoo finance)"),
        
        textInput("symb", "Symbol", "^GDAXI"),
        
        ##selectInput("src",
        ##            label = "Data source",
        ##            choices = c("Yahoo! Finance" = "yahoo", 
        ##                        "Google Finance" = "google",
        ##                        "Federal Reserve Bank" = "FRED",
        ##                        "Oanda, The Currency Site" = "Oanda"),
        ##            selected = "Yahoo! Finance"),
        
        dateRangeInput("dates", 
                       "Compare to historic returns from",
                       start = Sys.Date()-90, end = Sys.Date()),
        
        actionButton("get", "Get Data"),
        
        br(),
        br(),

        uiOutput("newBox")
        
        ),
    
    # Show a tabset that includes a plot, summary, and table view
    # of the generated distribution
    mainPanel(
        tags$style(type="text/css",
                   ".shiny-output-error { visibility: hidden; }",
                   ".shiny-output-error:before { visibility: hidden; }"),
        tabsetPanel(
            tabPanel("Charts", plotOutput("chart")), 
            tabPanel("Model", plotOutput("hist"), div(textOutput("ks")), 
                     div(textOutput("ksp")) 
                     ), 
            tabPanel("About", textOutput("textAbout")),
            id = "tab"
        )
    )
))