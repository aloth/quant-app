library(shiny)
library(quantmod)
library(xts)
library(TTR)
library(VGAM)

# Define server logic for random distribution application
shinyServer(function(input, output) {
    
    # acquiring data
    dataInput <- reactive({
        if (input$get == 0)
            return(NULL)
        
        return(isolate({
            getSymbols(input$symb,src="yahoo", auto.assign = FALSE)
        }))
    })
    
    datesInput <- reactive({
        if (input$get == 0)
            return(NULL)
        
        return(isolate({
            paste0(input$dates[1], "::",  input$dates[2])
        }))
    })
    
    returns <- reactive({ 
        if (input$get == 0)
            return(NULL)
        
        dailyReturn(dataInput())
    })
    
    xs <- reactive({ 
        if (input$get == 0)
            return(NULL)
        
        span <- range(returns())
        seq(span[1], span[2], by = diff(span) / 100)
    })
    
    # tab based controls
    output$newBox <- renderUI({
        switch(input$tab,
               "Charts" = chartControls,
               "Model" = modelControls,
               "About" = helpText()
        )
    })
    
    # Charts tab
    chartControls <- div(
        wellPanel(
            selectInput("chart_type",
                        label = "Chart type",
                        choices = c("Candlestick" = "candlesticks", 
                                    "Matchstick" = "matchsticks",
                                    "Bar" = "bars",
                                    "Line" = "line"),
                        selected = "Candlestick"
            ),
            selectInput("chart_theme",
                        label = "Chart theme",
                        choices = names(quantmod:::.chart.theme),
                        selected = "white"
            ),
            checkboxInput(inputId = "log_y", label = "log y axis", 
                          value = FALSE)
        ),
        
        wellPanel(
            p(strong("Technical Analysis")),
            checkboxInput("ta_vol", label = "Volume", value = FALSE),
            checkboxInput("ta_sma", label = "Simple Moving Average", 
                          value = FALSE),
            checkboxInput("ta_ema", label = "Exponential Moving Average", 
                          value = FALSE),
            checkboxInput("ta_wma", label = "Weighted Moving Average", 
                          value = FALSE),
            checkboxInput("ta_rsi", label = "Relative Strength Indicator", 
                          value = FALSE),
            checkboxInput("ta_bb", label = "Bolinger Bands", 
                          value = FALSE),
            checkboxInput("ta_atr", label = "Average True Range", 
                          value = FALSE), 
            checkboxInput("ta_momentum", label = "Momentum", 
                          value = FALSE),
            checkboxInput("ta_macd", label = "Moving Average Convergence Divergence", 
                          value = FALSE),            
            br(),
            
            actionButton("chart_act", "Update Technical Analysis")
        )
    )
    
    TAInput <- reactive({
        if (input$chart_act == 0)
            return("NULL")
        
        tas <- isolate({c(input$ta_vol, input$ta_sma, input$ta_ema, input$ta_wma, input$ta_rsi,
                          input$ta_bb, input$ta_atr, input$ta_momentum, input$ta_macd)})
        funcs <- c(addVo(), addSMA(), addEMA(), addWMA(), addRSI(),
                   addBBands(), addATR(), addMomentum(), addMACD())
        
        if (any(tas)) funcs[tas]
        else "NULL"
    })
    
    output$chart <- renderPlot({
        chartSeries(dataInput(),
                    name = input$symb,
                    type = input$chart_type,
                    subset = datesInput(),
                    log.scale = input$log_y,
                    theme = input$chart_theme,
                    TA = TAInput())
    })
    
    # Model tab
    modelControls <- div(
        br(),
        
        sliderInput("n", "Number of bins in histogram",
                    min = 1, max = 300, value = 30
        ),
        
        br(),
        
        wellPanel(
            selectInput("family", "Model returns as",
                        choices = c("normal", "double exponential", "t"),
                        selected = "normal"
            ),
            
            sliderInput("mu", "Mean",
                        min = -1, max = 1, value = 0, step = 0.01
            ), 
            
            sliderInput("sigma", "Standard Deviation",
                        min = 0, max = 0.1, value = 0.05, step = 0.001
            ),
            conditionalPanel(condition = "input.family == 't'",
                             sliderInput("df", "Degrees of freedom",
                                         min = 2, max = 1000, value = 10
                             )
            )
            
        )
    )
    
    ys <- reactive({ 
        if (input$get == 0)
            return(NULL)
        
        switch(input$family,
               "double exponential" = dlaplace(xs(), 
                                               location = input$mu, 
                                               scale = input$sigma
               ),
               "normal" = dnorm(xs(), 
                                mean = input$mu, 
                                sd = input$sigma
               ),
               "t" = dt((xs() - input$mu) / input$sigma,
                        df = input$df) * sqrt(2 * length(returns()))
        )
    })
    
    ks <- reactive({
        switch(input$family,
               "double exponential" = ks.test(returns(), "plaplace", 
                                              input$mu, input$sigma),
               "normal" = ks.test(returns(), "pnorm", 
                                  input$mu, input$sigma),
               "t" = ks.test((returns() - input$mu) / input$sigma, "pt", 
                             input$df)
        )
    })
    
    output$hist <- renderPlot({
        hist(returns(), xlab = "returns", freq = FALSE,
             main = paste(input$symb, "Daily Returns:", 
                          input$dates[1], "-", input$dates[2], sep = " "),
             breaks = input$n)
        lines(xs(), ys(), col = "red")
    })
    
    
    output$ks <- renderText({
        paste0("Kolmogorv-Smirnoff statistic: ", ks()$statistic)
    })
    output$ksp <- renderText({
        paste0("P-value for model: ", ks()$p.value)
    })
    
    # About tab
    output$textAbout <- renderText({paste0("This Shiny application helps to analyze different financial assets.
In order to perform a technical analysis of the selected asset various chart types and technical indicators are available.
This application was published by Alexander Loth (@xlth) for demostration and evaluation purposes only.
                                           Last update: 2015-09-15.")})
    
})