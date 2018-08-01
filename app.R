### RISK PERFORMANCE ANALYSISinsl TOOL ###

library(shiny)
library(shinyWidgets)
library(ggplot2)
library(shinythemes)


ui <- fluidPage(
    themeSelector(),
    
    titlePanel("RISK PERFORMANCE TOOL (prototype)"),
    # SIDEBAR    
    sidebarLayout(
        
        ## SIDEBAR input parameters #
        sidebarPanel(
            numericInput("InitialAmount",
                         h3("Initial Amount in Euro:"),
                         min = 5000,
                         max = 100000,
                         value = 5000,
                         step = 100),
            br(),
            hr(),
            br(),
            
            sliderInput("TimeHorizon",
                        h3("Time Horizon in years:"),
                        min = 2,
                        max = 25,
                        step = 1, 
                        value = 5),
            br(),
            hr(),
            br(),
            
            sliderInput("MonthlyContribution",
                        h3("Monthly Contribution in Euro:"),
                        min = 0,
                        max = 5000,
                        value = 100,
                        step = 100),
            br(),
            hr(),
            br(),
            
            #checkboxGroupButtons("RiskCategory",
            radioButtons("RiskCategory",
                         h3("Choose your Risk Category:"),
                         c(
                             "DEFENSIVE  -3% / +11%" = "def",
                             "CAUTIOUS    -8% / +17%" = "cau",
                             "BALANCED    -14% / +25%" = "bal", 
                             "GROWTH      -20% / +33%" = "gro",
                             "ADVANCED    -25% / +41%" = "adv" 
                         )
            ) # end of radioButton
            #justified = TRUE, 
            #status = "primary",
            #direction = "vertical",
            #individual = FALSE,
            #checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
            #)
            
            
        ), ## ens of sidebarPanel
        
        ## MAIN PANEL: Chart and "Your Profile" window 
        mainPanel(
            
            wellPanel(#1
                h2("RISK PROFILE CHART"),
                
                plotOutput("riskChart")
            ), # end of wellPanel 1
            
            wellPanel(#2
                h2("YOUR PROFILE"),
                
                h3("Initial Amout: "),
                span(h3(textOutput(outputId = "one"), style="color:red")),
                
                br(),
                
                h3("Time Horizon: "),
                span(h3(textOutput(outputId = "two"), style="color:red")),
                
                br(),
                
                h3("Monthly Contribution:"),
                span(h3(textOutput(outputId = "three"), style="color:red")),
                
                br(),
                
                h3("Risk Category: "),
                span(h3(textOutput(outputId = "four")))
                
                
                
            ) # end of wellPanel 2
            
            ### MAIN PANEL: "Your Profile""
            
        ) ## end of main panel
    ) # end of sidebarLayout
) # end of fluidPage


server <- function(input, output) {
    
    output$riskChart <- renderPlot({
        ggplot()
    })
    
    
    output$one <- renderText({paste("Initial Amount is", input$InitialAmount, "Euro")}) 
    output$two <- renderText({paste(input$TimeHorizon, "years")})
    output$three <- renderText({paste(input$MonthlyContribution, "Euro")})
    output$four <- renderText({
        if (input$RiskCategory == "def") {
            paste("DEFENSIVE  -3% / +11%")
        } else if (input$RiskCategory == "cau") {
            paste("CAUTIOUS    -8% / +17%")
        } else if (input$RiskCategory == "bal") {
            paste("BALANCED    -14% / +25%")
        } else if (input$RiskCategory == "gro") {
            paste("GROWTH      -20% / +33%")
        } else if (input$RiskCategory == "adv") {
            paste ("ADVANCED    -25% / +41%")
        } else {
            paste("Choose a category")
        }
        
        
        
    })
}


shinyApp(ui = ui, server = server)

