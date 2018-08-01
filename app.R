### RISK PERFORMANCE ANALYSISinsl TOOL ###

library(shiny)
library(shinyWidgets)
library(ggplot2)
library(shinythemes)
library(dplyr)

# VARIABLES & PARAMETERS

# years -> Time Horizon
# monthly -> Monthly Contribution in Euro
# w -> Initial Amount in Euro
# risk -> Risk Rategory with values c(0.03, 0.08, 0.14, 0.20, 0.25) but only numbers 1-5 are relevant in this code
#       1: DEFENSIVE, 2: CAUTIOUS, 3: BALANCED, 4: GROWTH, 5: ADVANCED
# mu
# sigma
# alp -> alpha - conf. interval 0.05, 0.5, 0.95

# Model formula
# (exp(log(w+(monthly*12*years)) + years*mu + sqrt(years)*sigma*qnorm(alp)))

df <- data_frame(risk = c("def", "cau", "bal", "gro", "adv"),
                 mu = c(0.0374, 0.0507, 0.0661, 0.0759, 0.0798),
                 sigma = c(0.0290, 0.0435, 0.0609, 0.0783, 0.0929))
years <- c(0:25)
alpha <- c(0.05, 0.5, 0.95)


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
            div(style = "font-size: 28px;", 
                radioButtons("RiskCategory",
                             h3("Choose your Risk Category:"),
                             c(
                                 "DEFENSIVE  -3% / +11%" = "def",
                                 "CAUTIOUS    -8% / +17%" = "cau",
                                 "BALANCED    -14% / +25%" = "bal", 
                                 "GROWTH      -20% / +33%" = "gro",
                                 "ADVANCED    -25% / +41%" = "adv" 
                             )
                )
            ) # end div of radioButton
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
                
                h2("Initial Amout: "),
                span(h3(textOutput(outputId = "one"), style="color:mediumblue")),
                
                br(),
                
                h2("Time Horizon: "),
                span(h3(textOutput(outputId = "two"), style="color:mediumblue")),
                
                br(),
                
                h2("Monthly Contribution:"),
                span(h3(textOutput(outputId = "three"), style="color:mediumblue")),
                
                br(),
                
                h2("Risk Category: "),
                span(h3(textOutput(outputId = "four"), style="color:mediumblue"))
                
                
                
            ) # end of wellPanel 2
            
            ### MAIN PANEL: "Your Profile""
            
        ) ## end of main panel
    ) # end of sidebarLayout
) # end of fluidPage


server <- function(input, output) {
    
    output$riskChart <- renderPlot({
        
        riskParams <- filter(df, risk == input$RiskCategory)
        muu <- riskParams$mu
        sigmaa <- riskParams$sigma
        
        RPA_model <- function(alpha) {
            c(
                exp(log(input$InitialAmount + (input$MonthlyContribution*12*years)) + 
                         years*muu + sqrt(years)*sigmaa*qnorm(alpha))
            )
        }

        pessimistic_scenario <- data_frame(years, return = RPA_model(alpha[1]), scenario = rep("Pessimistic", length(years)))
        expected_scenario <- data_frame(years, return = RPA_model(alpha[2]), scenario = rep("Expected", length(years)))
        
        bind_rows(pessimistic_scenario, expected_scenario) %>%
            ggplot(aes(x = years, y = return, color = scenario)) + geom_line() + theme_linedraw(base_size = 20)
    })
    
    
    output$one <- renderText({paste("Initial Amount is", input$InitialAmount, "Euro")}) 
    output$two <- renderText({paste(input$TimeHorizon, "years")})
    output$three <- renderText({paste(input$MonthlyContribution, "Euro")})
    output$four <- renderText({
        if (input$RiskCategory == "def") {
            (paste("DEFENSIVE: from -3% to +11%"))
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

