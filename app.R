### RISK PERFORMANCE ANALYSIS TOOL ###

library(shiny)
library(shinyWidgets)
library(ggplot2)
library(shinythemes)
library(dplyr)
library(grid)
library(gridExtra)
library(rdrop2)
library(scales)

#---- Save Data ----#
outputDir <- "responses"

saveData <- function(data) {
    data <- t(data)
    # Create a unique file name
    fileName <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data))
    # Write the data to a temporary file locally
    filePath <- file.path(tempdir(), fileName)
    write.csv(data, filePath, row.names = FALSE, quote = TRUE)
    # Upload the file to Dropbox
    drop_upload(filePath, path = outputDir)
}

loadData <- function() {
    # Read all the files into a list
    filesInfo <- drop_dir(outputDir)
    filePaths <- filesInfo$path
    data <- lapply(filePaths, drop_read_csv, stringsAsFactors = FALSE)
    # Concatenate all data together into one data.frame
    data <- do.call(rbind, data)
    data
}

fields <- c("InitialAmount", "TimeHorizon", "MonthlyContribution", "RiskCategory")
#---- End of Save Data ----#


#---- VARIABLES & PARAMETERS ----#

# years -> Time Horizon
# monthly -> Monthly Contribution in Euro
# w -> Initial Amount in Euro
# risk -> Risk Rategory with values c(0.03, 0.08, 0.14, 0.20, 0.25) but only numbers 1-5 are relevant in this code
#       1: CAUTIOUS, 2:TENTATIVE, 3: CONFIDENT, 4: AMBITIOUS, 5: ADVENTUROUS 
# mu
# sigma
# alp -> alpha - conf. interval 0.05, 0.5, 0.95

# Model formula
# (exp(log(w+(monthly*12*years)) + years*mu + sqrt(years)*sigma*qnorm(alp)))

df <- data_frame(risk = c("cautious", "tentative", "confident", "ambitious", "adventurous"),
                 mu = c(0.0374, 0.0507, 0.0661, 0.0759, 0.0798),
                 sigma = c(0.0290, 0.0435, 0.0609, 0.0783, 0.0929))
years <- c(0:25)
alpha <- c(0.05, 0.5, 0.95)

#---- End of VARIABLES & PARAMETERS ----#


#---- User Interface ----#
ui <- fluidPage(theme = shinytheme("flatly"),
    #themeSelector(),
    
    titlePanel(h3("RISK PERFORMANCE TOOL", align = "center", style = "font-size:42px;"),
    windowTitle = "RPA-Tool"),
   
     # SIDEBAR    
    sidebarLayout(
        
        ## SIDEBAR input parameters #
        sidebarPanel(
            numericInput("InitialAmount",
                         h4("Initial Amount in Euro:"),
                         min = 5000,
                         max = 100000,
                         value = 5000,
                         step = 100),
           # hr(),
            sliderInput("TimeHorizon",
                        h4("Time Horizon* in years:"),
                        min = 0,
                        max = 25,
                        step = 1, 
                        value = 5),
            "*Choose during how many years you would like to invest",
            #hr(),
            sliderInput("MonthlyContribution",
                        h4("Monthly Contribution in Euro:"),
                        min = 0,
                        max = 5000,
                        value = 0,
                        step = 100),
            #hr(),
            div(style = "font-size: 16px;", 
                radioButtons("RiskCategory",
                             h4("Choose your Risk Category:"),
                             c(
                                 "CAUTIOUS" = "cautious",
                                 "TENTATIVE" = "tentative",
                                 "CONFIDENT" = "confident", 
                                 "AMBITIOUS" = "ambitious",
                                 "ADVENTUROUS" = "adventurous" 
                             )
                )
            ),  # end div of radioButton
            #hr(),
            tags$head(
                tags$style(HTML("#go{background-color:#00D39B; font-size: 18px}"))
            ),
            div(style = "font-size: 42px;",
            actionButton("go", "Show Plot")),
            hr(),
            #br(),
            wellPanel(#Your Profile
                style = "background-color: #ffffff;", style = "border-color: #ffffff;",
                h4("YOUR PROFILE"),
                
                h5(#"Initial Amout: "
                   ),
                    span(h5(textOutput(outputId = "one"), style="color:#3A80C3")),

                h5(#"Time Horizon: "
                   ),
                    span(h5(textOutput(outputId = "two"), style="color:#3A80C3")),

                h5(#"Monthly Contribution:"
                   ),
                    span(h5(textOutput(outputId = "three"), style="color:#3A80C3")),

                h5(#"Risk Category: "
                   ),
                    span(h5(textOutput(outputId = "four"), style="color:#3A80C3")) 
            ) #end of wellPanel Your Profile
            
        ), ## end of sidebarPanel
        
        ## MAIN PANEL: Chart and "Your Profile" window 
        mainPanel(
            
            wellPanel(#1
                h3("PERFORMANCE CHART"),
                
            navbarPage("",
                       tabPanel("Long Term (25 Years)",
                div(plotOutput(outputId = "riskChart", width = "100%", height = "500px")),
                style = "padding-right: 5%; padding-left: 5%"),
                
                       tabPanel("Short Term (5 Years)",
                div(plotOutput(outputId = "riskChartFive", width = "100%", height = "500px")),
                    style = "padding-right: 5%; padding-left: 5%; width: 100%; height: 500%"
                )
                
            ), # end of navbarPage
            br(),
            
            h4(img(src = "chart_icon.png", #height = 480, width = 320, 
                                     width = "3%"), "Understanding the Performance Chart"), 
            h5("The above chart represents the expected returns 
            you may have after a specific amount of investment 
            years (so called Time-Horizon)."),
            br(),
            h5("4 Lines are represented:",
               br(strong("•	Invested:"), "is the total amount you have invested during the time frame of your investment"),
               strong("•	Expected:"), "is the most likely expected outcome / return of your investment on a given date",
               br(strong("•	Optimistic:"), " is the maximum outcome you could be expecting from your investment"),
               strong("•	Pessimistic:"), "is the minimum or lowest outcome you could be expecting from your investment (with a 95% probability)" 
            )
            ), # end of wellPanel 1
            
           wellPanel(#2
             wellPanel(
                h4(img(src = "info_icon.png", #height = 480, width = 320, 
                       width = "3%"), "How to choose your Risk Category?"),
                h5("With investing, there’s always a chance that investments 
                   can go down in value. By defining your risk category or 
                   risk/reward balance, you define the maximum loss you could 
                   accept for the maximum reward possible.")),
              wellPanel(align = "center",
                img(src = "RPATable.png", width = "100%")
            )
           )
        ) ## end of main panel
    ) # end of sidebarLayout
) # end of fluidPage

#---- End of User Interface ----#


#---- Server ----#
server <- function(input, output) {

    v <- reactiveValues(doPlot = FALSE)
    
    observeEvent(input$go, {
        # 0 will be coerced to FALSE
        # 1+ will be coerced to TRUE
        v$doPlot <- input$go
    })
 
    output$riskChart <- renderPlot({
        
    if (v$doPlot == FALSE) return() 
        
    isolate({
        riskParams <- filter(df, risk == input$RiskCategory)
        muu <- riskParams$mu
        sigmaa <- riskParams$sigma
        
        RPA_model <- function(alpha, muu) {
            c(
                exp(log(input$InitialAmount + (input$MonthlyContribution*12*years)) + 
                         years*muu + sqrt(years)*sigmaa*qnorm(alpha))
            )
        }

        optimistic_scenario <- data_frame(years, return = RPA_model(alpha[3],muu = muu), scenario = rep("Optimistic", length(years)))
        expected_scenario <- data_frame(years, return = RPA_model(alpha[2], muu = muu), scenario = rep("Expected", length(years)))
        pessimistic_scenario <- data_frame(years, return = RPA_model(alpha[1], muu = muu), scenario = rep("Pessimistic", length(years)))
        invested_scenario <- data_frame(years, return = RPA_model(alpha[2],muu = 0), scenario = rep("Invested", length(years)))

        g <- bind_rows(invested_scenario, pessimistic_scenario, expected_scenario, optimistic_scenario)

        #g$returnminsd <- g$return - sd(g$return, na.rm = TRUE)
        #g$returnmaxsd <- g$return + sd(g$return, na.rm = TRUE)
        
        p <- ggplot(g, aes(x = years, 
                           y = return, 
                           color = factor(scenario, ordered = TRUE, 
                                          levels = c("Optimistic", "Expected", "Pessimistic", "Invested")))) + 
            #geom_ribbon(aes(ymin = min(return), ymax = return, fill = factor(scenario, ordered = TRUE, levels = c("Optimistic", "Expected", "Pessimistic", "Invested"))), alpha = 0.5) +
            #scale_fill_manual(name='', values = c("Invested"= "#ffffff", "Pessimistic"="#ffffff", "Expected"="#e52b50", "Optimistic"="#70e0a0")) +
            theme_classic(base_size = 20) + 
            theme(legend.title = element_blank()) +
            geom_line(linetype = "solid", size = 2) +
            scale_y_continuous(labels = comma) +
            scale_color_manual(values = c("#6fb3e8", "#2175c4", "#e2768b", "#a8bdd1")) + 
            geom_vline(xintercept = input$TimeHorizon, color = "black", linetype="dotted") 

        abc <- (filter(g, years == input$TimeHorizon))

        p2 <- p + geom_point(data=abc, aes(x=years, y=return), size = 8) + 
            geom_label(data=abc, aes(x=years, y=return, label=paste(round(return, digits = 0), "€")), 
                       nudge_x = 2, nudge_y = 1, fontface = "bold", size = 6, show.legend=FALSE) +
            labs(y = "Return in €", x = "Years") 
        

        p2 
   
    })
    })
    
    v2 <- reactiveValues(doPlot2 = FALSE)
    
    observeEvent(input$go, {
        # 0 will be coerced to FALSE
        # 1+ will be coerced to TRUE
        v2$doPlot2 <- input$go
    })
    
    output$riskChartFive <- renderPlot({
        
        if (v2$doPlot2 == FALSE) return() 
        
        isolate({
        riskParams <- filter(df, risk == input$RiskCategory)
        muu <- riskParams$mu
        sigmaa <- riskParams$sigma
        
        RPA_model <- function(alpha, muu) {
            c(
                exp(log(input$InitialAmount + (input$MonthlyContribution*12*years)) + 
                        years*muu + sqrt(years)*sigmaa*qnorm(alpha))
            )
        }
        
        optimistic_scenario <- data_frame(years, return = RPA_model(alpha[3],muu = muu), scenario = rep("Optimistic", length(years)))
        expected_scenario <- data_frame(years, return = RPA_model(alpha[2], muu = muu), scenario = rep("Expected", length(years)))
        pessimistic_scenario <- data_frame(years, return = RPA_model(alpha[1], muu = muu), scenario = rep("Pessimistic", length(years)))
        invested_scenario <- data_frame(years, return = RPA_model(alpha[2],muu = 0), scenario = rep("Invested", length(years)))
        
        g5 <- bind_rows(invested_scenario, pessimistic_scenario, expected_scenario, optimistic_scenario) %>% 
            filter(years <= 5)
        
        #g$returnminsd <- g$return - sd(g$return, na.rm = TRUE)
        #g$returnmaxsd <- g$return + sd(g$return, na.rm = TRUE)
        
        p5 <- ggplot(g5, aes(x = years, 
                           y = return, 
                           color = factor(scenario, ordered = TRUE, 
                                          levels = c("Optimistic", "Expected", "Pessimistic", "Invested")))) + 
            #geom_ribbon(aes(ymin = min(return), ymax = return, fill = factor(scenario, ordered = TRUE, levels = c("Optimistic", "Expected", "Pessimistic", "Invested"))), alpha = 0.5) +
            #scale_fill_manual(name='', values = c("Invested"= "#ffffff", "Pessimistic"="#ffffff", "Expected"="#e52b50", "Optimistic"="#70e0a0")) +
            theme_classic(base_size = 20) + 
            theme(legend.title = element_blank()) +
            geom_line(linetype = "solid", size = 2) +
            scale_y_continuous(labels = comma) +
            scale_color_manual(values = c("#6fb3e8", "#2175c4", "#e2768b", "#a8bdd1")) + 
            geom_vline(xintercept = input$TimeHorizon, color = "black", linetype="dotted") 
        
        abc <- (filter(g5, years == input$TimeHorizon))
        
        
        p52 = p5 + geom_point(data=abc, aes(x=years, y=return), size = 8) + 
            geom_label(data=abc, aes(x=years, y=return, label=paste(round(return, digits = 0), "€")), 
                       nudge_x = 0.35, nudge_y = 1, fontface = "bold", size = 6, show.legend = FALSE) +
            #xlim(NA, 5) + ylim(NA, ymaxim$return) + 
            labs(y = "Return in €", x = "Years")
            
           # theme(legend.key=element_rect(color=c("#6fb3e8", "#2175c4", "#e2768b", "#a8bdd1"), fill=c("#6fb3e8", "#2175c4", "#e2768b", "#a8bdd1")))
        
        p52 
        
    })
})
    
    
    output$one <- renderText({paste("Initial Amount is:", input$InitialAmount, "Euro")}) 
    output$two <- renderText({paste("Time Horizon is:", input$TimeHorizon, "years")})
    output$three <- renderText({paste("Monthly Contribution is:", input$MonthlyContribution, "Euro")})
    output$four <- renderText({
        if (input$RiskCategory == "cautious") {
            (paste("Risk Cathegory is: CAUTIOUS"))
        } else if (input$RiskCategory == "tentative") {
            paste("Risk Cathegory is: TENTATIVE")
        } else if (input$RiskCategory == "confident") {
            paste("Risk Cathegory is: CONFIDENT")
        } else if (input$RiskCategory == "ambitious") {
            paste("Risk Cathegory is: AMBITIOUS")
        } else if (input$RiskCategory == "adventurous") {
            paste ("Risk Cathegory is: ADVENTUROUS")
        } else {
            paste("Choose a category")
        }
        
    })
    
    
    formData <- reactive({
        data <- sapply(fields, function(x) input[[x]])
        data
    })
    
    
    observeEvent(input$go, {
        saveData(formData())
    })
    
}

#---- End of Server functions ----#


shinyApp(ui = ui, server = server)

