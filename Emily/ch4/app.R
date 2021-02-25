#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Chapter 4 practice problem graphs"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("n",
                        "Select n",
                        min = 1,
                        max = 1000,
                        value = 30),
            sliderInput("lambda",
                        "Real Lambda",
                        min = .5, 
                        max = 3, 
                        step = .2)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        mu_hat <- seq(.5,2,.1)
        log_likelihood <-
            map_dbl(mu_hat, ~ -input$n * log(.x) - 1 / .x * sum(rexp(input$n, .x)))
        exp_info <- input$n / mu_hat ^ 2
        obs_info <-
            map_dbl(mu_hat, ~ -input$n / .x ^ 2 + 2 * sum(rexp(input$n, .x)) / .x ^ 3)
        score_fxn <-
            map_dbl(mu_hat, ~ -input$n / .x + 1 / .x ^ 2 * sum(rexp(input$n, .x)))
        
        data.frame(mu_hat, exp_info, obs_info, score_fxn, log_likelihood) %>%
            pivot_longer(cols = exp_info:log_likelihood) %>%
            ggplot(aes(x = mu_hat, y = value, color = name)) +
            geom_line()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
