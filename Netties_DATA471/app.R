########################################
#
#  DATA471_A2_Netball_Champs
#
########################################


library(shiny)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(reshape)
library(reshape2)
library(gghighlight)

##################################
#
#  source data and preparation
#
#################################

# development:  comment in
#dat <- data.frame(read.csv("Netties_DATA471/ANZ_Premiership_2017_2022.csv"))
# production: comment in
dat <- data.frame(read.csv("ANZ_Premiership_2017_2022.csv"))
dat$Team <- factor(dat$Team)
dat$sel_team <- dat$Team

melted <- melt.data.frame(data = dat,
                          measure.vars = c("W", "L", "D", "BP"),
                          variable_name = "WL")
year_range <- range(dat$Year)

#########################
#
#  UI
#
#########################


# Define UI for application that has barchart
ui <- fluidPage(
    # Application title
  titlePanel(
    "ANZ Netball Premiership: 2017 - 2022"
    ),

  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    
    sidebarPanel(
      # select favourite team
      selectInput(
        "myteam",
        label = "Choose your favourie team!",
        choices = unique(dat$Team),
        selected = dat$Team[1],
        multiple = TRUE,
        selectize = FALSE,
        size = 6
      ),
      
      # slider date range 
      sliderInput(
        "date_slide",
        label = "Select date range", 
        min = year_range[1], 
        max = year_range[2], 
        value = year_range,
        step = 1,
        round = TRUE
      )),
    
    mainPanel(
      
      plotOutput("stack"),
      plotOutput("line"),
      plotOutput("bubble")
  )

))


#######################################
#
#  Server 
#
#######################################

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # build stacked bar chart
  output$stack <- renderPlot({
    # filter data from ui inputs
    dat <- melted %>%
      filter(Year >= input$date_slide[1]) %>%
      filter(Year <= input$date_slide[2]) 
    
    
    # plot stacked boxplots
    ggplot(data = dat,
           aes(x = Team,
               y = value,
               fill =  WL)) +
   geom_bar(stat = "identity", 
            alpha = 0.9) +

      # geom_bar(aes(x = sel_team,
      #              y = value),
      #          stat = "identity",
      #          alpha = 0.9) +
      
      facet_grid(~Year) +
      # # coord_flip() +
      # gghighlight(Team == input$myteam) +
   theme_test() +
      theme(axis.text.x.bottom = element_text(angle = 45)) +
   labs(tite = "Netball Championship: Win-Loss record",
       x = "Championship Team",
       y = "Total games played")
    })
  
  # line plot for season points
  output$line <- renderPlot({
    dat <- melted %>% 
      # filter(sel_team == input$myteam) %>%
      filter(Year >= input$date_slide[1]) %>%
      filter(Year <= input$date_slide[2]) 
    
    ggplot(data = dat,
           aes(x = Year,
               y = Pts,
               col = Team)
           ) +
      geom_line(size =2,
                stat = "identity",
                alpha = 0.9)+
      theme_test() +
      labs(tite = "Championship points",
           x = "Year",
           y = "Total points")
  })
  
  
  # bubble plot
  # line plot for season points
  output$bubble <- renderPlot({
    dat <- melted %>% 
      # filter(sel_team == input$myteam) %>%
      filter(Year >= input$date_slide[1]) %>%
      filter(Year <= input$date_slide[2]) 
    
    
    ggplot(data = dat,
           aes(x = GF,
               y = GA,
               size = G.,
               colour = Team)) +
      geom_point(aes(group = Team),
                 alpha= 0.5) +
      # stat_point(aes(group = Team),
      #            alpha = ) +
      scale_size("Goal differential",
                 range =c(0, 20)) +
      theme_test() +
      labs(tite = "Championship points",
           x = "Goals For",
           y = "Goals Against")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
