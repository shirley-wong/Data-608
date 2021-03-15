library(tidyverse)
library(shiny)
library(plotly)

df <- read_csv('https://raw.githubusercontent.com/shirley-wong/Data-608/main/module3/cleaned-cdc-mortality-1999-2010-2.csv')

df_avg <- df %>%
    group_by(ICD.Chapter, Year) %>%
    summarise(National_Avg = weighted.mean(Crude.Rate, Population))

df <- df %>% left_join(df_avg, by = c('ICD.Chapter', 'Year')) 


# Define UI for application that draws a histogram
ui <- fluidPage(
    
    #set app title
    titlePanel('DATA 608 Module 3: Interactive Visualizations in R'),
    
    #create a horizontal tab panel on top
    tabsetPanel(
        
        #content of tab panel 1
        tabPanel('Question 1',
                 
                 #create a siderbar-mainpanel layout in tab panel 1
                 sidebarLayout(
                     
                     #content in siderbar for question 1
                     sidebarPanel(
                         h4('Criteria'),
                         selectInput('Q1_Year', 'Year', unique(df$Year), selected = 2010),
                         selectInput('Q1_Cause', 'Cause', unique(df$ICD.Chapter), selected = 'Neoplasms'),
                         actionButton('Q1_Update', 'Update')
                         ),
                     
                     #content in mainpanel for question 1
                     mainPanel(
                         strong(textOutput('Q1_title')),
                         plotlyOutput('Q1_Barplot'),
                         )
                     )
                 ),
        
        #content of tab panel 2
        tabPanel('Question 2',
                 
                 #create a siderbar-mainpanel layout in tab panel 2
                 sidebarLayout( 
                     
                     #content in siderbar for question 2
                    sidebarPanel(
                         h4('Criteria'),
                         selectInput('Q2_State', 'State', unique(df$State), selected = 'AL'),
                         selectInput('Q2_Cause', 'Cause', unique(df$ICD.Chapter), selected = 'Neoplasms'),
                         actionButton('Q2_Update', 'Update')
                     ),
                 
                    #content in mainpanel for question 2
                    mainPanel(
                        strong(textOutput('Q2_title')),
                        plotlyOutput('Q2_Lineplot')
                     )
                 )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    #----------Question 1------------------#
    rv <- reactiveValues(data = df %>% 
                             filter(Year == 2010,
                                    ICD.Chapter == 'Neoplasms'),
                         cause = 'Neoplasms'
                         )
    
    
    observeEvent(input$Q1_Update, {
        rv$data <- df %>% 
            filter(Year == input$Q1_Year,
                   ICD.Chapter == input$Q1_Cause)
    })
    
    observeEvent(input$Q1_Update, {
        rv$cause <- input$Q1_Cause
    })
    
    
    output$Q1_Barplot <- renderPlotly({
        plot_ly(data = rv$data, 
                x = ~Crude.Rate, 
                y = ~reorder(State, Crude.Rate), 
                type = 'bar', 
                orientation = 'h',
                height = 800,
                width = 500) %>%
            layout(#title = paste('Crude Mortality Rate of Cause:',input$Q1_Cause, 'Across States'),
                   yaxis = list(title = 'State'))
        
    })
    
    output$Q1_title <- renderText(paste('Crude Mortality Rate of Cause:',rv$cause, 'Across States'))
    
    
    
    
    #----------Question 2------------------#
    rv2 <- reactiveValues(data = df %>% 
                              filter(State == 'AL',
                                     ICD.Chapter == 'Neoplasms'),
                          cause = 'Neoplasms',
                          State = 'AL'
    )
    
    
    observeEvent(input$Q2_Update, {
        rv2$data <- df %>% 
            filter(State == input$Q2_State,
                   ICD.Chapter == input$Q2_Cause)
    })
    
    observeEvent(input$Q2_Update, {
        rv2$cause <- input$Q2_Cause
    })
    
    observeEvent(input$Q2_Update, {
        rv2$State <- input$Q2_State
    })  
    
    output$Q2_Lineplot <- renderPlotly({
        plot_ly(rv2$data) %>%
            add_lines(x = ~Year, 
                      y = ~Crude.Rate,
                      name = 'Crude Rate') %>%
            add_lines(x = ~Year,
                      y = ~National_Avg,
                      name = 'National Average')
    })
    
    output$Q2_title <- renderText(paste('Crude Mortality Rate of Cause:',rv2$cause, 'in', rv2$State, 'from Year 1999 to 2010'))
    
}

# Run the application 
shinyApp(ui = ui, server = server)
