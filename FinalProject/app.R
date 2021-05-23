library(tidyverse)
library(shiny)
library(shinydashboard)
library(plotly)
library(lubridate)
library(leaflet)
library(tigris)
library(htmltools)
library(censusapi)
library(DT)
library(rsconnect)

ts <- read_csv('https://raw.githubusercontent.com/shirley-wong/Data-608/main/FinalProject/Data/time-series-19-covid-combined.csv')
us_1 <- read_csv('https://raw.githubusercontent.com/shirley-wong/Data-608/main/FinalProject/Data/us_simplified_1.csv')
us_2 <- read_csv('https://raw.githubusercontent.com/shirley-wong/Data-608/main/FinalProject/Data/us_simplified_2.csv')
us_3 <- read_csv('https://raw.githubusercontent.com/shirley-wong/Data-608/main/FinalProject/Data/us_simplified_3.csv')
ww <- read_csv('https://raw.githubusercontent.com/shirley-wong/Data-608/main/FinalProject/Data/worldwide-aggregate.csv')


## Tab1 Data: US
us <- us_1 %>%
    rbind(us_2, us_3) %>%
    select(-Admin2, -Country.Region) %>%
    mutate(Date=as_date(Date)) %>%
    rename(State=Province.State) %>%
    gather(key = 'Type', value = 'Count',-Date, -State) %>%
    mutate(State = case_when(
        State == "Northern Mariana Islands" ~ "Commonwealth of the Northern Mariana Islands",
        State == "Virgin Islands" ~ "United States Virgin Islands",
        TRUE ~ State))

state_off <- data.frame(state.abb, state.name) %>% 
    add_row(state.abb="AS", state.name="American Samoa") %>%
    add_row(state.abb="DC", state.name="District of Columbia") %>%
    add_row(state.abb="GU", state.name="Guam") %>%
    add_row(state.abb="MP", state.name="Commonwealth of the Northern Mariana Islands") %>%
    add_row(state.abb="PR", state.name="Puerto Rico") %>%
    add_row(state.abb="VI", state.name="United States Virgin Islands")
colnames(state_off) <- c("ST", "State")
us <- left_join(us, state_off)


## Tab1 Data: Population
#ref: https://learn.r-journalism.com/en/mapping/census_maps/census-maps/
CENSUS_KEY = "a9c05d4d76efcbd0906e761072691d79ed7a8cd9"
Sys.setenv(CENSUS_KEY = "a9c05d4d76efcbd0906e761072691d79ed7a8cd9")
readRenviron("~/.Renviron")
#Sys.getenv("CENSUS_KEY")

state_pop <- getCensus(name="acs/acs5",
                       vintage=2015,
                       key=CENSUS_KEY,
                       vars=c("NAME", "B01003_001E"),
                       region="state:*")
colnames(state_pop) <- c("state_id", "State", "Population")
state_pop$state_id <- as.numeric(state_pop$state_id)

# Joining state population dataframe to relationship file
state_pop <- left_join(state_pop, state_off)
state_pop <- state_pop %>% add_row(state_id=3, State="American Samoa", Population=46366, ST="AS") %>%
    add_row(state_id=14, State="Guam", Population=168801, ST="GU") %>%
    add_row(state_id=7, State="Commonwealth of the Northern Mariana Islands", Population=51659, ST="MP") %>%
    add_row(state_id=52, State="United States Virgin Islands", Population=105870, ST="VI") %>%
    select(-state_id)


## Tab2 Data: Countries ts
ts <- ts %>%
    select(-`Province/State`, -Recovered) %>%
    mutate(Date=as_date(Date)) %>%
    rename(Country = `Country/Region`) %>%
    gather(key = 'Type', value = 'Count', -Date, -Country)

top10 <- ts %>% group_by(Country) %>% filter(Type=="Confirmed") %>%
    summarise(Count=sum(Count)) %>% 
    arrange(desc(Count)) %>% top_n(10) %>% select(Country)

ts <- ts %>% filter(Country %in% top10$Country) %>%
    select(Country, Date, Type, Count) 


## Tab3 Data: Worldwide ww
ww <- ww %>% 
    select(-`Increase rate`) %>%
    gather(key = 'Type', value = 'Count', -Date) %>%
    group_by(Type) %>%
    mutate(Total = cumsum(Count))


## Define UI

sidebar <- dashboardSidebar(width=300,
                            sidebarMenu(
                                menuItem("United States", tabName="us", icon=icon("user")),
                                menuItem("Countries", tabName="countries", icon=icon("users")),
                                menuItem("Worldwide", tabName="worldwide", icon=icon("globe")),
                                menuItem("About", tabName="info", icon=icon("info"))
                            )
)

body <- dashboardBody(
    tabItems(
        tabItem(tabName="us",
                fluidPage(
                    fluidRow(
                        box(title=strong("United States COVID-19 Cases and Deaths by State"), 
                            width=12, 
                            solidHeader=TRUE)
                        #background = "teal" ,status="primary")
                    ),
                    fluidRow(
                        column(width=2,
                               radioButtons("us_Cases", "Cases:",
                                            choices = c("Confirmed", "Deaths"),
                                            selected = "Confirmed")),
                        column(width=3,
                               dateRangeInput('us_Date', "Time Period:", 
                                              min = min(us$Date), 
                                              max = max(us$Date),
                                              start = min(us$Date), 
                                              end = max(us$Date))),
                               #sliderInput("us_Date", "Time Period:", 
                               #            min = min(us$Date), 
                               #            max = max(us$Date),
                               #            value = c(min(us$Date), max(us$Date)))),
                        actionButton("us_Update", "Update")
                    ),
                    
                    fluidRow(
                        column(width=4,
                               box(width=NULL, 
                                   height = '860px', status = "primary", 
                                   title = textOutput('us_Chart_Title'), solidHeader = TRUE,
                                   plotlyOutput("us_Chart"))
                        ),
                        column(width=8,
                               box(width=NULL, 
                                   height = '860px', status = "primary", 
                                   title = textOutput('us_Map_Title'), solidHeader = TRUE,
                                   leafletOutput("us_Map", height=800, width="100%"))
                        )
                    )
                )
        ),
        
        tabItem(tabName="countries",
                
                fluidPage(
                    fluidRow(
                        box(title=strong("COVID-19 Confirmed and Deaths Cases by Countries"), 
                            width=12, 
                            solidHeader=TRUE)
                    ),
                    
                    fluidRow(
                        column(width=8,
                               
                               fluidRow(
                                   
                                   column(width=7,
                                          sliderInput("ts_Date", "Time Period:",
                                                      min = min(ts$Date), 
                                                      max = max(ts$Date),
                                                      value = c(min(ts$Date), max(ts$Date)))),
                                   column(width=5,
                                          radioButtons("ts_Cases", "Cases:",
                                                       choices = c("Confirmed", "Deaths"),
                                                       selected = "Confirmed",
                                                       inline = TRUE))
                               ),
                               fluidRow(
                                   column(width=12,
                                          checkboxGroupInput("ts_Name", "Countries:",
                                                             choices = unique(top10$Country),
                                                             selected = top10$Country,
                                                             inline = TRUE))
                               ),
                               fluidRow(
                                   box(width=12, 
                                       title = textOutput('ts_Chart_Title'), status = "primary",
                                       height = '720px', solidHeader = TRUE,
                                       plotlyOutput("ts_Chart"))
                               )
                        ),
                        column(width=4,
                               box(width=12, 
                                   title = "Monthly Case Detail by Country", status = "primary", 
                                   height='870px', solidHeader = TRUE,
                                   column(width=12, dataTableOutput("ts_Table"), 
                                          style="height:800px; overflow-y: scroll;overflow-x: scroll;")))
                    )
                    
                )
        ),
        
        tabItem(tabName="worldwide",
                
                fluidPage(
                    
                    fluidRow(
                        box(width=12, 
                            title=strong("COVID-19 Confirmed, Recovered and Deaths Cases Worldwide"),
                            solidHeader=TRUE)
                    ),
                    
                    fluidRow(
                        valueBoxOutput("ww_box_Confirmed"),
                        valueBoxOutput("ww_box_Recovered"),
                        valueBoxOutput("ww_box_Deaths")
                        
                    ),
                    
                    fluidRow(
                        column(width=12,
                               box(width=12, 
                                   title = "Cumulative Covid-19 Cases Worldwide",
                                   status = "primary",
                                   height = '750px', solidHeader = TRUE,
                                   plotlyOutput("ww_Chart")))
                        
                    )
                    
                )
                
        ),
        
        tabItem(tabName="info",
                
                fluidPage(
                    box(width=12, title=strong("Reference:"), solidHeader=TRUE,
                        #HTML("
                        h5(strong('Data Source:')),
                        tagList('The Coronavirus disease 2019 (Covid-19) dataset is in CSV format and is sourced from a upstream repository 
                        maintained by the Johns Hopkinds University Center for Systems Science and Engineering (CSSE) on Github which 
                        comes from a variety of public sources. The data can be found here: ', 
                                a('(https://github.com/datasets/covid-19)', href='https://github.com/datasets/covid-19')),
                        br(),br(),
                        p('Aggregated data sources:'),
                        tagList('- [World Health Organization (WHO)]', a('(https://www.who.int/)', href='https://www.who.int/')),
                        br(),
                        tagList('- [European Centre for Disease Prevention and Control (ECDC)]', a('(https://www.ecdc.europa.eu/en/geographical-distribution-2019-ncov-cases)', href='https://www.ecdc.europa.eu/en/geographical-distribution-2019-ncov-cases')),
                        br(),
                        tagList('- [DXY.cn. Pneumonia. 2020.]', a('(http://3g.dxy.cn/newh5/view/pneumonia)', href='http://3g.dxy.cn/newh5/view/pneumonia')),
                        br(),
                        tagList('- [US CDC]', a('(https://www.cdc.gov/coronavirus/2019-ncov/index.html)', href='https://www.cdc.gov/coronavirus/2019-ncov/index.html')),
                        br(),
                        tagList('- [BNO News]', a('(https://bnonews.com/index.php/2020/02/the-latest-coronavirus-cases/)', href='https://bnonews.com/index.php/2020/02/the-latest-coronavirus-cases/')),
                        br(),
                        tagList('- [WorldoMeters]', a('(https://www.worldometers.info/coronavirus/)', href='https://www.worldometers.info/coronavirus/')),
                        br(),
                        tagList('- [1Point3Arces]', a('(https://coronavirus.1point3acres.com/en)', href='https://coronavirus.1point3acres.com/en')),
                        br(),
                        tagList('- [COVID Tracking Project]', a('(https://covidtracking.com/data)', href='https://covidtracking.com/data'), "(US Testing and Hospitalization Data. We use the maximum reported value from 'Currently' and 'Cumulative' 
                        Hospitalized for our hospitalization number reported for each state.)"),
                        br(), 
                        br(),
                        h5(strong('US Centers for Disease Control and Prevention (CDC) Covid-19 Information:')),
                        tagList('CDC Covid-19 Homepage: ', a('https://www.cdc.gov/coronavirus/2019-nCoV/index.html', href='https://www.cdc.gov/coronavirus/2019-nCoV/index.html')),
                        br(),
                        tagList('CDC Covid-19 Data Tracker: ', a('https://covid.cdc.gov/covid-data-tracker/#datatracker-home', href='https://covid.cdc.gov/covid-data-tracker/#datatracker-home')),
                        br(), br(),
                        h5(strong('Tools:')),
                        p('This project is managed in R, using ', em('shiny, plotly, leaflet, '), 'and ', em('censusapi'), ', displayed using ', em('shinydashboard'), '.'))

                )
                
        )
    )
)

ui <- dashboardPage(skin="blue",
                    dashboardHeader(title='Covid-19 Cases Dashboard', titleWidth=300),
                    sidebar,
                    body
)



## Define server

server <- function(input, output) {
    
    # -------------------------- Menu1: United States --------------------------- #
    rv <- reactiveValues(data = us %>% 
                             filter(Type == 'Confirmed') %>%
                             group_by(State, ST) %>%
                             summarise(Count=sum(Count)), 
                         case = "Confirmed"
    )
    
    
    observeEvent(input$us_Update, {
        rv$data <- us %>% 
            filter(Type == input$us_Cases,
                   Date >= min(input$us_Date) & Date <= max(input$us_Date)) %>%
            group_by(State, ST) %>%
            summarise(Count=sum(Count))
    })
    
    observeEvent(input$us_Update, {
        rv$case <- input$us_Cases
    })
    
    output$us_Chart <- renderPlotly({
        plot_ly(data = rv$data, 
                x = ~Count, 
                y = ~reorder(State, Count), 
                type = 'bar', 
                orientation = 'h',
                marker = list(color = ~Count, colorscale = "YlGnBu"),
                #color = ~Count,
                #colors = "YlOrRd",
                width = 400,
                height = 800) %>%
            layout(yaxis = list(title = 'State', tickfont = list(size=9)),
                   xaxis = list(tickfont = list(size=10)))
        
    })
    
    output$us_Chart_Title <- renderText(paste(rv$case, ' Cases By State'))
    
    output$us_Map_Title <- renderText(paste(rv$case, ' Cases Per 100,000 Residents in Each State'))
    
    output$us_Map <- renderLeaflet({
        
        # Joining us dataframe to adjusted state population dataframe
        cov_state_pop <- left_join(rv$data, state_pop)
        
        # Calculating per 100,000 residents and rounding to 2 digits
        cov_state_pop$per_capita <- round(cov_state_pop$Count/cov_state_pop$Population*100000,2)
        cov_state_pop <- filter(cov_state_pop, !is.na(per_capita))
        
        #ref: https://learn.r-journalism.com/en/mapping/census_maps/census-maps/
        #downloading the shapefiles for states at the lowest resolution
        states <- states(cb=T)
        
        #use the Tigris function geo_join to bring together the states shapefiles and the us df
        states_merged_cov <- geo_join(states, cov_state_pop, "STUSPS", "ST") 
        
        #create a color palette based on the number range in the per_capita column
        pal <- colorNumeric("YlOrRd", domain=states_merged_cov$per_capita)
        
        #Using the Base R method of filtering subset() to get rid of NA values because 
        #we're dealing with a SpatialPolygonsDataFrame and not a normal dataframe, 
        #thus filter() wouldn't work
        states_merged_cov <- subset(states_merged_cov, !is.na(Count))
        
        m_labels <- paste("State: ", states_merged_cov$State, "<br/>",
                          "Residents: ", states_merged_cov$Population, "<br/>",
                          "Case Type: ", rv$case, "<br/>",
                          "Case Count: ", states_merged_cov$Count, "<br/>",
                          "Per Capita: ", states_merged_cov$per_capita) %>%
            lapply(htmltools::HTML)
        
        leaflet() %>%
            addProviderTiles("CartoDB.Positron") %>%
            setView(-98.483330, 38.712046, zoom = 4) %>% 
            addPolygons(data = states_merged_cov,
                        fillColor = ~pal(states_merged_cov$per_capita),
                        fillOpacity = 0.7, weight = 0.2,
                        smoothFactor = 0.2,
                        label=m_labels, 
                        labelOptions = labelOptions(style=list("font-weight" = "normal",
                                                               padding = "3px 8px"),
                                                    textsize = "13px", direction = "auto")) %>%
            addLegend(pal=pal,
                      values = states_merged_cov$per_capita,
                      title = "Case per Capita",
                      position = "bottomright")
        
    })
    
    # -------------------------- Menu2: Countries --------------------------- #
    
    rv2 <- reactiveValues(data = ts %>% 
                              filter(Type == 'Confirmed') %>%
                              arrange(Date, Country), 
                          case = "Confirmed")
    
    trigger <- reactive({list(input$ts_Name, input$ts_Date, input$ts_Cases)})
    
    observeEvent(trigger(), {
        rv2$data <- ts %>% 
            filter(Country %in% input$ts_Name,
                   Type == input$ts_Cases,
                   Date >= min(input$ts_Date) & Date <= max(input$ts_Date))
    })
    
    observeEvent(input$ts_Cases, {
        rv2$case <- input$ts_Cases
    })
    
    output$ts_Table <- renderDataTable(
        #rv2$data$Count <- round(rv2$data$Count, 0)
        DT::datatable(rv2$data %>%
                          mutate(Date = format(Date, '%Y-%m')) %>%
                          group_by(Country, Date, Type) %>%
                          summarise(Count = sum(Count)), 
                      escape=FALSE, 
                      options = list(paging=FALSE, autoWidth=TRUE, scrollX=TRUE))
    )
    
    output$ts_Chart_Title <- renderText(paste(rv2$case, ' Cases By Country (Top 10)'))
    
    output$ts_Chart <- renderPlotly({
        plot_ly(rv2$data %>%
                    mutate(Date = format(Date, '%Y-%m')) %>%
                    group_by(Country, Date) %>%
                    summarise(Count = sum(Count)), 
                x=~Date, 
                y=~Count,
                color=~Country,
                width=900,
                height=650) %>%
            add_lines() %>%
            layout(yaxis = list(title = 'Monthly Count'),
                   xaxis = list(title = 'Year-Month'),
                   legend = list(orientation = 'h', y=-0.15))
        
    })
    
    
    
    # -------------------------- Menu3: Worldwide --------------------------- #
    
    output$ww_box_Confirmed <- renderValueBox({
        valueBox(
            paste0(ww %>% filter(Date=='2021-05-16', Type=='Confirmed') %>% .$Total), 
            "# of Confirmed Cases (last updated: 2021-05-16)", icon = icon("hospital"), color = "yellow"
        )
    })
    
    output$ww_box_Recovered <- renderValueBox({
        valueBox(
            paste0(ww %>% filter(Date=='2021-05-16', Type=='Recovered') %>% .$Total), 
            "# of Recovered Cases (last updated: 2021-05-16)", icon = icon("heart"), color = "green"
        )
    })
    
    output$ww_box_Deaths <- renderValueBox({
        valueBox(
            paste0(ww %>% filter(Date=='2021-05-16', Type=='Deaths') %>% .$Total), 
            "# of Deaths Cases (last updated: 2021-05-16)", icon = icon("user"), color = "red"
        )
    })
    
    output$ww_Chart <- renderPlotly({
        plot_ly(ww, 
                x=~Date, 
                y=~Total,
                color=~Type,
                colors = c('darkorange1', 'firebrick2', 'forestgreen'),
                width=1500,
                height=650) %>%
            add_lines() %>%
            layout(yaxis = list(title = 'Number of Cumulative Cases'),
                   xaxis = list(title = 'Date'),
                   legend = list(x=0.1, y=0.8))
        
    })
    
    # -------------------------- Menu4: About --------------------------- #
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)