# load the required libraries
library(shiny)
library(leaflet)
library(sf)
library(lubridate)
library(tidyverse)  # added by Aldo
library(rsconnect)  # added by Maggie
library(ggplot2)    # added by Maggie

# global settings

#setwd("~/Documents/Notre Dame/Fall 2020/Data Visualization/DV Final Project/DV Final Project Shiny Code")
#setwd("C:/Users/heman/OneDrive/Desktop/degree/4 sem/data visualization/final_project/DV Final Project Shiny Code/")
#### <Hemanth's code>

# Read in and format dataset
city <- st_read("City_Council_Districts.shp", stringsAsFactors = FALSE)
cases <- read.csv("mod_Code_Enforcement_Cases.csv", stringsAsFactors = FALSE) #2020/11/17 Hemanth change
colnames(cases)[7] = "Case_Date"
cases$Case_Date <- mdy_hm(as.character(cases$Case_Date)) #2020/11/17 Hemanth change
city$Dist <- as.numeric(city$Dist)
# Create spatial data
cases.spatial <- cases %>% #projecting the table as an sf and setting the coordinate system
    st_as_sf(coords = c("Lon","Lat")) %>% 
    st_set_crs(value = 4326)
# Create the pop up label content on spatial map for districts
cases.spatial$popup <- paste("<b>CaseNumber",cases.spatial$Case_Number,"</b><br>",
                             "CaseType: ",cases.spatial$Case_Type_Code_Description,"<br>",
                             "DateReported: ",cases.spatial$Case_Date)
# Create a color palette for case types
pala <- colorFactor(palette = "Set1", domain = cases.spatial$Case_Type_Code_Description)
# Create a color palette for city districts
palb <- colorFactor(c("#2ca25f","#8856a7","#43a2ca","#2c7fb8","#e9a3c9"), domain = city$Dist )
labels <- paste( "<b> Council: ", city$Council_Me, "</b><br>",
                 "Email: ", city$Email )

##### </Hemanth's code>

##### <Aldo's code>

# load data
abandoned_dat <- st_read("Abandoned_Property_Parcels.shp", stringsAsFactors = FALSE )
park_dat <- read.csv("Parks_Locations_and_Features.csv",stringsAsFactors = FALSE)
# create park spatial data
park_spat <- park_dat %>% 
    st_as_sf(coords = c("Lon","Lat")) %>% 
    st_set_crs(value = 4326)
# Change the name of this park to match what is shown in online maps
park_spat$Park_Name <- park_spat$Park_Name %>% 
    str_replace("Martin Luther King Jr.","Martin Luther King Jr. Community Center")


# replace NA with a space in Direction
abandoned_dat$Direction <- abandoned_dat$Direction %>% 
    replace_na("")


##### </Aldo's code>

#### <Maggie's code>

public <- read_csv("Public_Facilities.csv")

abandoned_dat$Outcome_St  <- abandoned_dat$Outcome_St %>% replace_na("Status Unknown")
# remove lat/lon from ADDR1
public$POPL_ADDR1 <- str_replace(public$POPL_ADDR1,pattern = "\\(.*", "")
#### </Maggie's code>


# Define UI for application
ui <- fluidPage(
    # Application title
    ##### <Hemanth's code>
    titlePanel("City of South Bend Data Vizualization Tool"),
    #### <Maggie's code>
#    titlePanel("South Bend Public Facilities"),    @Maggie: Removed this
#    leafletOutput("mymap3"),                       @Maggie: Replaced plotOutput("distPlot") with leafletOutput("mymap3") below
    #### </Maggie's code>
    
    
    # Sidebar with Input and Output functions
    sidebarLayout(
        sidebarPanel(
            # Sidepanel with selectInput_Functions,
            selectInput(inputId = "Case_Type",
                        label = "Violation Type",
                        choices = c("All",sort(unique(cases.spatial$Case_Type_Code_Description))),
                        selected = "All"
                        ),
            dateRangeInput(inputId = "dates",
                           label = "Date Range", 
                           startview = "year", 
                           start = Sys.Date() %m-% months(12), #2020/11/17 Hemanth change
                           end = Sys.Date()                    #2020/11/17 Hemanth change
                           ),
            ##### </Hemanth's code>
            
            ##### <Aldo's code>
            hr(),

            #### <Maggie's code>
            checkboxGroupInput("checkbox", "Abandoned Property Types",
                                choices = unique(abandoned_dat$Outcome_St)),
            #### </Maggie's code>
            #### </Rob's code>
 hr(),
 
            checkboxGroupInput( inputId = "parkTypes",
                                label = "Park Types",
                                choices = sort( unique(park_spat$Park_Type) ),
                                selected = c( "Block Park", "Community Park", "Neighborhood Park" )
                              )
            #### </Rob's code>  
        ),
        # *Output_Functions
        # Show a Spatial Map of generated distribution
        mainPanel(
            tabsetPanel(type = "tabs",
                        ##### <Hemanth's code>
                        tabPanel(title = "Districts & Code Enforcement Cases",
                                 leafletOutput("mymap"),
                                 tags$p("This tab allows exploration of city council districts by showing the  
                                        boundaries of each district as well as the name and email address of  
                                        the council person.  It also allows plotting of different types
                                        of code violations."),
                                 tags$b("To use - please select Violation Type and Date Range, 
                                        then navigate and zoom on the map above. 
                                        Color-coded elements of the map are clickable.")),
                        ##### </Hemanth's code>
                        
                        ##### <Aldo's code>
                        tabPanel(title = "Parks & Abandoned Properties",
                                 leafletOutput("mymap2"),
                                 tags$p("This tab allows exploration of various park types in the city. 
                                        It also enables plotting of different categories of abandoned
                                        properties, which might be reimagined as parks or urban green spaces."),
                                 tags$b("To use - please select options from Abandoned Property Types and 
                                        Park Types on the left, then navigate and zoom on the map above. 
                                        Color-coded elements of the map are clickable.")),
                        ##### </Aldo's code>

                        ##### <Maggie's code>
                        tabPanel(title = "Facilities & Abandoned Properties",
                                 leafletOutput("mymap3"),
                                 tags$p("This tab allows exploration of public facilities in the city. 
                                        It also enables plotting of different categories of abandoned 
                                        properties, which might be repurposed as new public facilities."),
                                 tags$b("To use - please select options from Abandoned Property Types, 
                                        then navigate and zoom on the map above. 
                                        Color-coded elements of the map are clickable.")),
                        ##### </Maggie's code>
                        
                        ##### </Rob's code>
                        tabPanel(title = "Parks & Population",
                                 leafletOutput("mymap4"),
                                 tags$p("This tab allows exploration of park locations within census tracts 
                                        that are colored to reflect population density.  This could be used 
                                        to identify areas where certain populations are underserved by park 
                                        locations."),
                                 tags$b("To use - please select options from Park Types, 
                                        then navigate and zoom on the map above. 
                                        Color-coded elements of the map are clickable."))
                        ##### </Rob's code>
                        )#end tabset panel
            ),
        position = "left"
    ) 
)

# Define server logic required
server <- function(input, output) {
    
    ##### <Hemanth's code>
    data <- reactive(
#        c(input$dates,input$Case_Type),
        {
            if(input$Case_Type == "All"){
                a <- cases.spatial %>% filter(
                    Case_Date >= input$dates[1],
                    Case_Date <= input$dates[2]
                )
                #print(nrow(a))
                return(a)
            }else{
                a <- cases.spatial %>% filter(
                    Case_Date >= input$dates[1],
                    Case_Date <= input$dates[2] &
                        Case_Type_Code_Description == input$Case_Type
                )
                #print(nrow(a))
                return(a)
            }
    })
    output$mymap <- renderLeaflet({
        leaflet( city ) %>%
            addTiles() %>%
            addPolygons( fillColor = ~palb( Dist ),
                         weight = 2,
                         opacity = 0.3,
                         color = "white",
                         dashArray = 3,
                         fillOpacity = 0.7,
                         highlight = highlightOptions(
                             weight = 5,
                             color = "#090",
                             dashArray = "",
                             fillOpacity = 0.7,
                             bringToFront = T ),
                         popup = labels
                         ) %>%
            addCircleMarkers( data = data(), 
                              popup = ~popup, 
                              color = ~pala(Case_Type_Code_Description),
                              clusterOptions = T,
                              stroke = FALSE, 
                              fillOpacity = 0.7, 
                              radius = 5
                              ) %>% 
            addLegend( pal = palb,
                       values = ~city$Dist,
                       opacity = 0.7,
                       title = "Districts",
                       position = "bottomright" 
                       )
    }) 
    ##### </Hemanth's code>
    
    ##### <Aldo's code>
    parks_ab <- reactive({
        if(length(input$checkbox)!=0){
            
            abandoned2 <- abandoned_dat %>% filter(Outcome_St %in% input$checkbox)
            parks2 <- park_spat %>% filter(Park_Type %in% input$parkTypes)
            label1 <- paste( "<b>Address: </b>", abandoned2$Address_Nu,
                             abandoned2$Direction,
                             abandoned2$Street_Nam,
                             abandoned2$Suffix,"<br>",
                             "<b>Structures: </b>", abandoned2$Structures,"<br>",
                             "<b>Status: </b>",abandoned2$Outcome_St)
            label2 <- paste(parks2$Park_Name,"<br>",
                            parks2$Address,"<br>",
                            parks2$Park_Type)
            
            pal2 <- colorFactor(palette = 'Set1', domain =parks2$Park_Types)
            
            leaflet(abandoned_dat%>%filter(Outcome_St %in% input$checkbox)) %>% 
                addTiles() %>% 
                addPolygons(popup = label1) %>%
                addCircleMarkers(data = parks2,
                                 popup = label2,
                                 color = ~pal2(Park_Type),
                                 stroke = 0,
                                 fillOpacity = 0.8,
                                 radius = 8) %>% 
                addLegend(pal = pal2,
                          values = parks2$Park_Type,
                          opacity = 0.8,
                          title = "Park Types",
                          position = "bottomright")
            
        } else {
            parks2 <- park_spat %>% filter(Park_Type %in% input$parkTypes)
            label2 <- paste(parks2$Park_Name,"<br>",
                            parks2$Address,"<br>",
                            parks2$Park_Type)
            
            pal2 <- colorFactor(palette = 'Set1', domain =parks2$Park_Types)
            
            leaflet(abandoned_dat) %>% 
                addTiles() %>% 
                addCircleMarkers(data = parks2,
                                 popup = label2,
                                 color = ~pal2(Park_Type),
                                 stroke = 0,
                                 fillOpacity = 0.8,
                                 radius = 8) %>% 
                addLegend(pal = pal2,
                          values = parks2$Park_Type,
                          opacity = 0.8,
                          title = "Park Types",
                          position = "bottomright")}

    })
    output$mymap2 <-  renderLeaflet({
        parks_ab()
    })    
    ##### </Aldo's code>
    
    ##### <Maggie's code>
    display <- reactive({
        
        # if (input$checkbox){
        
        if(length(input$checkbox)!=0){
            
            abandoned2 <- abandoned_dat %>% filter(Outcome_St %in% input$checkbox)
            
            label1 <- paste( "<b>Address: </b>", abandoned2$Address_Nu,
                             abandoned2$Direction,
                             abandoned2$Street_Nam,
                             abandoned2$Suffix,"<br>",
                             "<b>Structures: </b>", abandoned2$Structures,"<br>",
                             "<b>Status: </b>",abandoned2$Outcome_St)
            pal2 <- colorFactor(palette = 'Set1', domain =public$POPL_TYPE)
            label2 <- paste(public$POPL_NAME,"<br>",
                            public$POPL_ADDR1,"<br>",
                            public$POPL_TYPE)
            
            
            # #########      
            # abandoned_dat %>% filter(Outcome_St %in% input$checkbox)  
            
            
            leaflet(abandoned_dat%>%filter(Outcome_St %in% input$checkbox)) %>% 
                addTiles() %>% 
                addPolygons(popup = label1) %>%
                addCircleMarkers( data = public,
                                  popup = label2,
                                  color = ~pal2(POPL_TYPE),
                                  stroke = 0,
                                  fillOpacity = 1,
                                  radius = 8) %>% 
                addLegend( pal = pal2,
                           values = public$POPL_TYPE,
                           opacity = 0.7,
                           title = "Public Facility Types",
                           position = "bottomright" )
            
            
        } else {
            pal2 <- colorFactor(palette = 'Set1', domain =public$POPL_TYPE)
            label2 <- paste(public$POPL_NAME,"<br>",
                            public$POPL_ADDR1,"<br>",
                            public$POPL_TYPE)
            
            
            leaflet(abandoned_dat) %>% 
                addTiles() %>% 
                addCircleMarkers( data = public,
                                  popup = label2,
                                  color = ~pal2(POPL_TYPE),
                                  stroke = 0,
                                  fillOpacity = 1,
                                  radius = 8) %>% 
                addLegend( pal = pal2,
                           values = public$POPL_TYPE,
                           opacity = 0.7,
                           title = "Public Facility Types",
                           position = "bottomright" )
            
        }
    })
    output$mymap3 <-  renderLeaflet({
        display()
    })
    ##### </Maggie's code>
    ##### </Rob's code>
    
    # Global Variables (I use Aldo's park_dat & park_spat variables as well)
    cens_dat <- st_read( "2010_CensusData.shp" )
    
    # Assign palette and label construction
    pal <- colorBin( "YlOrRd", domain = cens_dat$SE_T001_00 )
    labels_rc <- paste( "<b>", cens_dat$NAMELSAD, "</b><br>",
                     "Population: ", cens_dat$SE_T001_00 )
    
    # Reactive Leaflet Creation
    parks_pop <- reactive( {
        leaflet( cens_dat ) %>% 
            addTiles() %>% 
            addPolygons( fillColor = ~pal( SE_T001_00 ),
                         weight = 2,
                         opacity = 0.3,
                         color = "white",
                         dashArray = 3,
                         fillOpacity = 0.7,
                         highlight = highlightOptions(
                             weight = 5,
                             color = "#090",
                             dashArray = "",
                             fillOpacity = 0.7,
                             bringToFront = T ),
                         popup = labels_rc
            ) %>%
            addMarkers( data = park_spat %>% filter(Park_Type %in% input$parkTypes),
                        popup = ~Park_Name ) %>% 
            addLegend( pal = pal,
                       values = ~SE_T001_00,
                       opacity = 0.7,
                       title = "Total Population",
                       position = "bottomright" )
                             } )
    
    # Reactive Leaflet Assignment to UI
    output$mymap4 <- renderLeaflet( { parks_pop() } )
    
    ##### </Rob's code>
}

# Run the application 
shinyApp(ui = ui, server = server)