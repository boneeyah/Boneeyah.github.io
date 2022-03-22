library(shiny)
library(shinyWidgets)
library(dplyr)
library(tidyr)
library(tidyverse)
library(mapproj)
#reading in the csvs
brewery = read.csv("breweries.csv") #read breweries.csv
beer = read.csv("beers.csv") #read beers.csv
#renaming columns for the join to the beer dataset
brewery=rename(brewery, Brewery_id=Brew_ID, Brewery=Name)
brewery$State <- trimws(brewery$State, which = c("left"))#added to remove space before state
#pulling ids with inaccurate info
Brewery_id<-as.integer(c(96,415,167, 262, 139))
error_brew_ids<-as.data.frame(Brewery_id)
#joining to get Brewery Names
brew_filter = merge(x=error_brew_ids,y=brewery,by="Brewery_id") %>% select(Brewery_id, Brewery)
#joining on brewery name to pull in correct columns--filter out inaccurate
brew_filter = merge(x=brewery,y=brew_filter,by="Brewery") %>% mutate(type = case_when( Brewery_id.x==Brewery_id.y ~ "correct",TRUE~  "wrong"))
#filtering down to rows with errors
brew_replace =brew_filter %>%filter(type == "wrong") %>% select(Brewery_id.y, Brewery, City, State)%>% rename(Brewery_id=Brewery_id.y)
#removing errors and replacing with updated df
brewery = anti_join(x=brewery,y=error_brew_ids,by="Brewery_id")
brewery<-rbind(brewery, brew_replace)
brewery$City = str_replace(brewery$City, "Menominie", "Menomonie")
#join between brewery and beer data
beer_brewery = merge(x=brewery,y=beer,by="Brewery_id")
#print first 6 rows
head(beer_brewery)
#print last 6 rows
tail(beer_brewery)
#3.  Address the missing values in each column.
#filtering of NAs for ABV and IBU-- rows drop from 2410 down to 1405 for this filtering
#only use this filtered dataset for the questions related to ABV/IBU#only use this filtered dataset for the questions related to ABV/IBU
clean_beer_brewery <- beer_brewery %>% filter_at(vars(ABV,IBU),all_vars(!is.na(.)))

#question 1
#How many breweries are present in each state? (counties heatmap)
#load libraries
library(ggplot2)
library(maps)
#library(tidyverse)#added to 1st chunk
library(ggthemes)
library(kableExtra)
#load counties table
cnty <- read.csv("cty-cnty.csv")
colnames(cnty)[3] <- "region"
#load population data 2021
pop_est <- read.csv("NST-EST2021-alldata.csv")
pop_est <- pop_est %>% select(NAME,POPESTIMATE2021) %>% rename(region=NAME, "Pop2021"=POPESTIMATE2021) %>% mutate(region=tolower(region))
pop_est <- pop_est[-c(1:5),]
pop_est$region %>% str_replace("District of Columbia", "washington, d.c.")
pop_est$rank <- rank(-pop_est$Pop2021)
#wrangle data
#brewery$State <- trimws(brewery$State, which = c("left"))
cnty <- cnty %>% select("City", "State","County","region")#select columns
cnty <- cnty[!duplicated(cnty),]
#left join brew and cnty
brewcomb <- merge(brewery, select(cnty, c("City", "State", "County","region")), by=c("City","State"))
brewcomb <- brewcomb %>% distinct(Brewery_id, .keep_all = TRUE)
#1 table wtih state count, map with state count and map with county count
#state and county tables
us_states <- map_data("state")
us_counties <- map_data("county")

#load and wrangle data
beer <- read.csv("beers.csv")
brew <- read.csv("breweries.csv", col.names = c("Brewery_id", "Name", "City", "State"))

beer <- merge(beer,brew, by = "Brewery_id")
beer <- beer[,c(1,3,4,5,10)]
beer$State <- trimws(beer$State,"left")

#state brewery count
brew_state <- brewcomb %>% mutate(region=tolower(region)) %>%
  group_by(region) %>% count(region)
brew_state <- brew_state %>% left_join(pop_est,by="region") %>% arrange(desc(n))
brew_state <- brew_state %>% mutate(region2=str_to_title(region),Pop2021=round(Pop2021/1000000,2))

#county brewery count
brew_county <- brewcomb %>% mutate(subregion=tolower(County)) %>% 
  group_by(subregion) %>% count(subregion)

ui <- fluidPage(
  
  # App title ----
  titlePanel("Case Study 1"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      conditionalPanel(condition = "input.tabs == 'Plot'",
                       #select hist or box
                       radioButtons("radio", label = "Radio buttons",
                                    choices = list("Histogram" = "Histogram", "Boxplot" = "Boxplot"), 
                                    selected = "Histogram"),
                       conditionalPanel(condition = "input.radio == 'Histogram'", sliderInput(inputId = "bins",
                                                                                              label = "Number of bins:",
                                                                                              min = 1,
                                                                                              max = 50,
                                                                                              value = 30)),
                       #drop box selection of input
                       selectInput("select", label = "ABV or IBU",
                                   choices = list("ABV" = "ABV", "IBU" = "IBU"),
                                   selected = 1),
                       
                       #select state
                       pickerInput("State", label = "State", 
                                   choices = unique(sort(beer$State)), 
                                   selected = unique(sort(beer$State)),
                                   multiple = TRUE,
                                   options = list(`actions-box`= TRUE)),
                       
                       #Input: Slider for the number of bins ----
                       
                       radioButtons("regg", label = "Show Regression Line?",
                                    choices = list("Yes" = "Yes", "No" = "No"),
                                    selected = "No"),
                       hr(),
                       fluidRow(column(3, verbatimTextOutput("value")))
      ),
      conditionalPanel(condition = "input.tabs =='Map'",
                       radioButtons("mapradio", label = "Map Type",
                                    choices = list("State" = "State", "County"="County"),
                                    selected = "State"))
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      tabsetPanel(id = "tabs",
                  tabPanel("Plot",
                           # Output: Histogram ----
                           plotOutput(outputId = "distPlot"),
                           plotOutput(outputId = "distPlot2")),
                  tabPanel("Map",
                           plotOutput(outputId = "plotmap"))
      )
    )
  )
)
# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$distPlot <- renderPlot({
    beer2 <- beer[beer$State %in% input$State,]
    
    if(input$radio == "Histogram")
    {
      if(input$select == "ABV")
      {
        x    <- beer2$ABV[!is.na(beer2$ABV)]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)
        
        p1 <- hist(x, breaks = bins, col = "#75AADB", border = "white",
                   xlab = "Alcohol by Volume (ABV)",
                   main = "Histogram of ABV")
      }
      if(input$select == "IBU")
      {
        x    <- beer2$IBU[!is.na(beer2$IBU)]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)
        
        p1 <- hist(x, breaks = bins, col = "#75AADB", border = "white",
                   xlab = "International Bitterness Units (IBU)",
                   main = "Histogram of IBU")
      } 
    }
    if(input$radio == "Boxplot")
    {
      if(input$select == "ABV")
      {
        x <- beer2$ABV[!is.na(beer2$ABV)]
        
        p1 <- boxplot(x, col = "#75AADB", border = "black",
                      xlab = "Alcohol by Volume (ABV)",
                      main = "Boxplot of ABV")
      }
      if(input$select == "IBU")
      {
        x <- beer2$IBU[!is.na(beer2$IBU)]
        
        p1 <- boxplot(x, col = "#75AADB", border = "black",
                      xlab = "International Bitterness Units (IBU)",
                      main = "Boxplot of IBU")
      }
    }
  })
  output$distPlot2 <- renderPlot({
    if(input$regg == "No"){
      beer2 <- beer[beer$State %in% input$State,]
      plot(beer2$ABV,beer2$IBU,
           xlab = "ABV",
           ylab = "IBU",
           main = "Scatterplot IBU vs ABV",
           col = "#75AADB",
           pch = 16)
    }
    if(input$regg == "Yes"){
      beer2 <- beer[beer$State %in% input$State,]
      fit <- lm(IBU~ABV,data = beer2)
      plot(beer2$ABV,beer2$IBU,
           xlab = "ABV",
           ylab = "IBU",
           main = "Scatterplot IBU vs ABV",
           col = "#75AADB",
           pch = 16)
      abline(fit$coefficients[1],fit$coefficients[2], lwd = 2, col = "#DB8F8A")
      mtext(bquote(hat(Y) == .(fit$coefficients[1]) + .(fit$coefficients[2])*(ABV)),
            side = 3,
            line = -2,
            adj = 0,
            cex = 1.25,
            col = "#8F504C")
    }
  })
  output$plotmap <- renderPlot({
    if(input$mapradio == "State"){
      #state gradient map
      us_states$region2 <- str_to_title(us_states$region)
      us_states1 <-  us_states %>% left_join(brew_state,by=("region2"))
      us_states1 %>% ggplot(aes(x=long,y=lat,group=group,fill=n))+
        geom_polygon(color = "gray90", size=.1)+
        coord_map(projection = "albers", lat0=45, lat1=55)+
        scale_fill_viridis_c()+
        theme(axis.line = element_blank(),
              axis.text = element_blank(),
              axis.ticks = element_blank(),
              panel.background = element_blank(),
              axis.title = element_blank())+
        labs(fill="Brewery\nCount")+
        ggtitle("Breweries by State\nContinental US")
    } else if(input$mapradio == "County"){
      #county gradient map
      us_counties1 <-  us_counties %>% left_join(brew_county,by=("subregion"))
      us_counties1 %>% ggplot(aes(x=long,y=lat,group=group,fill=n))+
        geom_polygon(color = "gray90", size=.1)+
        coord_map(projection = "albers", lat0=45, lat1=55)+
        scale_fill_viridis_c()+
        theme(axis.line = element_blank(),
              axis.text = element_blank(),
              axis.ticks = element_blank(),
              panel.background = element_blank(),
              axis.title = element_blank())+
        labs(fill="Brewery\nCount")+
        ggtitle("Breweries by State\nContinental US")
    }
  })
  
}
shinyApp(ui, server)