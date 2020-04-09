#load library
library(shiny)
library(readr)   # Read Data into R
library(readxl)  # Read Excel Data into R
library(skimr)   # skim(datatsets)
library(dplyr)   # Recoding
library(janitor) # Cleans Datasets
library(stringr) #
library(ggplot2)
library(plotly)
library(scales)

# Import COVID19 Dataset From - https://github.com/nytimes/covid-19-data/blob/master/us-states.csv
urlfileNYTimes = "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv"
COVID19_USdata_NYTimes = read_csv(url(urlfileNYTimes))

for(i in 1:nrow(COVID19_USdata_NYTimes)){
  COVID19_USdata_NYTimes[i,6] = as.numeric(COVID19_USdata_NYTimes[i,1])
}

USdataFinal = data.frame()

for(i in 1:(max(COVID19_USdata_NYTimes[,6])-(18282-1))){
  Temp = COVID19_USdata_NYTimes %>%
    filter( V6 == str_c(18282-1+i))
  
  Date = Temp %>%
    select(date) 
  Date = Date[1,1]
  
  Cases = Temp %>%
    select(cases)
  Cases = sum(Cases)
  
  Deaths = Temp %>%
    select(deaths)
  Deaths = sum(Deaths)
  
  USdata = cbind(Date, Cases, Deaths)
  
  USdataFinal = rbind(USdataFinal, USdata)
}


# Read in a Column that Lists US States
#USStates = read_xlsx("USStates.xlsx", col_names = FALSE)
#USStatesFull = rbind(USStates, "District of Columbia", "Guam", "Puerto Rico")


ui = fluidPage(
  
  # App Title
  titlePanel("COVID19 Cases & Deaths in US"),
  
  # Sidebar Layouts 
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput(inputId="YaxisScale",label="Choose Y-axis Scale",choices = c("Log"="Log","Linear"="Linear"),
                  selected = "Log",multiple = F),
      
      selectInput(inputId="USState",label="Choose US State",choices = c("United States"="United States",
                                                                        "Alabama"="Alabama",
                                                                        "Alaska"="Alaska",
                                                                        "Arizona"="Arizona",
                                                                        "Arkansas"="Arkansas",
                                                                        "California"="California",
                                                                        "Colorado"="Colorado",
                                                                        "Connecticut"="Connecticut",
                                                                        "Delaware"="Delaware",
                                                                        "Florida"="Florida",
                                                                        "Georgia"="Georgia",
                                                                        "Hawaii"="Hawaii",
                                                                        "Idaho"="Idaho",
                                                                        "Illinois"="Illinois",
                                                                        "Indiana"="Indiana",
                                                                        "Iowa"="Iowa",
                                                                        "Kansas"="Kansas",
                                                                        "Kentucky"="Kentucky",
                                                                        "Louisiana"="Louisiana",
                                                                        "Maine"="Maine",
                                                                        "Maryland"="Maryland",
                                                                        "Massachusetts"="Massachusetts",
                                                                        "Michigan"="Michigan",
                                                                        "Minnesota"="Minnesota",
                                                                        "Mississippi"="Mississippi",
                                                                        "Missouri"="Missouri",
                                                                        "Montana"="Montana",
                                                                        "Nebraska"="Nebraska",
                                                                        "Nevada"="Nevada",
                                                                        "New Hampshire"="New Hampshire",
                                                                        "New Jersey"="New Jersey",
                                                                        "New Mexico"="New Mexico",
                                                                        "New York"="New York",
                                                                        "North Carolina"="North Carolina",
                                                                        "North Dakota"="North Dakota",
                                                                        "Ohio"="Ohio",
                                                                        "Oklahoma"="Oklahoma",
                                                                        "Oregon"="Oregon",
                                                                        "Pennsylvania"="Pennsylvania",
                                                                        "Rhode Island"="Rhode Island",
                                                                        "South Carolina"="South Carolina",
                                                                        "South Dakota"="South Dakota",
                                                                        "Tennessee"="Tennessee",
                                                                        "Texas"="Texas",
                                                                        "Utah"="Utah",
                                                                        "Vermont"="Vermont",
                                                                        "Virginia"="Virginia",
                                                                        "Washington"="Washington",
                                                                        "West Virginia"="West Virginia",
                                                                        "Wisconsin"="Wisconsin",
                                                                        "Wyoming"="Wyoming",
                                                                        "District of Columbia"="District of Columbia",
                                                                        "Guam"="Guam",
                                                                        "Puerto Rico"="Puerto Rico"),
                  
                  selected = "United States",multiple = F),
      
      strong("Blue = ",span("Cases", style = "color:blue"),"&  "),
      strong("Red = ", span("Deaths", style = "color:red"))
      
    ),
    
    mainPanel(
      
      # Output
      plotlyOutput(outputId = "LinePlot")
      
      
    )
    
  )
)

server = function(input, output){
  
  # Data Visualization
  
  
  
  output$LinePlot = renderPlotly({
    
    if(input$YaxisScale=="Log"){
      YaxisScale = scale_y_log10(labels = comma, n.breaks = 10)
    }else if(input$YaxisScale=="Linear"){
      YaxisScale = scale_y_continuous(labels = comma, n.breaks = 10)
    }
    
    if(input$USState=="United States"){
      
      plot1 = ggplotly(ggplot(data = USdataFinal) + 
                         geom_point(aes(x = date, y = Cases), color = "blue", size = 2) +
                         geom_point(aes(x = date, y = Deaths), color = "red", size = 2) +
                         xlab("Date") +
                         ylab("Cases & Deaths in United States") +
                         theme(text = element_text(size=14, face="bold")) +
                         YaxisScale)
      
    }else if(input$USState=="Alabama"){
      
      COVID19_US_Cases = COVID19_USdata_NYTimes %>%
        filter(state == "Alabama") %>%
        select(everything(), -fips)
      
      plot1 = ggplotly(ggplot(data = COVID19_US_Cases) + 
                         geom_point(aes(x = date, y = cases), color = "blue", size = 2) +
                         geom_point(aes(x = date, y = deaths), color = "red", size = 2) +
                         xlab("Date") +
                         ylab("Cases & Deaths in Alabama") +
                         theme(text = element_text(size=14, face="bold")) +
                         YaxisScale)  
      
    }else if(input$USState=="Alaska"){
      
      COVID19_US_Cases = COVID19_USdata_NYTimes %>%
        filter(state == "Alaska") %>%
        select(everything(), -fips)
      
      plot1 = ggplotly(ggplot(data = COVID19_US_Cases) + 
                         geom_point(aes(x = date, y = cases), color = "blue", size = 2) +
                         geom_point(aes(x = date, y = deaths), color = "red", size = 2) +
                         xlab("Date") +
                         ylab("Cases & Deaths in Alaska") +
                         theme(text = element_text(size=14, face="bold")) +
                         YaxisScale)
      
    }else if(input$USState=="Arizona"){
      
      COVID19_US_Cases = COVID19_USdata_NYTimes %>%
        filter(state == "Arizona") %>%
        select(everything(), -fips)
      
      plot1 = ggplotly(ggplot(data = COVID19_US_Cases) + 
                         geom_point(aes(x = date, y = cases), color = "blue", size = 2) +
                         geom_point(aes(x = date, y = deaths), color = "red", size = 2) +
                         xlab("Date") +
                         ylab("Cases & Deaths in Arizona") +
                         theme(text = element_text(size=14, face="bold")) +
                         YaxisScale)
      
    }else if(input$USState=="Arkansas"){
      
      COVID19_US_Cases = COVID19_USdata_NYTimes %>%
        filter(state == "Arkansas") %>%
        select(everything(), -fips)
      
      plot1 = ggplotly(ggplot(data = COVID19_US_Cases) + 
                         geom_point(aes(x = date, y = cases), color = "blue", size = 2) +
                         geom_point(aes(x = date, y = deaths), color = "red", size = 2) +
                         xlab("Date") +
                         ylab("Cases & Deaths in Arkansas") +
                         theme(text = element_text(size=14, face="bold")) +
                         YaxisScale)
      
    }else if(input$USState=="California"){
      
      COVID19_US_Cases = COVID19_USdata_NYTimes %>%
        filter(state == "California") %>%
        select(everything(), -fips)    
      
      plot1 = ggplotly(ggplot(data = COVID19_US_Cases) + 
                         geom_point(aes(x = date, y = cases), color = "blue", size = 2) +
                         geom_point(aes(x = date, y = deaths), color = "red", size = 2) +
                         xlab("Date") +
                         ylab("Cases & Deaths in California") +
                         theme(text = element_text(size=14, face="bold")) +
                         YaxisScale)
      
    }else if(input$USState=="Colorado"){
      
      COVID19_US_Cases = COVID19_USdata_NYTimes %>%
        filter(state == "Colorado") %>%
        select(everything(), -fips)
      
      plot1 = ggplotly(ggplot(data = COVID19_US_Cases) + 
                         geom_point(aes(x = date, y = cases), color = "blue", size = 2) +
                         geom_point(aes(x = date, y = deaths), color = "red", size = 2) +
                         xlab("Date") +
                         ylab("Cases & Deaths in Colorado") +
                         theme(text = element_text(size=14, face="bold")) +
                         YaxisScale)
      
    }else if(input$USState=="Connecticut"){
      
      COVID19_US_Cases = COVID19_USdata_NYTimes %>%
        filter(state == "Connecticut") %>%
        select(everything(), -fips)
      
      plot1 = ggplotly(ggplot(data = COVID19_US_Cases) + 
                         geom_point(aes(x = date, y = cases), color = "blue", size = 2) +
                         geom_point(aes(x = date, y = deaths), color = "red", size = 2) +
                         xlab("Date") +
                         ylab("Cases & Deaths in Connecticut") +
                         theme(text = element_text(size=14, face="bold")) +
                         YaxisScale)
      
    }else if(input$USState=="Delaware"){
      
      COVID19_US_Cases = COVID19_USdata_NYTimes %>%
        filter(state == "Delaware") %>%
        select(everything(), -fips)
      
      plot1 = ggplotly(ggplot(data = COVID19_US_Cases) + 
                         geom_point(aes(x = date, y = cases), color = "blue", size = 2) +
                         geom_point(aes(x = date, y = deaths), color = "red", size = 2) +
                         xlab("Date") +
                         ylab("Cases & Deaths in Delaware") +
                         theme(text = element_text(size=14, face="bold")) +
                         YaxisScale)
      
    }else if(input$USState=="Florida"){
      
      COVID19_US_Cases = COVID19_USdata_NYTimes %>%
        filter(state == "Florida") %>%
        select(everything(), -fips)
      
      plot1 = ggplotly(ggplot(data = COVID19_US_Cases) + 
                         geom_point(aes(x = date, y = cases), color = "blue", size = 2) +
                         geom_point(aes(x = date, y = deaths), color = "red", size = 2) +
                         xlab("Date") +
                         ylab("Cases & Deaths in Florida") +
                         theme(text = element_text(size=14, face="bold")) +
                         YaxisScale)
      
    }else if(input$USState=="Georgia"){
      
      COVID19_US_Cases = COVID19_USdata_NYTimes %>%
        filter(state == "Georgia") %>%
        select(everything(), -fips)
      
      plot1 = ggplotly(ggplot(data = COVID19_US_Cases) + 
                         geom_point(aes(x = date, y = cases), color = "blue", size = 2) +
                         geom_point(aes(x = date, y = deaths), color = "red", size = 2) +
                         xlab("Date") +
                         ylab("Cases & Deaths in Georgia") +
                         theme(text = element_text(size=14, face="bold")) +
                         YaxisScale)
      
    }else if(input$USState=="Hawaii"){
      
      COVID19_US_Cases = COVID19_USdata_NYTimes %>%
        filter(state == "Hawaii") %>%
        select(everything(), -fips)
      
      plot1 = ggplotly(ggplot(data = COVID19_US_Cases) + 
                         geom_point(aes(x = date, y = cases), color = "blue", size = 2) +
                         geom_point(aes(x = date, y = deaths), color = "red", size = 2) +
                         xlab("Date") +
                         ylab("Cases & Deaths in Hawaii") +
                         theme(text = element_text(size=14, face="bold")) +
                         YaxisScale)
      
    }else if(input$USState=="Idaho"){
      
      COVID19_US_Cases = COVID19_USdata_NYTimes %>%
        filter(state == "Idaho") %>%
        select(everything(), -fips)
      
      plot1 = ggplotly(ggplot(data = COVID19_US_Cases) + 
                         geom_point(aes(x = date, y = cases), color = "blue", size = 2) +
                         geom_point(aes(x = date, y = deaths), color = "red", size = 2) +
                         xlab("Date") +
                         ylab("Cases & Deaths in Idaho") +
                         theme(text = element_text(size=14, face="bold")) +
                         YaxisScale)
      
    }else if(input$USState=="Illinois"){
      
      COVID19_US_Cases = COVID19_USdata_NYTimes %>%
        filter(state == "Illinois") %>%
        select(everything(), -fips)    
      
      plot1 = ggplotly(ggplot(data = COVID19_US_Cases) + 
                         geom_point(aes(x = date, y = cases), color = "blue", size = 2) +
                         geom_point(aes(x = date, y = deaths), color = "red", size = 2) +
                         xlab("Date") +
                         ylab("Cases & Deaths in Illinois") +
                         theme(text = element_text(size=14, face="bold")) +
                         YaxisScale)
      
    }else if(input$USState=="Indiana"){
      
      COVID19_US_Cases = COVID19_USdata_NYTimes %>%
        filter(state == "Indiana") %>%
        select(everything(), -fips)    
      
      plot1 = ggplotly(ggplot(data = COVID19_US_Cases) + 
                         geom_point(aes(x = date, y = cases), color = "blue", size = 2) +
                         geom_point(aes(x = date, y = deaths), color = "red", size = 2) +
                         xlab("Date") +
                         ylab("Cases & Deaths in Indiana") +
                         theme(text = element_text(size=14, face="bold")) +
                         YaxisScale)
      
    }else if(input$USState=="Iowa"){
      
      COVID19_US_Cases = COVID19_USdata_NYTimes %>%
        filter(state == "Iowa") %>%
        select(everything(), -fips)
      
      plot1 = ggplotly(ggplot(data = COVID19_US_Cases) + 
                         geom_point(aes(x = date, y = cases), color = "blue", size = 2) +
                         geom_point(aes(x = date, y = deaths), color = "red", size = 2) +
                         xlab("Date") +
                         ylab("Cases & Deaths in Iowa") +
                         theme(text = element_text(size=14, face="bold")) +
                         YaxisScale)
      
    }else if(input$USState=="Kansas"){
      
      COVID19_US_Cases = COVID19_USdata_NYTimes %>%
        filter(state == "Kansas") %>%
        select(everything(), -fips)
      
      plot1 = ggplotly(ggplot(data = COVID19_US_Cases) + 
                         geom_point(aes(x = date, y = cases), color = "blue", size = 2) +
                         geom_point(aes(x = date, y = deaths), color = "red", size = 2) +
                         xlab("Date") +
                         ylab("Cases & Deaths in Kansas") +
                         theme(text = element_text(size=14, face="bold")) +
                         YaxisScale)
      
    }else if(input$USState=="Kentucky"){
      
      COVID19_US_Cases = COVID19_USdata_NYTimes %>%
        filter(state == "Kentucky") %>%
        select(everything(), -fips)
      
      plot1 = ggplotly(ggplot(data = COVID19_US_Cases) + 
                         geom_point(aes(x = date, y = cases), color = "blue", size = 2) +
                         geom_point(aes(x = date, y = deaths), color = "red", size = 2) +
                         xlab("Date") +
                         ylab("Cases & Deaths in Kentucky") +
                         theme(text = element_text(size=14, face="bold")) +
                         YaxisScale)
      
    }else if(input$USState=="Louisiana"){
      
      COVID19_US_Cases = COVID19_USdata_NYTimes %>%
        filter(state == "Louisiana") %>%
        select(everything(), -fips)
      
      plot1 = ggplotly(ggplot(data = COVID19_US_Cases) + 
                         geom_point(aes(x = date, y = cases), color = "blue", size = 2) +
                         geom_point(aes(x = date, y = deaths), color = "red", size = 2) +
                         xlab("Date") +
                         ylab("Cases & Deaths in Louisiana") +
                         theme(text = element_text(size=14, face="bold")) +
                         YaxisScale)
      
    }else if(input$USState=="Maine"){
      
      COVID19_US_Cases = COVID19_USdata_NYTimes %>%
        filter(state == "Maine") %>%
        select(everything(), -fips)
      
      plot1 = ggplotly(ggplot(data = COVID19_US_Cases) + 
                         geom_point(aes(x = date, y = cases), color = "blue", size = 2) +
                         geom_point(aes(x = date, y = deaths), color = "red", size = 2) +
                         xlab("Date") +
                         ylab("Cases & Deaths in Maine") +
                         theme(text = element_text(size=14, face="bold")) +
                         YaxisScale)
      
    }else if(input$USState=="Maryland"){
      
      COVID19_US_Cases = COVID19_USdata_NYTimes %>%
        filter(state == "Maryland") %>%
        select(everything(), -fips)
      
      plot1 = ggplotly(ggplot(data = COVID19_US_Cases) + 
                         geom_point(aes(x = date, y = cases), color = "blue", size = 2) +
                         geom_point(aes(x = date, y = deaths), color = "red", size = 2) +
                         xlab("Date") +
                         ylab("Cases & Deaths in Maryland") +
                         theme(text = element_text(size=14, face="bold")) +
                         YaxisScale)
      
    }else if(input$USState=="Massachusetts"){
      
      COVID19_US_Cases = COVID19_USdata_NYTimes %>%
        filter(state == "Massachusetts") %>%
        select(everything(), -fips)    
      
      plot1 = ggplotly(ggplot(data = COVID19_US_Cases) + 
                         geom_point(aes(x = date, y = cases), color = "blue", size = 2) +
                         geom_point(aes(x = date, y = deaths), color = "red", size = 2) +
                         xlab("Date") +
                         ylab("Cases & Deaths in Massachusetts") +
                         theme(text = element_text(size=14, face="bold")) +
                         YaxisScale)
      
    }else if(input$USState=="Michigan"){
      
      COVID19_US_Cases = COVID19_USdata_NYTimes %>%
        filter(state == "Michigan") %>%
        select(everything(), -fips)   
      
      plot1 = ggplotly(ggplot(data = COVID19_US_Cases) + 
                         geom_point(aes(x = date, y = cases), color = "blue", size = 2) +
                         geom_point(aes(x = date, y = deaths), color = "red", size = 2) +
                         xlab("Date") +
                         ylab("Cases & Deaths in Michigan") +
                         theme(text = element_text(size=14, face="bold")) +
                         YaxisScale)
      
    }else if(input$USState=="Minnesota"){
      
      COVID19_US_Cases = COVID19_USdata_NYTimes %>%
        filter(state == "Minnesota") %>%
        select(everything(), -fips)
      
      plot1 = ggplotly(ggplot(data = COVID19_US_Cases) + 
                         geom_point(aes(x = date, y = cases), color = "blue", size = 2) +
                         geom_point(aes(x = date, y = deaths), color = "red", size = 2) +
                         xlab("Date") +
                         ylab("Cases & Deaths in Minnesota") +
                         theme(text = element_text(size=14, face="bold")) +
                         YaxisScale)
      
    }else if(input$USState=="Mississippi"){
      
      COVID19_US_Cases = COVID19_USdata_NYTimes %>%
        filter(state == "Mississippi") %>%
        select(everything(), -fips)
      
      plot1 = ggplotly(ggplot(data = COVID19_US_Cases) + 
                         geom_point(aes(x = date, y = cases), color = "blue", size = 2) +
                         geom_point(aes(x = date, y = deaths), color = "red", size = 2) +
                         xlab("Date") +
                         ylab("Cases & Deaths in Mississippi") +
                         theme(text = element_text(size=14, face="bold")) +
                         YaxisScale)
      
    }else if(input$USState=="Missouri"){
      
      COVID19_US_Cases = COVID19_USdata_NYTimes %>%
        filter(state == "Missouri") %>%
        select(everything(), -fips)    
      
      plot1 = ggplotly(ggplot(data = COVID19_US_Cases) + 
                         geom_point(aes(x = date, y = cases), color = "blue", size = 2) +
                         geom_point(aes(x = date, y = deaths), color = "red", size = 2) +
                         xlab("Date") +
                         ylab("Cases & Deaths in Missouri") +
                         theme(text = element_text(size=14, face="bold")) +
                         YaxisScale)
      
    }else if(input$USState=="Montana"){
      
      COVID19_US_Cases = COVID19_USdata_NYTimes %>%
        filter(state == "Montana") %>%
        select(everything(), -fips)
      
      plot1 = ggplotly(ggplot(data = COVID19_US_Cases) + 
                         geom_point(aes(x = date, y = cases), color = "blue", size = 2) +
                         geom_point(aes(x = date, y = deaths), color = "red", size = 2) +
                         xlab("Date") +
                         ylab("Cases & Deaths in Montana") +
                         theme(text = element_text(size=14, face="bold")) +
                         YaxisScale)
      
    }else if(input$USState=="Nebraska"){
      
      COVID19_US_Cases = COVID19_USdata_NYTimes %>%
        filter(state == "Nebraska") %>%
        select(everything(), -fips)
      
      plot1 = ggplotly(ggplot(data = COVID19_US_Cases) + 
                         geom_point(aes(x = date, y = cases), color = "blue", size = 2) +
                         geom_point(aes(x = date, y = deaths), color = "red", size = 2) +
                         xlab("Date") +
                         ylab("Cases & Deaths in Nebraska") +
                         theme(text = element_text(size=14, face="bold")) +
                         YaxisScale)
      
    }else if(input$USState=="Nevada"){
      
      COVID19_US_Cases = COVID19_USdata_NYTimes %>%
        filter(state == "Nevada") %>%
        select(everything(), -fips)
      
      plot1 = ggplotly(ggplot(data = COVID19_US_Cases) + 
                         geom_point(aes(x = date, y = cases), color = "blue", size = 2) +
                         geom_point(aes(x = date, y = deaths), color = "red", size = 2) +
                         xlab("Date") +
                         ylab("Cases & Deaths in Nevada") +
                         theme(text = element_text(size=14, face="bold")) +
                         YaxisScale)
      
    }else if(input$USState=="New Hampshire"){
      
      COVID19_US_Cases = COVID19_USdata_NYTimes %>%
        filter(state == "New Hampshire") %>%
        select(everything(), -fips)
      
      plot1 = ggplotly(ggplot(data = COVID19_US_Cases) + 
                         geom_point(aes(x = date, y = cases), color = "blue", size = 2) +
                         geom_point(aes(x = date, y = deaths), color = "red", size = 2) +
                         xlab("Date") +
                         ylab("Cases & Deaths in New Hampshire") +
                         theme(text = element_text(size=14, face="bold")) +
                         YaxisScale)
      
    }else if(input$USState=="New Jersey"){
      
      COVID19_US_Cases = COVID19_USdata_NYTimes %>%
        filter(state == "New Jersey") %>%
        select(everything(), -fips)
      
      plot1 = ggplotly(ggplot(data = COVID19_US_Cases) + 
                         geom_point(aes(x = date, y = cases), color = "blue", size = 2) +
                         geom_point(aes(x = date, y = deaths), color = "red", size = 2) +
                         xlab("Date") +
                         ylab("Cases & Deaths in New Jersey") +
                         theme(text = element_text(size=14, face="bold")) +
                         YaxisScale)
      
    }else if(input$USState=="New Mexico"){
      
      COVID19_US_Cases = COVID19_USdata_NYTimes %>%
        filter(state == "New Mexico") %>%
        select(everything(), -fips)
      
      plot1 = ggplotly(ggplot(data = COVID19_US_Cases) + 
                         geom_point(aes(x = date, y = cases), color = "blue", size = 2) +
                         geom_point(aes(x = date, y = deaths), color = "red", size = 2) +
                         xlab("Date") +
                         ylab("Cases & Deaths in New Mexico") +
                         theme(text = element_text(size=14, face="bold")) +
                         YaxisScale)
      
    }else if(input$USState=="New York"){
      
      COVID19_US_Cases = COVID19_USdata_NYTimes %>%
        filter(state == "New York") %>%
        select(everything(), -fips)
      
      plot1 = ggplotly(ggplot(data = COVID19_US_Cases) + 
                         geom_point(aes(x = date, y = cases), color = "blue", size = 2) +
                         geom_point(aes(x = date, y = deaths), color = "red", size = 2) +
                         xlab("Date") +
                         ylab("Cases & Deaths in New York") +
                         theme(text = element_text(size=14, face="bold")) +
                         YaxisScale)
      
    }else if(input$USState=="North Carolina"){
      
      COVID19_US_Cases = COVID19_USdata_NYTimes %>%
        filter(state == "North Carolina") %>%
        select(everything(), -fips)
      
      plot1 = ggplotly(ggplot(data = COVID19_US_Cases) + 
                         geom_point(aes(x = date, y = cases), color = "blue", size = 2) +
                         geom_point(aes(x = date, y = deaths), color = "red", size = 2) +
                         xlab("Date") +
                         ylab("Cases & Deaths in North Carolina") +
                         theme(text = element_text(size=14, face="bold")) +
                         YaxisScale)
      
    }else if(input$USState=="North Dakota"){
      
      COVID19_US_Cases = COVID19_USdata_NYTimes %>%
        filter(state == "North Dakota") %>%
        select(everything(), -fips)
      
      plot1 = ggplotly(ggplot(data = COVID19_US_Cases) + 
                         geom_point(aes(x = date, y = cases), color = "blue", size = 2) +
                         geom_point(aes(x = date, y = deaths), color = "red", size = 2) +
                         xlab("Date") +
                         ylab("Cases & Deaths in North Dakota") +
                         theme(text = element_text(size=14, face="bold")) +
                         YaxisScale)
      
    }else if(input$USState=="Ohio"){
      
      COVID19_US_Cases = COVID19_USdata_NYTimes %>%
        filter(state == "Ohio") %>%
        select(everything(), -fips)
      
      plot1 = ggplotly(ggplot(data = COVID19_US_Cases) + 
                         geom_point(aes(x = date, y = cases), color = "blue", size = 2) +
                         geom_point(aes(x = date, y = deaths), color = "red", size = 2) +
                         xlab("Date") +
                         ylab("Cases & Deaths in Ohio") +
                         theme(text = element_text(size=14, face="bold")) +
                         YaxisScale)
      
    }else if(input$USState=="Oklahoma"){
      
      COVID19_US_Cases = COVID19_USdata_NYTimes %>%
        filter(state == "Oklahoma") %>%
        select(everything(), -fips)
      
      plot1 = ggplotly(ggplot(data = COVID19_US_Cases) + 
                         geom_point(aes(x = date, y = cases), color = "blue", size = 2) +
                         geom_point(aes(x = date, y = deaths), color = "red", size = 2) +
                         xlab("Date") +
                         ylab("Cases & Deaths in Oklahoma") +
                         theme(text = element_text(size=14, face="bold")) +
                         YaxisScale)
      
    }else if(input$USState=="Oregon"){
      
      COVID19_US_Cases = COVID19_USdata_NYTimes %>%
        filter(state == "Oregon") %>%
        select(everything(), -fips) 
      
      plot1 = ggplotly(ggplot(data = COVID19_US_Cases) + 
                         geom_point(aes(x = date, y = cases), color = "blue", size = 2) +
                         geom_point(aes(x = date, y = deaths), color = "red", size = 2) +
                         xlab("Date") +
                         ylab("Cases & Deaths in Oregon") +
                         theme(text = element_text(size=14, face="bold")) +
                         YaxisScale)
      
    }else if(input$USState=="Pennsylvania"){
      
      COVID19_US_Cases = COVID19_USdata_NYTimes %>%
        filter(state == "Pennsylvania") %>%
        select(everything(), -fips)
      
      plot1 = ggplotly(ggplot(data = COVID19_US_Cases) + 
                         geom_point(aes(x = date, y = cases), color = "blue", size = 2) +
                         geom_point(aes(x = date, y = deaths), color = "red", size = 2) +
                         xlab("Date") +
                         ylab("Cases & Deaths in Pennsylvania") +
                         theme(text = element_text(size=14, face="bold")) +
                         YaxisScale)
      
    }else if(input$USState=="Rhode Island"){
      
      COVID19_US_Cases = COVID19_USdata_NYTimes %>%
        filter(state == "Rhode Island") %>%
        select(everything(), -fips)
      
      plot1 = ggplotly(ggplot(data = COVID19_US_Cases) + 
                         geom_point(aes(x = date, y = cases), color = "blue", size = 2) +
                         geom_point(aes(x = date, y = deaths), color = "red", size = 2) +
                         xlab("Date") +
                         ylab("Cases & Deaths in Rhode Island") +
                         theme(text = element_text(size=14, face="bold")) +
                         YaxisScale)
      
    }else if(input$USState=="South Carolina"){
      
      COVID19_US_Cases = COVID19_USdata_NYTimes %>%
        filter(state == "South Carolina") %>%
        select(everything(), -fips)
      
      plot1 = ggplotly(ggplot(data = COVID19_US_Cases) + 
                         geom_point(aes(x = date, y = cases), color = "blue", size = 2) +
                         geom_point(aes(x = date, y = deaths), color = "red", size = 2) +
                         xlab("Date") +
                         ylab("Cases & Deaths in South Carolina") +
                         theme(text = element_text(size=14, face="bold")) +
                         YaxisScale)
      
    }else if(input$USState=="South Dakota"){
      
      COVID19_US_Cases = COVID19_USdata_NYTimes %>%
        filter(state == "South Dakota") %>%
        select(everything(), -fips)
      
      plot1 = ggplotly(ggplot(data = COVID19_US_Cases) + 
                         geom_point(aes(x = date, y = cases), color = "blue", size = 2) +
                         geom_point(aes(x = date, y = deaths), color = "red", size = 2) +
                         xlab("Date") +
                         ylab("Cases & Deaths in South Dakota") +
                         theme(text = element_text(size=14, face="bold")) +
                         YaxisScale)
      
    }else if(input$USState=="Tennessee"){
      
      COVID19_US_Cases = COVID19_USdata_NYTimes %>%
        filter(state == "Tennessee") %>%
        select(everything(), -fips)
      
      plot1 = ggplotly(ggplot(data = COVID19_US_Cases) + 
                         geom_point(aes(x = date, y = cases), color = "blue", size = 2) +
                         geom_point(aes(x = date, y = deaths), color = "red", size = 2) +
                         xlab("Date") +
                         ylab("Cases & Deaths in Tennessee") +
                         theme(text = element_text(size=14, face="bold")) +
                         YaxisScale)
      
    }else if(input$USState=="Texas"){
      
      COVID19_US_Cases = COVID19_USdata_NYTimes %>%
        filter(state == "Texas") %>%
        select(everything(), -fips)
      
      plot1 = ggplotly(ggplot(data = COVID19_US_Cases) + 
                         geom_point(aes(x = date, y = cases), color = "blue", size = 2) +
                         geom_point(aes(x = date, y = deaths), color = "red", size = 2) +
                         xlab("Date") +
                         ylab("Cases & Deaths in Texas") +
                         theme(text = element_text(size=14, face="bold")) +
                         YaxisScale)
      
    }else if(input$USState=="Utah"){
      
      COVID19_US_Cases = COVID19_USdata_NYTimes %>%
        filter(state == "Utah") %>%
        select(everything(), -fips)
      
      plot1 = ggplotly(ggplot(data = COVID19_US_Cases) + 
                         geom_point(aes(x = date, y = cases), color = "blue", size = 2) +
                         geom_point(aes(x = date, y = deaths), color = "red", size = 2) +
                         xlab("Date") +
                         ylab("Cases & Deaths in Utah") +
                         theme(text = element_text(size=14, face="bold")) +
                         YaxisScale)
      
    }else if(input$USState=="Vermont"){
      
      COVID19_US_Cases = COVID19_USdata_NYTimes %>%
        filter(state == "Vermont") %>%
        select(everything(), -fips)    
      
      plot1 = ggplotly(ggplot(data = COVID19_US_Cases) + 
                         geom_point(aes(x = date, y = cases), color = "blue", size = 2) +
                         geom_point(aes(x = date, y = deaths), color = "red", size = 2) +
                         xlab("Date") +
                         ylab("Cases & Deaths in Vermont") +
                         theme(text = element_text(size=14, face="bold")) +
                         YaxisScale)
      
    }else if(input$USState=="Virginia"){
      
      COVID19_US_Cases = COVID19_USdata_NYTimes %>%
        filter(state == "Virginia") %>%
        select(everything(), -fips)
      
      plot1 = ggplotly(ggplot(data = COVID19_US_Cases) +  
                         geom_point(aes(x = date, y = cases), color = "blue", size = 2) +
                         geom_point(aes(x = date, y = deaths), color = "red", size = 2) +
                         xlab("Date") +
                         ylab("Cases & Deaths in Virginia") +
                         theme(text = element_text(size=14, face="bold")) +
                         YaxisScale)
      
    }else if(input$USState=="Washington"){
      
      COVID19_US_Cases = COVID19_USdata_NYTimes %>%
        filter(state == "Washington") %>%
        select(everything(), -fips)
      
      plot1 = ggplotly(ggplot(data = COVID19_US_Cases) + 
                         geom_point(aes(x = date, y = cases), color = "blue", size = 2) +
                         geom_point(aes(x = date, y = deaths), color = "red", size = 2) +
                         xlab("Date") +
                         ylab("Cases & Deaths in Washington") +
                         theme(text = element_text(size=14, face="bold")) +
                         YaxisScale)
      
    }else if(input$USState=="West Virginia"){
      
      COVID19_US_Cases = COVID19_USdata_NYTimes %>%
        filter(state == "West Virginia") %>%
        select(everything(), -fips)
      
      plot1 = ggplotly(ggplot(data = COVID19_US_Cases) + 
                         geom_point(aes(x = date, y = cases), color = "blue", size = 2) +
                         geom_point(aes(x = date, y = deaths), color = "red", size = 2) +
                         xlab("Date") +
                         ylab("Cases & Deaths in West Virginia") +
                         theme(text = element_text(size=14, face="bold")) +
                         YaxisScale)
      
    }else if(input$USState=="Wisconsin"){
      
      COVID19_US_Cases = COVID19_USdata_NYTimes %>%
        filter(state == "Wisconsin") %>%
        select(everything(), -fips)
      
      plot1 = ggplotly(ggplot(data = COVID19_US_Cases) + 
                         geom_point(aes(x = date, y = cases), color = "blue", size = 2) +
                         geom_point(aes(x = date, y = deaths), color = "red", size = 2) +
                         xlab("Date") +
                         ylab("Cases & Deaths in Wisconsin") +
                         theme(text = element_text(size=14, face="bold")) +
                         YaxisScale)
      
    }else if(input$USState=="Wyoming"){
      
      COVID19_US_Cases = COVID19_USdata_NYTimes %>%
        filter(state == "Wyoming") %>%
        select(everything(), -fips)
      
      plot1 = ggplotly(ggplot(data = COVID19_US_Cases) + 
                         geom_point(aes(x = date, y = cases), color = "blue", size = 2) +
                         geom_point(aes(x = date, y = deaths), color = "red", size = 2) +
                         xlab("Date") +
                         ylab("Cases & Deaths in Wyoming") +
                         theme(text = element_text(size=14, face="bold")) +
                         YaxisScale)
      
    }else if(input$USState=="District of Columbia"){
      
      COVID19_US_Cases = COVID19_USdata_NYTimes %>%
        filter(state == "District of Columbia") %>%
        select(everything(), -fips)
      
      plot1 = ggplotly(ggplot(data = COVID19_US_Cases) + 
                         geom_point(aes(x = date, y = cases), color = "blue", size = 2) +
                         geom_point(aes(x = date, y = deaths), color = "red", size = 2) +
                         xlab("Date") +
                         ylab("Cases & Deaths in D.C.") +
                         theme(text = element_text(size=14, face="bold")) +
                         YaxisScale)
      
    }else if(input$USState=="Guam"){
      
      COVID19_US_Cases = COVID19_USdata_NYTimes %>%
        filter(state == "Guam") %>%
        select(everything(), -fips)
      
      plot1 = ggplotly(ggplot(data = COVID19_US_Cases) + 
                         geom_point(aes(x = date, y = cases), color = "blue", size = 2) +
                         geom_point(aes(x = date, y = deaths), color = "red", size = 2) +
                         xlab("Date") +
                         ylab("Cases & Deaths in Guam") +
                         theme(text = element_text(size=14, face="bold")) +
                         YaxisScale)
      
    }else if(input$USState=="Puerto Rico"){
      
      COVID19_US_Cases = COVID19_USdata_NYTimes %>%
        filter(state == "Puerto Rico") %>%
        select(everything(), -fips)
      
      plot1 = ggplotly(ggplot(data = COVID19_US_Cases) + 
                         geom_point(aes(x = date, y = cases), color = "blue", size = 2) +
                         geom_point(aes(x = date, y = deaths), color = "red", size = 2) +
                         xlab("Date") +
                         ylab("Cases & Deaths in Puerto Rico") +
                         theme(text = element_text(size=14, face="bold")) +
                         YaxisScale)                   
      
      
    }
    
    plot1
    
  })
  
  
}

shinyApp(ui = ui, server = server)



