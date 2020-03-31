# install.packages("shiny") # R application deployment
# install.packages("leaflet") #creating the map

library(shiny)
library(leaflet)


#Manuel data entering
#Please make this better
SA_provinces <- data.frame(
    
    name = c("NC", "NW", "GP", "LP","MP", 
             "KZN", "EC", "FS", "WC", "UNK"),
    
    lat = c(-30.377, -26.333, -25.9811, -23.6277,-25.92623,
            -28.743,-32.24608,-28.456,-33.6468,-32.44),
    
    lng = c(21.070, 25.795, 28.167, 29.351,30.206,
            30.753,26.5249,27.063,19.565,29.68),
    
    #Current Active Cases
    totCases = c(6,8,633,14,12,
                 179,12,74,325,90),
    
    totDeaths  = c(0,0,1,0,0,
                   2,0,1,1,0)
)

hist_stats <- data.frame(
    totTest = c(645,848,924,1017,1476,2405,2911,3070,4832,6438,7425,9315,12815,15290,20471,28537,31963,35593,38409,41072),
    
    # totPositive = c(13,16,24,38,51,62,85,116,150,202,240,274,402,554,927,1770,1187,1280,1326,1353),
    # 
    # totrecovered = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
    # 
    # totDead = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,2,3,3,3),
    
    
    date = c("2020/03/11","2020/03/12","2020/03/13","2020/03/14","2020/03/15",
             "2020/03/16","2020/03/17", "2020/03/18","2020/03/19","2020/03/20",
             "2020/03/21","2020/03/22","2020/03/23","2020/03/24","2020/03/26",
             "2020/03/27","2020/03/28","2020/03/29","2020/03/30","2020/03/31")
    
)



# R SHINY

# Define UI for application that draws map and barplot
ui <- fluidPage(
    titlePanel("Active COVID-19 cases in the Republic of South Africa (RSA)"),
    mainPanel(h3( paste("Active cases:", sum(SA_provinces$totCases ), paste(", Deaths:", sum(SA_provinces$totDeaths)))   )),
    
    mainPanel(
        
        tabsetPanel(
            tabPanel("Province Map",    
                     leafletOutput("covidmap"),
                     plotOutput("covidBarplot")),
            
            tabPanel("Total tests conducted",   
                     plotOutput("testDayPlotCUM"), 
                     plotOutput("testPerDay"))    
          )),
    

 
    
    #Add source
    mainPanel(h4("Awaiting update per province from the NICD")
        ,h6("Source: National Institute for Communicable Deseases of South Africa, URL: https://www.nicd.ac.za, Twitter: @nicd_sa"))
)


# Define server logic required to draw the map an barplot
server <- function(input, output){ 

    output$covidmap <- renderLeaflet({
        leaflet() %>%
            addTiles() %>%  # Add default OpenStreetMap map tiles
            addCircleMarkers(popup = paste(SA_provinces$name, SA_provinces$totCases, sep = ", Total cases: "),
                             lng = SA_provinces$lng,
                             lat = SA_provinces$lat,
                             radius = SA_provinces$totCases/sum(SA_provinces$totCases)*15) #size of the circle
        
        
        
    })
    
    #Adding the barplot annd legend
    output$covidBarplot <- renderPlot({
        
        myOrder = order(SA_provinces$totCases, decreasing = T)
        barplot(SA_provinces$totCases[myOrder],
                names = SA_provinces$name[myOrder],
                las = 2,
                col = heat.colors(10),
                main = "COVID-19 per province in RSA",
                xlab = "Province Abbreviation",
                ylab = "Total Cases"
        )
        legend("topright", legend = paste(SA_provinces$totCases[myOrder], SA_provinces$name[myOrder],  sep = " - "),
               fill = heat.colors(10))
    })
    
    output$testDayPlotCUM <- renderPlot({
        
        totCaseLarge10 <- seq(1,length(hist_stats$totTest),1)
        plot(totCaseLarge10,hist_stats$totTest, type = "b",
             xlab = "Each day after 10 cases were recorded",
             ylab = "Cumulative tests",
             main = "Total COVID-19 tests conducted",
             lwd = 2,
             axes = F,
             col = "blue")
        axis(side = 1, totCaseLarge10, las = 2)
        axis(side = 2, las = 2)
        abline(h = seq(2500,max(hist_stats$totTest), by = 2500), col = "lightgray", lty = 3)
        
    })
    
    output$testPerDay <- renderPlot({
        
        
        testPerday = 0
        for(i in 2:length(hist_stats$totTest))
        {
            testPerday = cbind(testPerday,(hist_stats$totTest[i]- hist_stats$totTest[i-1]))
        }
        
        
        barplot(testPerday, 
                names = 1:length(testPerday),
                col = "lightblue",
                main = "COVID-19 tests per day",
                xlab = "Each day after 10 cases were recorded",
                ylab = "Total Tests",
                axes = T,
                las = 2,
                yaxt = "n")
        abline(h = testPerday, col = "lightgray", lty = 3)
        #axis(side = 1,0:length(testPerday), las = 2)
        axis(side = 2, sort(c(seq(1000,max(testPerday), by = 1000),testPerday)), las = 2)
        
        
        
        
    })

}

# Run the application 
shinyApp(ui = ui, server = server)

