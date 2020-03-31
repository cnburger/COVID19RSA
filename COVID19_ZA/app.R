install.packages("shiny") # R application deployment
install.packages("leaflet") #creating the map

library(shiny)
library(leaflet)

# Define UI for application that draws map and barplot
ui <- fluidPage(

    # Application title
    titlePanel("Active COVID-19 cases in the Republic of South Africa (RSA)"),
    
    #Print the map
    leafletOutput("covidmap"),
    
    #Print the barplot
    plotOutput("covidBarplot"),
    
    #Add source
    mainPanel(h6("Source: National Institute for Communicable Deseases. URL: https://www.nicd.ac.za/ Twitter: @nicd_sa"))
)

#Manuel data entering
#Please make this better
SA_provinces <- data.frame(
    
    name = c("NC", "NW", "GP", "LP","MP", 
             "KZN", "EC", "FS", "WC"),
    
    lat = c(-30.377, -26.333, -25.9811, -23.6277,-25.92623,
            -28.743,-32.24608,-28.456,-33.6468),
    
    lng = c(21.070, 25.795, 28.167, 29.351,30.206,
            30.753,26.5249,27.063,19.565),
    
    #Current Active Cases
    totCases = c(3,8,618,11,11,
                 171,12,72,324)
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
                las = 1,
                col = heat.colors(9),
                main = "COVID-19 per province in RSA",
                xlab = "Province Abbreviation",
                ylab = "Total Cases"
        )
        legend("topright", legend = paste(SA_provinces$totCases[myOrder], SA_provinces$name[myOrder],  sep = " - "),
               fill = heat.colors(9))
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
