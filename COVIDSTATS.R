install.packages("leaflet")
install.packages("ggplot2")
library(leaflet)
library(ggplot2)
m <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng=27.9015, lat=-26.1201, popup="The birthplace of R")
m  # Print the map


SA_provinces <- data.frame(
  
                        name = c("NC", "NW", "GP", "LP","MP", 
                                 "KZN", "EC", "FS", "WC"),
                      
                        lat = c(-30.377, -26.333, -25.9811, -23.6277,-25.92623,
                                -28.743,-32.24608,-28.456,-33.6468),
                        
                        lng = c(21.070, 25.795, 28.167, 29.351,30.206,
                                30.753,26.5249,27.063,19.565),
                        
                        totCases = c(6,6,584,12,11,
                                     167,12,72,310)
                        )
m <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addCircleMarkers(popup = paste(SA_provinces$name, SA_provinces$totCases, sep = ", Total cases: "),
             lng = SA_provinces$lng,
             lat = SA_provinces$lat,
             radius = SA_provinces$totCases/sum(SA_provinces$totCases)*20)
m  # Print the map


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





hist_stats <- data.frame(
    totTest = c(645,848,924,1017,1476,2405,2911,3070,4832,6438,7425,9315,12815,15290,20471,28537,31963,35593),
    
    totPositive = c(13,16,24,38,51,62,85,116,150,202,240,274,402,554,927,1770,1187,1280),
    
    totrecovered = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),

    totDead = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,2),


    date = c("2020/03/11","2020/03/12","2020/03/13","2020/03/14","2020/03/15",
             "2020/03/16","2020/03/17", "2020/03/18","2020/03/19","2020/03/20",
             "2020/03/21","2020/03/22","2020/03/23","2020/03/24","2020/03/26",
             "2020/03/27","2020/03/28","2020/03/29")
    
)

# ggplot(hist_stats$date,hist_stats$totTest, lty = 7)
# 
# 
# ggplot(data=hist_stats, aes(x=date, y=totTest, group=1)) +
#   geom_line(linetype = "dashed")+
#   geom_point() + 
#   theme(axis.text.x = element_text(angle = 90))

plot()
  




