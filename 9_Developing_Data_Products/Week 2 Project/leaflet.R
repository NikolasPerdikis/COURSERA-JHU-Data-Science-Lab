library(leaflet)
my_map <- leaflet() %>% 
  addTiles()
my_map


my_map=leaflet()
my_map=addTiles(my_map)
my_map

my_map <- my_map %>% 
    addMarkers(lat=48.5214780,lng=17.4237850,popup = "Here is Rady") 
my_map


set.seed(2016-04-25)
df <- data.frame(lat = runif(20, min = 39.2, max = 39.3),
                 lng = runif(20, min = -76.6, max = -76.5))
df %>% 
    leaflet() %>%
    addTiles() %>%
    addMarkers()

hopkinsIcon <- makeIcon(
    iconUrl = "https://grcf.jhmi.edu/wp-content/uploads/2013/07/medicine.shield.small_.blue_.jpg",
    iconWidth = 31*215/230, iconHeight = 31,
    iconAnchorX = 31*215/230/2, iconAnchorY = 16
)


hopkinsLatLong <- data.frame(
    lat = c(39.2973166, 39.3288851, 39.2906617),
    lng = c(-76.5929798, -76.6206598, -76.5469683))

hopkinsLatLong %>% 
    leaflet() %>%
    addTiles() %>%
    addMarkers(icon = hopkinsIcon)

#The order of the links below is the one of the set of lat/lon values

hopkinsSites <- c(
    "<a href='http://www.jhsph.edu/'>East Baltimore Campus</a>",
    "<a href='https://apply.jhu.edu/visit/homewood/'>Homewood Campus</a>",
    "<a href='http://www.hopkinsmedicine.org/johns_hopkins_bayview/'>Bayview Medical Center</a>",
    "<a href='http://www.peabody.jhu.edu/'>Peabody Institute</a>",
    "<a href='http://carey.jhu.edu/'>Carey Business School</a>"
)

hopkinsLatLong %>% 
    leaflet() %>%
    addTiles() %>%
    addMarkers(icon = hopkinsIcon, popup = hopkinsSites)

#cluster of points, zoom in/out to see different cluster sizes

df <- data.frame(lat = runif(500, min = 39.25, max = 39.35),
                 lng = runif(500, min = -76.65, max = -76.55))
df %>% 
    leaflet() %>%
    addTiles() %>%
    addMarkers(clusterOptions = markerClusterOptions())


df <- data.frame(lat = runif(20, min = 39.25, max = 39.35),
                 lng = runif(20, min = -76.65, max = -76.55))
df %>% 
    leaflet() %>%
    addTiles() %>%
    addCircleMarkers()



md_cities <- data.frame(name = c("Baltimore", "Frederick", "Rockville", "Gaithersburg", 
                                 "Bowie", "Hagerstown", "Annapolis", "College Park", "Salisbury", "Laurel"),
                        pop = c(619493, 66169, 62334, 61045, 55232,
                                39890, 38880, 30587, 30484, 25346),
                        lat = c(39.2920592, 39.4143921, 39.0840, 39.1434, 39.0068, 39.6418, 38.9784, 38.9897, 38.3607, 39.0993),
                        lng = c(-76.6077852, -77.4204875, -77.1528, -77.2014, -76.7791, -77.7200, -76.4922, -76.9378, -75.5994, -76.8483))
md_cities %>%
    leaflet() %>%
    addTiles() %>%
    addCircles(weight = 10, radius = sqrt(md_cities$pop) * 30)


#Rectangles
leaflet() %>%
    addTiles() %>%
    addRectangles(lat1 = 37.3858, lng1 = -122.0595, 
                  lat2 = 37.3890, lng2 = -122.0625)

#Adding Legends
df <- data.frame(lat = runif(20, min = 39.25, max = 39.35),
                 lng = runif(20, min = -76.65, max = -76.55),
                 col = sample(c("red", "blue", "green"), 20, replace = TRUE),
                 stringsAsFactors = FALSE)

df %>%
    leaflet() %>%
    addTiles() %>%
    addCircleMarkers(color = df$col) %>%
    addLegend(labels = LETTERS[1:3], colors = c("blue", "red", "green"))



df %>% leaflet() %>% addTiles()