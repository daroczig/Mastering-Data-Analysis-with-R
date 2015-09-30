## Extracted code chunks from
##
##  Gergely Daróczi (2015): Mastering Data Analysis with R.
##
##    Chapter #13: Data Around Us. pp. 297-322.
##
##
##  This file includes the code chunks from the above mentioned
##  chapter except for the leading ">" and "+" characters, which
##  stand for the prompt in the R console. The prompt was
##  intentionally removed here along with arbitrary line-breaks,
##  so that you copy and paste the R expressions to the R console
##  in a more convenient and seamless way.
##
##  Code chunks are grouped here by the printed pages of the book.
##  Two hash signs at the beginning of a line stands for a page
##  break, while an extra empty line between the code chunks
##  represents one or more paragraphs in the original book between
##  the examples for easier navigation.
##
##  Sometimes extra instructions starting with a double hash are
##  also provided on how to run the below expressions.
##
##
##  Find more information on the book at http://bit.ly/mastering-R
##  and you can contact me on Twitter and GitHub by the @daroczig
##  handle, or mail me at daroczig@rapporter.net
##

library(hflights)
library(data.table)
dt <- data.table(hflights)[, list(
    N         = .N,
    Cancelled = sum(Cancelled),
    Distance  = Distance[1],
    TimeVar   = sd(ActualElapsedTime, na.rm = TRUE),
    ArrDelay  = mean(ArrDelay, na.rm = TRUE)) , by = Dest]

##

str(dt)

library(ggmap)
(h <- geocode('Houston, TX'))

##

h <- data.frame(lat = 29.7630556, lon = -95.36306556)

dt[, c('lon', 'lat') := geocode(Dest)]

str(setDF(dt))

par(mar=(c(0,0,1,0)))
library(maps)
map('state')
title('Flight destinations from Houston,TX')

##

points(h$lon, h$lat, col = 'blue', pch = 13)
points(dt$lon, dt$lat, col = 'red', pch = 19)

text(dt$lon, dt$lat + 1, labels = dt$Dest, cex = 0.7)

##

map('state')
title('Frequent flight destinations from Houston,TX')
points(h$lon, h$lat, col = 'blue', pch = 13)
points(dt$lon, dt$lat, col = rgb(1, 0, 0, dt$N/max(dt$N)), pch = 19)
legend('bottomright', legend = round(quantile(dt$N)), col = rgb(1, 0, 0, quantile(dt$N)/max(dt$N)), pch = 19, box.col = NA)

##

str(map_data <- map('state', plot = FALSE, fill = TRUE))

grep('^washington', map_data$names, value = TRUE)

states <- sapply(strsplit(map_data$names, ':'), '[[', 1)

library(maptools)
us <- map2SpatialPolygons(map_data, IDs = states, proj4string = CRS('+proj=longlat +datum=WGS84'))

## ## alternative way of getting the data in the right form:
## library(raster)
## us <- getData('GADM', country = 'USA', level = 1)
## library(rgeos)
## us <- gSimplify(us, tol = 0.01, topologyPreserve = TRUE)

plot(us)

library(sp)
dtp <- SpatialPointsDataFrame(dt[, c('lon', 'lat')], dt, proj4string = CRS('+proj=longlat +datum=WGS84'))
str(sp::over(us, dtp))

##

## ## alternative way of getting state info
## geocode('LAX')
## geocode('LAX', 'more')
## geocode('LAX', 'all')$results[[1]]$address_components[[6]]$short_name

str(sapply(sp::over(us, dtp, returnList = TRUE), function(x) sum(x$Cancelled)))

dtp <- SpatialPointsDataFrame(dt[, c('lon', 'lat')], dt[, 'Cancelled', drop = FALSE], proj4string = CRS('+proj=longlat +datum=WGS84'))
str(cancels <- sp::over(us, dtp, fn = sum))

val <- cancels$Cancelled[match(states, row.names(cancels))]

##

val[is.na(val)] <- 0

map('state', col = rgb(1, 0, 0, sqrt(val/max(val))), fill = TRUE)
title('Number of cancelled flights from Houston to US states')
points(h$lon, h$lat, col = 'blue', pch = 13)
legend('bottomright', legend = round(quantile(val)), fill = rgb(1, 0, 0, sqrt(quantile(val)/max(val))), box.col = NA)

##

library(fields)
out  <- as.image(dt$ArrDelay, x = dt[, c('lon', 'lat')], nrow = 256, ncol = 256)

table(is.na(out$z))

image(out)

##

image(out, xlim = base::range(map_data$x, na.rm = TRUE), ylim = base::range(map_data$y, na.rm = TRUE))

look <- image.smooth(out, theta = .5)
table(is.na(look$z))

##

image(look)

out  <- as.image(dt$ArrDelay, x = dt[, c('lon', 'lat')], nrow = 512, ncol = 512)
look <- image.smooth(out, theta = 1)

usa_data <- map('usa', plot = FALSE, region = 'main')
p <- expand.grid(look$x, look$y)
library(sp)
n <- which(point.in.polygon(p$Var1, p$Var2, usa_data$x, usa_data$y) == 0)
look$z[n] <- NA

##

map('usa')
image(look, add = TRUE)
map('state', lwd = 3, add = TRUE)
title('Arrival delays of flights from Houston')
points(dt$lon, dt$lat, pch = 19, cex = .5)
points(h$lon, h$lat, pch = 13)

library(deldir)
map('usa')
plot(deldir(dt$lon, dt$lat), wlines = 'tess', col = c('red', 'darkgray'), lwd = 2, add = TRUE, pch = 19)

##

library(OpenStreetMap)
map <- openmap(c(max(map_data$y, na.rm = TRUE),
                 min(map_data$x, na.rm = TRUE)),
               c(min(map_data$y, na.rm = TRUE),
                 max(map_data$x, na.rm = TRUE)),
               type = 'stamen-terrain')

##

map <- openproj(map, projection = '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')

plot(map)
plot(deldir(dt$lon, dt$lat), wlines = "tess", col = c('red', 'black'), lwd = 2, add = TRUE, pch = 19, cex = 0.5)

##

map <- openmap(c(max(map_data$y),
                 min(map_data$x)),
               c(min(map_data$y),
                 max(map_data$x)),
               type = 'cloudmade-1960')
plot(map)

##

cancels$state <- rownames(cancels)
cancels$Cancelled[is.na(cancels$Cancelled)] <- 0

library(googleVis)
plot(gvisGeoChart(cancels, 'state', 'Cancelled',
                  options = list(
                      region      = 'US',
                      displayMode = 'regions',
                      resolution  = 'provinces')))


##

dt$LatLong <- paste(dt$lat, dt$lon, sep = ':')
dt$tip <- apply(dt, 1, function(x)
                 paste(names(dt), x, collapse = '<br/ >'))
plot(gvisMap(dt, 'LatLong', tipvar = 'tip'))

##

library(rCharts)
map <- Leaflet$new()
map$setView(as.numeric(dt[which(dt$Dest == 'MCI'), c('lat', 'lon')]), zoom = 4)

##


for (i in 1:nrow(dt))
    map$marker(c(dt$lat[i], dt$lon[i]), bindPopup = dt$tip[i])
map$show()

library(leaflet)

pal <- colorNumeric(palette = 'Blues', domain = countries$gdp_md_est)

leaflet(us) %>% addProviderTiles("Acetate.terrain") %>% addPolygons( color = ~pal(gdp_md_est)) %>% addMarkers(lng = dt$lon, lat = dt$lat, popup = dt$tip)

##

dt <- dt[point.in.polygon(dt$lon, dt$lat, usa_data$x, usa_data$y) == 1, ]

##

library(diagram)
library(scales)

map('usa')
title('Number of flights, cancellations and delays from Houston')
image(look, add = TRUE)
map('state', lwd = 3, add = TRUE)
for (i in 1:nrow(dt)) {
    curvedarrow(
        from       = rev(as.numeric(h)),
        to         = as.numeric(dt[i, c('lon', 'lat')]),
        arr.pos    = 1,
        arr.type   = 'circle',
        curve      = 0.1,
        arr.col    = alpha('black', dt$N[i] / max(dt$N)),
        arr.length = dt$N[i] / max(dt$N),
        lwd        = dt$Cancelled[i] / max(dt$Cancelled) * 25,
        lcol       = alpha('black', dt$Cancelled[i] / max(dt$Cancelled)))
}

## models

dm <- dist(dt[, c('lon', 'lat')])
dm <- as.matrix(dm)
idm <- 1/dm
diag(idm) <- 0
str(idm)

library(ape)
dt$TimeVar[is.na(dt$TimeVar)] <- 0
Moran.I(dt$TimeVar, idm)

##

library(spdep)
idml <- mat2listw(idm)
moran.test(dt$TimeVar, idml)

idml <- mat2listw(idm, style = 'W')
moran.test(dt$TimeVar, idml)
