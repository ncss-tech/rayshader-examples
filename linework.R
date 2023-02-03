
library(terra)
library(elevatr)
library(rayshader)
library(soilDB)
library(sf)




# clay center DSS site
bb <- '-97.0984 39.3804,-97.0984 39.4123,-97.0282 39.4123,-97.0282 39.3804,-97.0984 39.3804'

# convert text -> WKT -> sf
wkt <- sprintf('POLYGON((%s))', bb)
x <- st_as_sfc(wkt, crs = 4326)



# query WCS
m <- mukey.wcs(x, db = 'gSSURGO')

plot(m)

## get MU polygons -> rasterize
mu <- SDA_spatialQuery(x, what = 'mupolygon', geomIntersection = TRUE)
mu <- st_transform(mu, 5070)
mu <- vect(mu)

## prepare DEM
z <- get_elev_raster(raster::raster(m), z = 14, prj = 'EPSG:5070')
z <- rast(z)


m <- terra::resample(m, z, method = 'cubic')


# check: OK
plot(z, col = viridis::mako(25), axes = FALSE, mar = c(0, 0, 0, 0))
lines(mu)

png(file = 'overlay.png', width = ncol(m), height = nrow(m))
plot(m, legend = FALSE, axes = FALSE, maxcell = ncell(m), mar = c(0, 0, 0, 0))
dev.off()

## vector overlay
png(file = 'overlay.png', width = ncol(m) * 10, height = nrow(m) * 10)
plot(mu, legend = FALSE, axes = FALSE, lwd = 2, mar = c(0, 0, 0, 0))
dev.off()



# # a little vertical exaggeration
# z <- z * 3

# convert raster -> matrix
elmat <- raster_to_matrix(z)

# thematic map, as PNG, exact same dimensions
ov <- png::readPNG('overlay.png')




# compute shadows
raymat <- ray_shade(elmat, multicore = TRUE, progbar = TRUE, zscale = 0.1)
ambmat <- ambient_shade(elmat, multicore = TRUE, progbar = TRUE, zscale = 0.1)


## testing
elmat %>%
  sphere_shade(texture = "imhof4") %>%
  add_shadow(raymat) %>%
  add_shadow(ambmat) %>%
  add_overlay(ov, alphalayer = 0.8) %>%
  plot_map()




# camera parameters
.theta <- 15
.phi <- 35
.zoom <- 0.4
.fov <- 48

## output size
px.width <- 1200
px.height <- 800


elmat %>%
  sphere_shade(texture = "imhof4") %>%
  add_shadow(raymat) %>%
  add_shadow(ambmat) %>%
  add_overlay(ov, alphalayer = 0.9) %>%
  plot_3d(elmat, zscale = 3, windowsize = c(px.width, px.height),
          baseshape='rectangle', lineantialias=TRUE,
          theta = .theta, phi = .phi, zoom = .zoom, fov = .fov)


## do this after a session, to clear rgl device
rgl::rgl.close()








