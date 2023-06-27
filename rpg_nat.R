library(RPostgres)
library(dplyr)
library(knitr)
library(sf)
library(janitor)
library(aws.s3)
library(DBI)
conn <- dbConnect(Postgres(),
                  user = Sys.getenv("USER_POSTGRESQL"),
                  password = Sys.getenv("PASS_POSTGRESQL"),
                  host = Sys.getenv("HOST_POSTGRESQL"),
                  dbname = "defaultdb",
                  port = 5432,
                  check_interrupts = TRUE)

x<-48.01469669407841
y<- -4.105676552817091
rayon<-100000

point <- data.frame(y, x, rayon) %>% 
  st_as_sf(coords = c("x","y"), crs = "EPSG:4326")
  point_l93<-st_transform(point,crs=2154)%>% 
    rename(geom = geometry)
plot(point_l93)
str(point_l93)

dbListTables(conn)

# Ecriture de la table point dans une table PostGIS
write_sf(point_l93, conn, Id(schema = "public", table = "point_nlc"))
