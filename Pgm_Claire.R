renv::restore()
file.edit("~/.Renviron") # pour mettre les paramètres de la base postgre

library(RPostgres)
library(dplyr)
library(knitr)
library(sf)
library(janitor)
library(aws.s3)

conn <- dbConnect(Postgres(),
                  user = Sys.getenv("USER_POSTGRESQL"),
                  password = Sys.getenv("PASS_POSTGRESQL"),
                  host = Sys.getenv("HOST_POSTGRESQL"),
                  dbname = "defaultdb",
                  port = 5432,
                  check_interrupts = TRUE)

df <- data.frame(
  a = c("a", "b", "c"),
  b = c(1, 2, 3)
  
  
parcelles <- s3read_using(
    FUN = sf::read_sf,
    query = 'SELECT * FROM parcelles_graphiques LIMIT 10',
    object = "2023/sujet2/diffusion/ign/rpg/PARCELLES_GRAPHIQUES.gpkg",
    bucket = "projet-funathon",
    opts = list("region" = "")
  )
  
  write_sf(parcelles, conn, Id(schema = "test_schema", table = "test_parcelles"), delete_layer = TRUE)
)

res <- dbSendQuery(conn, "CREATE SCHEMA IF NOT EXISTS test_schema")
dbWriteTable(conn, Id(schema = "test_schema", table = "test_table"), df, overwrite = TRUE)
res <- dbGetQuery(conn, "SELECT * FROM test_schema.test_table")

res %>% kable()

conn <- dbConnect(Postgres(),
                  user = Sys.getenv("USER_POSTGRESQL"),
                  password = Sys.getenv("PASS_POSTGRESQL"),
                  host = Sys.getenv("HOST_POSTGRESQL"),
                  dbname = "defaultdb",
                  port = 5432,
                  check_interrupts = TRUE)

library(tidyverse) 
library(aws.s3)
library(sf)
library(RPostgres)
library(janitor)
library(kableExtra)
library(leaflet)
library(htmlwidgets)
library(RColorBrewer)

coord_gmaps <- "50.652404658153344, 3.0745582308029435"
lat <- as.numeric(str_split(coord_gmaps, fixed(","), simplify = TRUE)[,1])
lon <- as.numeric(str_split(coord_gmaps, fixed(","), simplify = TRUE)[,2])
rayon <- 10000

# Création d'une table sf «point» avec les coordonnées saisies
# Transformation des coordonnées en système de proj 2154 (Lambert II) 
pointbis <- data.frame(lon, lat, rayon) %>% 
  st_as_sf(coords = c("lon","lat"), crs = "EPSG:4326") %>%
  mutate(coord_pt_gps = st_as_text(geometry)) %>% 
  st_transform("EPSG:2154") %>% 
  st_sf() %>%
  clean_names() %>% 
  rename(geom = geometry)


