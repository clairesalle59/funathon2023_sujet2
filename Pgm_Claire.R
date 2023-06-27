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
point_claire <- data.frame(lon, lat, rayon) %>% 
  st_as_sf(coords = c("lon","lat"), crs = "EPSG:4326") %>%
  mutate(coord_pt_gps = st_as_text(geometry)) %>% 
  st_transform("EPSG:2154") %>% 
  st_sf() %>%
  clean_names() %>% 
  rename(geom = geometry)

# Optionnel, suppression de la table `point` si elle existe
res <- dbSendQuery(conn, "DROP TABLE IF EXISTS public.point_claire CASCADE;")

# Ecriture de la table point dans une table PostGIS
write_sf(point_claire, conn, Id(schema = "public", table = "point_claire"))

# Envoi de la requête de découpage du RPG autour du point sur PostGIS
query <- "SELECT row_number() OVER () AS row_id, p.coord_pt_gps, p.rayon, r.*  
    FROM public.point_claire p, rpg.parcelles r 
    WHERE ST_DWithin(p.geom, r.geom, p.rayon);"

parc_prox <- st_read(conn, query = query)


# Récupération des libellés des différentes cultures
lib_cult <- s3read_using(FUN = read_csv2,
                         object = "2023/sujet2/diffusion/ign/rpg/REF_CULTURES_GROUPES_CULTURES_2020.csv",
                         col_types = cols(.default = col_character()),
                         bucket = "projet-funathon",
                         opts = list("region" = "")) %>% clean_names()


lib_group_cult <- lib_cult %>% 
  select(code_groupe_culture, libelle_groupe_culture) %>% 
  distinct(code_groupe_culture, .keep_all=T)

lib_group_cult %>% kable()

# Création d'une palette de couleurs associée au groupe de culture
factpal <- colorFactor("Paired", parc_prox$code_group)

# Transformation de la projection car leaflet ne connait que le WGS 84
parc_prox <- parc_prox %>% st_transform(4326)

# Pour ajouter un marqueur du point
pt_mark <- point_claire %>% st_transform(4326)

# Ajout du libellé des cultures
parc_prox_lib <- parc_prox %>% 
  left_join(lib_cult %>% select(-code_groupe_culture), by = c("code_cultu" = "code_culture")) 

# Création d'un label ad hoc à afficher en surbrillance au passage de la souris sur la carte
labels <- sprintf("<strong>id_parcel : </strong>%s<br/>
                  <strong>Groupe culture : </strong>%s<br/>
                  <strong>Culture : </strong>%s<br/>
                  <strong>Surface (ha) : </strong>%s<br/>
                  <strong>Département : </strong>%s<br/>
                  <strong>Commune : </strong>%s<br/>",
                  parc_prox$id_parcel,
                  parc_prox_lib$libelle_groupe_culture,
                  parc_prox_lib$libelle_culture,
                  parc_prox$surf_parc,
                  parc_prox$insee_dep,
                  parc_prox$nom_com) %>% 
  lapply(htmltools::HTML)

# Création de la carte
carte_parc_prox_html <- leaflet(parc_prox_lib) %>% 
  addTiles("http://wxs.ign.fr/essentiels/wmts?REQUEST=GetTile&SERVICE=WMTS&VERSION=1.0.0&STYLE=normal&TILEMATRIXSET=PM&FORMAT=image/jpeg&LAYER=ORTHOIMAGERY.ORTHOPHOTOS&TILEMATRIX={z}&TILEROW={y}&TILECOL={x}") %>%
  addPolygons(fillColor = ~factpal(code_group),
              weight = 2,
              opacity = 1,
              color = "#ffd966",
              dashArray = "3",
              fillOpacity = 0.5,
              highlight = highlightOptions(
                weight = 5,
                color = "#A40000",
                dashArray = "",
                fillOpacity = 0.0,
                bringToFront = TRUE),
              label = labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto",
                encoding="UTF-8")) %>% 
  addMarkers(data = pt_mark, ~lon, ~lat, popup = ~coord_pt_gps, label = ~coord_pt_gps)

# Pour sauvegarder la carte
#saveWidget(widget = carte_parc_prox_html, file = "carte_parc_prox.html")

carte_parc_prox_html
