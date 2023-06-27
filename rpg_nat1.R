library(RPostgres)
library(dplyr)
library(knitr)
library(sf)
library(janitor)
library(aws.s3)
library(DBI)
library(kableExtra)
library(leaflet)
library(htmlwidgets)
library(RColorBrewer)
library( tidyverse)
conn <- dbConnect(Postgres(),
                  user = Sys.getenv("USER_POSTGRESQL"),
                  password = Sys.getenv("PASS_POSTGRESQL"),
                  host = Sys.getenv("HOST_POSTGRESQL"),
                  dbname = "defaultdb",
                  port = 5432,
                  check_interrupts = TRUE)
lon <--48.01469669407841
lat  <- -4.105676552817091
x<-48.01469669407841
y<- -4.105676552817091
rayon<-10000

point <- data.frame(y, x, rayon) %>% 
  st_as_sf(coords = c("y","x"), crs = "EPSG:4326") %>%
  mutate(coord_pt_gps = st_as_text(geometry)) %>% 
  st_transform("EPSG:2154") %>% 
  st_sf() %>%
  clean_names() %>% 
  rename(geom = geometry)


point_nlc <- data.frame(y, x, rayon) %>% 
  st_as_sf(coords = c("y","x"), crs = "EPSG:4326")

%>% mutate(coord_pt_gps = st_as_text(geometry)) %>% 
  
  point_l93<-st_transform(point,crs=2154)%>% 
    rename(geom = geometry)
plot(point_l93)
str(point_l93)
class(point_l93)
dbListTables(conn)

# Ecriture de la table point dans une table PostGIS
write_sf(point, conn, Id(schema = "public", table = "point_nlc"))

query_nlc <- "SELECT point_nlc.*,parcelles.*
    FROM public.point_nlc, rpg.parcelles 
    WHERE ST_DWithin(point_nlc.geom, parcelles.geom, point_nlc.rayon);"

parc_prox <- st_read(conn, query = query_nlc)
plot(st_geometry(parc_prox_nlc))
#-----------------------------------------------------
query1 <- "SELECT row_number()  OVER () AS row_id,  point_nlc.rayon, parcelles.*  
    FROM public.point_nlc, rpg.parcelles 
    WHERE ST_DWithin(point_nlc.geom, parcelles.geom,point_nlc.rayon);"

parc_prox <- st_read(conn, query = query1)
# etape 2.13 visualisation des donnees-------------------------
lib_cult <- s3read_using(FUN = read_csv2,
                         object = "2023/sujet2/diffusion/ign/rpg/REF_CULTURES_GROUPES_CULTURES_2020.csv",
                         col_types = cols(.default = col_character()),
                         bucket = "projet-funathon",
                         opts = list("region" = "")) %>% clean_names()


lib_group_cult <- lib_cult %>% 
  select(code_groupe_culture, libelle_groupe_culture) %>% 
  distinct(code_groupe_culture, .keep_all=T)

lib_group_cult %>% kable()
# etape 2.14 visualisation des donnees-------------------------
# Création d'une palette de couleurs associée au groupe de culture
factpal <- colorFactor("Paired", parc_prox_nlc$code_group)

# Transformation de la projection car leaflet ne connait que le WGS 84
parc_prox <- parc_prox_nlc %>% st_transform(4326)

# Pour ajouter un marqueur du point
pt_mark <- point %>% st_transform(4326)

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

# 2.15carte------------------
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
