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


###### PARTIE 3 : METEO

library(RPostgres)
library(dplyr)
library(aws.s3)
library(ggplot2)
library(raster)
library(sf)
library(janitor)
library(knitr)

# Pour avoir les noms de dates en français
invisible(Sys.setlocale("LC_ALL", "fr_FR.UTF-8"))

options(knitr.kable.NA = "")

conn <- dbConnect(Postgres(),
                 user = Sys.getenv("USER_POSTGRESQL"),
                 password = Sys.getenv("PASS_POSTGRESQL"),
                 host = Sys.getenv("HOST_POSTGRESQL"),
                 dbname = "defaultdb",
                 port = 5432,
                 check_interrupts = TRUE)

drias_raster <- s3read_using(
  function(f) readAll(brick(f)),
  object = "2023/sujet2/diffusion/resultats/drias.tif",
  bucket = "projet-funathon",
  opts = list("region" = ""))

drias_df <- as.data.frame(drias_raster, xy = TRUE) %>% tidyr::drop_na()
colnames(drias_df) <- c(
  "x",
  "y",
  "NORRRA",
  "NORSTM6",
  "NORSTM0",
  "NORSDA",
  "NORDATEVEG",
  "NORDATEDG",
  "NORDATEPG",
  "ARRA",
  "ASTM6",
  "ASTM0",
  "ASDA",
  "ADATEVEG",
  "ADATEDG",
  "ADATEPG",
  "ALTI"
)

drias_df %>% head(10) %>% kable()


####### 3.2 Visualisations du raster 

drias_raster_arra <- s3read_using(
  function(f) readAll(raster(f, band = 8)),
  object = "2023/sujet2/diffusion/resultats/drias.tif",
  bucket = "projet-funathon",
  opts = list("region" = ""))

# Avec palette custom
palette <- c("#1457ff", "#3c9aff", "#6dc4ff", "#a8e1ff", "#dff1fb", "#f8e9eb", "#fca8ab", "#f9575d", "#f2060b", "#a20102")
breaks <- c(-200, -160, -120, -80, -40, -0, 40, 80, 120, 160, 200)

raster::plot(x = drias_raster_arra,
             col = rev(palette),
             breaks = breaks,
             main = "Ecart de cumul de précipitations d'avril à octobre (mm)\nentre 2021-2050 et 1976-2005")


###### 3.3 Requêtes PostgreSQL

query <- "
SELECT *
FROM drias.previsions
"
drias_sf <- st_read(cnx, query = query)

ggplot() + 
  geom_sf(data = drias_sf, aes(fill = arra), color = NA) +
  binned_scale(aesthetics = "fill", scale_name = "custom", 
               palette = ggplot2:::binned_pal(scales::manual_pal(values = rev(palette)[-1])),
               guide = "bins",
               breaks = breaks)

###### 3.4 Appariement spatial entre données DRIAS et RPG

# On récupère par carreau de la grille DRIAS la surface pour chaque type de culture
query <- "
SELECT B.point, code_cultu, Sum(surf_parc) AS surface, B.arra
FROM rpg.parcelles AS A
JOIN drias.previsions AS B
ON ST_Intersects(A.geom , B.geometry)
GROUP BY B.point, B.arra, code_cultu
"
res <- dbSendQuery(conn, query)
arra_df <- dbFetch(res)

arra_df %>% head(10) %>% kable()


#######   3.5 Calcul d’indicateurs par type de culture

# Récupération des libellés des codes culture
culture_mapping <- s3read_using(
  FUN = read.csv,
  sep = ";",
  object = "2023/sujet2/diffusion/ign/rpg/CULTURE.csv",
  bucket = "projet-funathon",
  opts = list("region" = "")
)

# On aggrège au niveau national par code culture et on calcule un écart
# moyen du cumul par m2
agg_arra_df <- arra_df %>%
  group_by(code_cultu) %>%
  summarise(ecart_volume_precip = sum(surface * arra), surface = sum(surface)) %>%
  mutate(ecart_cumul_moyen = ecart_volume_precip / surface)

# Récupération des 10 cultures avec une forte perte de précipitation
agg_arra_df %>%
  dplyr::left_join(culture_mapping, by = c("code_cultu" = "Code")) %>%
  arrange(ecart_cumul_moyen) %>%
  head(10) %>%
  kable()


# Frontières régionales de métropole
region_sf <- st_read(
  conn, query = "SELECT * FROM adminexpress.region"
)
region_sf <- region_sf %>% st_transform(
  "EPSG:2154"
)
region_sf <- region_sf %>%
  dplyr::filter(!(insee_reg %in% c("03", "04", "06", "01", "02", "01_SBSM")))

# Parcelles de maïs doux
query_mid <- "
SELECT id_parcel, geom
FROM rpg.parcelles
WHERE code_cultu = 'MID'
"
cultures_mid <- st_read(conn, query = query_mid)
ggplot() + 
  geom_sf(data = region_sf) +
  geom_sf(data = st_buffer(cultures_mid, 5000), fill = "#fca8ab", color = NA)

###############  4  Évolution de la date théorique de récolte
