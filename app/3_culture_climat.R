# programme raster climat partie 3 2 paramere-----------------

library(RPostgres)
library(sf)
library(janitor)
library(aws.s3)
library(DBI)
library(kableExtra)
library(leaflet)
library(htmlwidgets)
library(RColorBrewer)
library( tidyverse)
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

cnx <- dbConnect(Postgres(),
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
# visualisation du raster -----------------
# Bande ARRA
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

---------------------------------------------------------------------------
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



# 3.4 Appariement spatial entre données DRIAS et RPG ----------------------

# On récupère par carreau de la grille DRIAS la surface pour chaque type de culture
query <- "
SELECT B.point, code_cultu, Sum(surf_parc) AS surface, B.arra
FROM rpg.parcelles AS A
JOIN drias.previsions AS B
ON ST_Intersects(A.geom , B.geometry)
GROUP BY B.point, B.arra, code_cultu
"
res <- dbSendQuery(cnx, query)
arra_df <- dbFetch(res)

arra_df %>% head(10) %>% kable()
# 3.5 recuperation indicateur ---------------------------------------------
---------------------------------------------------------------------
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
------------------------------------------------------------------------
# Frontières régionales de métropole
region_sf <- st_read(
  cnx, query = "SELECT * FROM adminexpress.region"
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
cultures_mid <- st_read(cnx, query = query_mid)
ggplot() + 
  geom_sf(data = region_sf) +
  geom_sf(data = st_buffer(cultures_mid, 5000), fill = "#fca8ab", color = NA) 






