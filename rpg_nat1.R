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
# etape 2.13 parametrage affichage -------------------------
lib_cult <- s3read_using(FUN = read_csv2,
                         object = "2023/sujet2/diffusion/ign/rpg/REF_CULTURES_GROUPES_CULTURES_2020.csv",
                         col_types = cols(.default = col_character()),
                         bucket = "projet-funathon",
                         opts = list("region" = "")) %>% clean_names()


lib_group_cult <- lib_cult %>% 
  select(code_groupe_culture, libelle_groupe_culture) %>% 
  distinct(code_groupe_culture, .keep_all=T)

lib_group_cult %>% kable()
# etape 2.14 palette couleur-------------------------
# Création d'une palette de couleurs associée au groupe de culture
factpal <- colorFactor("Paired", parc_prox$code_group)

# Transformation de la projection car leaflet ne connait que le WGS 84
parc_prox <- parc_prox %>% st_transform(4326)

# Pour ajouter un marqueur du point
pt_mark <- point_nlc %>% st_transform(4326)

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
carte_parc_prox_html

#2.1.6 parcelles recuperees---------------------------
t1 <- parc_prox %>%
  st_drop_geometry() %>%
  count(code_group) %>% 
  add_tally(n) %>% 
  mutate(n_pct = round(100 * n / nn, 1)) %>% 
  select(-nn) %>%
  rename(n_parcelles = n) %>%
  cbind(
    # Surfaces
    parc_prox %>%
      st_drop_geometry() %>%
      count(code_group, wt = surf_parc) %>% 
      add_tally(n) %>% 
      mutate(surf_pct = round(100 * n / nn, 1)) %>%
      select(-nn) %>%  
      rename(surf_parc_ha = n) %>%
      select(surf_parc_ha, surf_pct)
  ) %>%
  left_join(lib_group_cult, by = c("code_group" = "code_groupe_culture")) %>% 
  select(code_group, libelle_groupe_culture, everything()) %>% 
  arrange(desc(surf_parc_ha)) %>% 
  adorn_totals() %>% 
  mutate(taille_moy_parc = round(surf_parc_ha / n_parcelles, 1))

t1  %>% 
  setNames(c("Code", "Groupe de cultures", "Nombre de parcelles", "(%)", "Surface (ha)", "Surface (%)", "Taille moyenne (ha)")) %>% 
  kable(
    format="html",
    caption="<span style='font-size:medium'>Groupes de cultures <strong>locales</strong> par surfaces décroissantes</span>",
    format.args = list(decimal.mark = ",", big.mark = " "),
    booktabs = TRUE) %>%
  kable_styling(font_size = 15) %>% 
  gsub("font-size: initial !important;",
       "font-size: 20pt !important;",.)%>%
  kable_styling(bootstrap_options = c("striped", "hover")) %>% 
  row_spec(nrow(t1), bold = T, color = "white", background = "grey")
-------------------------------------
  # Couche département pour récupérer le département du point
  dep <- s3read_using(
    FUN = sf::read_sf,
    layer = "departement",
    object = "2023/sujet2/diffusion/ign/adminexpress_cog_simpl_000_2023.gpkg",
    bucket = "projet-funathon",
    opts = list("region" = "")) %>% 
  st_transform(2154)
# Jointure spatiale
df <- point %>% st_join(dep) %>% st_drop_geometry() %>% select(insee_dep)
dep_pt <- df[1,1]
# Récupération des statistiques départementales
stat_dep_pt <- s3read_using(
  FUN = readr::read_rds,
  object = "2023/sujet2/diffusion/resultats/stat_group_cult_by_dep.rds",
  bucket = "projet-funathon",
  opts = list("region" = ""))

# Récupération des statistiques nationales
stat_fm <- s3read_using(
  FUN = readr::read_csv,
  object = "2023/sujet2/diffusion/resultats/stat_group_cult_fm.csv",
  col_types = cols(code_group = col_character()),
  bucket = "projet-funathon",
  opts = list("region" = "")) %>% 
  select(code_group, libelle_groupe_culture, pct_surf) %>% 
  rename(pct_surf_fm = pct_surf)

#recuperation stat departementale-------------------------------

# Calcul des % surfaces autour du point
stat_pt <- parc_prox %>%
  st_drop_geometry() %>% 
  count(code_group, wt = surf_parc) %>%
  add_tally(n) %>% 
  mutate(pct_surf_local = round(100 * n / nn, 1)) %>%
  select(code_group, pct_surf_local) 

# Récupération des statistiques du département concerné
stat_dep_pt <- stat_dep_pt %>% 
  filter(insee_dep %in% dep_pt) %>% 
  select(insee_dep, code_group, libelle_groupe_culture, pct_surf) %>% 
  rename(pct_surf_dep = pct_surf)

# Appariement des statistiques locale, départementale et nationale
stat_compar <- stat_fm %>% 
  left_join(stat_dep_pt %>% select(code_group, pct_surf_dep), by = "code_group") %>% 
  left_join(stat_pt , by = "code_group") %>% 
  select(libelle_groupe_culture, pct_surf_local, pct_surf_dep, pct_surf_fm) %>% 
  arrange(desc(pct_surf_local)) %>%
  adorn_totals() 

stat_compar %>% 
  setNames(c("Groupe de cultures","Surf. locales (%)", "Surf. départ. (%)","Surf. France m. (%)")) %>%
  kable(
    format="html",
    caption="<span style='font-size:medium'>Comparaison des surfaces locales, départementales et nationales</span>",
    format.args = list(decimal.mark = ",", big.mark = " "),
    booktabs = TRUE) %>%
  kable_styling(font_size = 15) %>% 
  gsub("font-size: initial !important;",
       "font-size: 20pt !important;",.)%>%
  kable_styling(bootstrap_options = c("striped", "hover")) %>% 
  row_spec(nrow(stat_compar), bold = T, color = "white", background = "grey")

# graphique des cultures------------------
# Sélection des 10 groupes de cultures les plus répandus au niveau local 
tab <- stat_compar %>%
  filter(libelle_groupe_culture != "Total") %>%
  slice_head(n=10) %>% 
  rename(local = pct_surf_local, departement = pct_surf_dep, france = pct_surf_fm)

# Transposition de la table pour rassembler toutes les valeurs dans une seule variable value
tab_piv <- tab %>% pivot_longer(!libelle_groupe_culture) %>% rename(secteur = name) 

# Valeurs manquantes à
tab_piv[is.na(tab_piv)] <- 0

# On réordonne les secteurs dans le "bon" ordre, avec factor
tab_piv$secteur <- factor(
  tab_piv$secteur,
  levels = c("france", "departement", "local"))
tab_piv <- tab_piv %>% arrange(desc(secteur), desc(value))

# On réordonne les cultures par surface décroissante au niveau local, avec factor
x <- tab_piv %>% filter(secteur == "local") %>% arrange(value) %>% select(libelle_groupe_culture)
y <- pull(x, libelle_groupe_culture)

tab_piv$libelle_groupe_culture <- factor(tab_piv$libelle_groupe_culture, levels = y)

# Visualisation avec `geom_col`
p <- ggplot(tab_piv, aes(x = libelle_groupe_culture,
                         y = value, 
                         fill = factor(
                           secteur,
                           levels = c("france", "departement", "local")))) + 
  geom_col(position = "dodge") +
  labs(title = "Surfaces comparées des 10\nprincipales cultures locales, en %", x="Culture", y = "%", fill = "Secteur") +
  theme_classic()

# Flip du graphique pour avoir des barres horizontales  
p + coord_flip()

# Visualisation avec `geom_col` et `facet_wrap`   
ggplot(tab_piv, 
       aes(x = libelle_groupe_culture,
           y = value)) + 
  geom_col(fill = "lightblue", colour = "black", position = "dodge") +
  labs(title = "Surface par culture", x= "Culture", y = "%", fill = "Secteur") +
  geom_text(aes(label = value), hjust = -0.3, size = 10/.pt, colour = "black") +
  theme_classic() + coord_flip() + 
  facet_wrap(~secteur, nrow=3, scales='free')

# dashboard shiny`---------------
library(shiny)

# Define UI
ui <- fluidPage(
  leafletOutput("map", height = 800),
  numericInput("buffer_radius", "Rayon (en km) :", value = 5),
  tableOutput("table")
)

-----------------------------------
  library(shiny)
library(leaflet)


# Définition du serveur
server <- function(input, output) {
  
  # Rendre la carte
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles("http://wxs.ign.fr/essentiels/wmts?REQUEST=GetTile&SERVICE=WMTS&VERSION=1.0.0&STYLE=normal&TILEMATRIXSET=PM&FORMAT=image/jpeg&LAYER=ORTHOIMAGERY.ORTHOPHOTOS&TILEMATRIX={z}&TILEROW={y}&TILECOL={x}") %>%
      setView(lng = -1.4932577154775046, lat = 46.46946179131805, zoom = 12)
  })
  
  # Connexion à la base de données
  cnx <- connect_to_db()
  
  # Initialisation d'une "reactive value" pour le point sélectionné
  selectedPoint <- reactiveValues(lat = NULL, lng = NULL)
  
  # Gestion de l'évènement "clic"
  observeEvent(input$map_click, {
    
    clickData <- input$map_click
    if (!is.null(clickData)) {
      # Stockage des coordonnées du point
      selectedPoint$lat <- clickData$lat
      selectedPoint$lng <- clickData$lng
      
      buffer_radius <- input$buffer_radius
      sf <- query_db(cnx, selectedPoint$lat, selectedPoint$lng, buffer_radius)
      
      # Mise à jour de la carte
      leafletProxy("map") %>%
        clearMarkers() %>%
        clearShapes() %>%
        addMarkers(lng = selectedPoint$lng, lat = selectedPoint$lat) %>%
        plot_surroundings(sf)
      
      # Calculs sur les données de parcelles
      df <- compute_stats(sf)
      
      # Update de la table affichée
      output$table <- renderTable({
        df
      })
    }
  })
  
  observeEvent(input$buffer_radius, {
    # Vérification qu'un point a été sélectionné
    if (!is.null(selectedPoint$lat) && !is.null(selectedPoint$lng)) {
      # Requête avec le nouveau rayon
      buffer_radius <- input$buffer_radius
      sf <- query_db(cnx, selectedPoint$lat, selectedPoint$lng, buffer_radius)
      
      # Mise à jour de la carte
      leafletProxy("map") %>%
        clearShapes() %>%
        plot_surroundings(sf)
      
      # Calculs sur les données de parcelles
      df <- compute_stats(sf)
      
      # Update de la table affichée
      output$table <- renderTable({
        df
      })
    }
  })
} 

#' Connection au serveur PostgreSQL. Le mot de passe doit être stocké dans la--------
#' variable d'environnement PASS_POSTGRESQL.
#' 
#' @returns Connexion au serveur.
connect_to_db <- function() {
  # Connection à PostgreSQL
  cnx <- dbConnect(Postgres(),
                   user = Sys.getenv("USER_POSTGRESQL"),
                   password = Sys.getenv("PASS_POSTGRESQL"),
                   host = "postgresql-758156.projet-funathon",
                   dbname = "defaultdb",
                   port = 5432,
                   check_interrupts = TRUE)
  
  return(cnx)
}
#' Requête la table `parcelles` pour récupérer les parcelles qui se situent
#' dans un certain rayon autour d'un point repéré par une latitude et 
#' une longitude.
#' 
#' @param cnx Connexion à PostgreSQL.
#' @param lat Latitude.
#' @param lng Longitude.
#' @param radius Rayon.
#' @returns Objet `sf` avec les données des parcelles concernées.
query_db <- function(cnx, lat, lng, radius) {
  ...
}

-------------------------------------------------
  #' Requête la table `parcelles` pour récupérer les parcelles qui se situent
  #' dans un certain rayon autour d'un point repéré par une latitude et 
  #' une longitude.
  #' 
  #' @param cnx Connexion à PostgreSQL.
  #' @param lat Latitude.
  #' @param lng Longitude.
  #' @param radius Rayon.
  #' @returns Objet `sf` avec les données des parcelles concernées.
  query_db <- function(cnx, lat, lng, radius) {
    # Les données spatiales sur PostgreSQL sont stockées en Lambert 93.
    # Pour faire le join on veut donc projeter les coordonnées `lat`` et `lng`
    postgis_crs <- "EPSG:2154"
    coordinates <- data.frame(lng = c(lng), lat = c(lat)) %>%
      st_as_sf(coords = c("lng", "lat"), remove = TRUE) %>%
      st_set_crs("EPSG:4326") %>%
      st_transform(postgis_crs)
    
    # Requête PostgreSQL
    query <- sprintf(
      "SELECT * FROM rpg.parcelles WHERE ST_Intersects(geom, ST_Buffer(ST_SetSRID(ST_MakePoint(%f, %f), 2154), %.0f));",
      st_coordinates(coordinates)[1],
      st_coordinates(coordinates)[2],
      radius*1000)
    
    # Récupération des résultats
    sf <- st_read(
      cnx,
      query = query
    )
    
    return(sf)
  }


#2.4.3 compute_stats-----------------------------
  #' Crée la table à afficher sur l'application grâce à des calculs sur les
  #' données requêtées depuis PostgreSQL.
  #' 
  #' @param sf Données spatiales.
  #' @returns data.frame à afficher.
  compute_stats <- function(sf) {
    df <- sf %>% 
      st_drop_geometry() %>%
      count(code_group, name = "parcelles_grp") %>%
      add_tally(parcelles_grp, name = "parcelles_tot") %>%
      mutate(pct_parcelles = round(100*parcelles_grp/parcelles_tot, 1)) %>%
      select(-parcelles_tot) %>%
      cbind(
        # Comptage des surfaces
        sf %>% 
          st_drop_geometry() %>%
          count(code_group, wt = surf_parc, name = "surface_grp") %>% 
          add_tally(surface_grp, name = "surface_tot") %>% 
          mutate(surface_pct = round(100*surface_grp/surface_tot, 1)) %>%
          select(-surface_tot) %>%
          select(surface_grp, surface_pct)
      ) %>% 
      left_join(lib_group_cult, by = c("code_group" = "code_groupe_culture")) %>% 
      select(code_group, libelle_groupe_culture, everything()) %>% 
      arrange(desc(surface_grp)) %>% 
      adorn_totals() %>% 
      mutate(mean_surface = round(surface_grp/parcelles_grp, 1))
    
    return(
      df %>%
        select(-code_group) %>%
        setNames(
          c(
            "Groupe de cultures",
            "Nombre de parcelles",
            "Pourcentage de parcelles",
            "Surface (ha)",
            "Surface (%)",
            "Surface moyenne d'une parcelle (ha)"))
    )
  }  

# 2.4.4 plot_surrounding-------------------------------
# Création d'une palette de couleurs associée au groupe de culture
pal <- brewer.pal(12, "Paired")
pal <- colorRampPalette(pal)(24)
factpal <- colorFactor(pal, lib_group_cult$code_groupe_culture)

#' Rajoute les données d'un objet `sf` sous forme de polygones à une
#' carte `leaflet`.
#' 
#' @param leaflet_proxy Carte.
#' @param sf Données spatiales.
#' @returns Carte enrichie.
plot_surroundings <- function(leaflet_proxy, sf) {
  # Transformation de la projection (WGS 84)
  sf <- sf %>% st_transform(4326)
  
  # Ajout des libellés des cultures
  sf <- sf %>% 
    left_join(lib_cult %>% select(-code_groupe_culture), by = c("code_cultu" = "code_culture")) 
  
  # Création des labels à afficher au passage de la souris sur la carte.
  labels <- sprintf("<strong>Identifiant de la parcelle : </strong>%s<br/>
                    <strong>Groupe culture : </strong>%s<br/>
                    <strong>Culture : </strong>%s<br/>
                    <strong>Surface (ha) : </strong>%s<br/>
                    <strong>Département : </strong>%s<br/>
                    <strong>Commune : </strong>%s<br/>",
                    sf$id_parcel,
                    sf$libelle_groupe_culture,
                    sf$libelle_culture,
                    sf$surf_parc,
                    sf$insee_dep,
                    sf$nom_com) %>%
    lapply(htmltools::HTML)
  
  return(
    leaflet_proxy %>%
      addPolygons(
        data = sf,
        fillColor = ~factpal(code_group),
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
          encoding="UTF-8"))
  )
}

shiny::runApp("my-app")
