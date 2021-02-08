library(shiny)
library(googleVis)
library(dplyr)
library(stringr)
library(ggplot2)
library(data.table)
library(glue)
library(rmarkdown)

ui <- fluidPage(
  
  tabsetPanel(
    
# PREMIER ONGLET: COMPARAISON DES VILLES
    
    tabPanel("Analyse 1 - Comparaison des villes",
             sidebarLayout(
               sidebarPanel(
                 selectInput("pays_selection", 
                             h3("Choisir un pays :"), 
                             choices = list("France" = "france",
                                            "Espagne"= "spain",
                                            "Belgique"= "belgium",
                                            "Pays-Bas" = "the-netherlands",
                                            "Allemagne"= "germany",
                                            "Italie"= "italy"
                             ),
                             selected = "belgium"),
                 
                 uiOutput("villes_selection"),
                 
                 selectInput("fonction_1", 
                             h3("Choisir fonction 1 :"), 
                             choices = list("Revenu (en dollars)" = "revenue_30", 
                                            "Prix (en dollars)" = "price_30",
                                            "Disponibilite (en jours)" = "availability_30",
                                            "Nombre de nuits max" = "maximum_nights",
                                            "Nombre de nuits min" = "minimum_nights",
                                            "Note recue (en pourcentage)" = "review_scores_rating"
                             ),
                             selected = "availability_30"),
                 
                 selectInput("fonction_2", 
                             h3("Choisir fonction 2 :"), 
                             choices = list("Type de chambre" = "room_type", 
                                            "Nombre de chambre" = "bedrooms"
                             ),
                             selected = "bedrooms"),
                 
                 uiOutput("dates"),
                 
               ),
               mainPanel(
                 htmlOutput("fonction_1_moy"),
                 br(),
                 hr(),
                 plotOutput("fonction_1_2_distribution")
               ),
             )),
    
# DEUXIEME ONGLET: ANALYSE APPROFONDIE D'UNE VILLE

    tabPanel("Analyse 2 - Analyse approfondie d'une ville",
             sidebarLayout(
               sidebarPanel(

                 uiOutput("villes_selection2"),
                 
                 selectInput("f1",
                             h3("Choisir fonction 1 :"), 
                             choices = list("Revenu (en dollars)" = "revenue_30", 
                                            "Prix (en dollars)" = "price_30",
                                            "Disponibilite (en jours)" = "availability_30",
                                            "Nombre de nuits max" = "maximum_nights",
                                            "Nombre de nuits min" = "minimum_nights",
                                            "Note recue (en pourcentage)" = "review_scores_rating"
                                            
                             ),
                             selected = "revenue_30"),
                 
                 selectInput("f2",
                             h3("Choisir fonction 2 :"), 
                             choices = list("Type de chambre" = "room_type", 
                                            "Nombre de chambre" = "bedrooms",
                                            "Quartier" = "neighbourhood_cleansed"
                                            
                             ),
                             selected = "room_type"),
                 
                 uiOutput("dates2"),
                 
                 hr(),
                 h3("Carte :"),
                 uiOutput("f1_minmax"),
                 hr(),
               ),
               mainPanel(
                 br(),
                 uiOutput("titre_carte"),
                 uiOutput("sous_titre"),
                 htmlOutput("carte"),
                 br(),
                 fluidRow(
                   column(6, htmlOutput("proportion")),
                 )
               ),
             )
    )
  )
)