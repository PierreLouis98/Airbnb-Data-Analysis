library(shiny)
library(dplyr)
library(stringr)
library(ggplot2)
library(googleVis)
library(glue)
library(data.table)

source("../utils/data_preparation.R")

print("Start downloading...")
listings <- load_global_listings()
print("Done !")

server <- function(input, output) {
  
# PREMIER ONGLET
  
  ############### REACTIVE ##############
  
  listings_country <- reactive({
    listings %>% 
      filter(country == input$pays_selection)
  })
  
  listings_city <- reactive({
    if (is.null(input$villes)){
      return(NULL)
    }
    listings_country() %>% 
      filter(city %in% input$villes) 
  })
  
  listings_city_selected <- reactive({
    df <- listings_city()
    if (is.null(df)) {
      return(NULL)
    }
    dates_minmax <- input$dates_minmax
    if (is.null(dates_minmax)) {
      return(NULL)
    }
    df$date <- as.Date(df$date)
    result <- subset(df, date >= dates_minmax[1] & date <= dates_minmax[2])
    return(result)
  })
  
  listings_city_multiple_features <- reactive({
    if (is.null(input$villes) | is.null(input$fonction_2)){
      return(NULL)
    }
    listings_city_selected() %>% 
      filter(!is.na(!! rlang::sym(input$fonction_2)))
  })
  
  ############### UI ##############
  
  output$villes_selection <- renderUI({ 
    choix <- unique(listings_country()$city)
    checkboxGroupInput("villes", "Selectionner une ou plusieurs villes :", choices = choix)
  })
  
  output$dates <- renderUI({
    df2 <- listings_city()
    dates <- as.Date(df2$date)
    minmax <- range(df2$date, na.rm = TRUE)
    dateRangeInput(inputId="dates_minmax", h3("Indiquer une date min et max :"), min = minmax[1], start = minmax[1], max = minmax[2], end = minmax[2])
  })
  
  ############### PLOTS ##############
  
  output$fonction_1_moy <- renderGvis({
    if (is.null(listings_city_selected()))
      return(NULL)
    mean <- listings_city_selected() %>%
      group_by(city) %>%
      summarize(mean=round(mean(!! rlang::sym(input$fonction_1), na.rm = TRUE),2), .groups = 'keep')
    
    gvisColumnChart(mean, xvar='city', yvar='mean', 
                    options = list(width = 600, height = 350, title = glue("Moyenne de la fonction_1 choisie par ville"), colors="['#603913']"))
  })
  
  
  output$fonction_1_2_distribution <- renderPlot({
    if (is.null(listings_city_multiple_features()))
      return(NULL)
    
    ggplot(listings_city_multiple_features(), aes(city, listings_city_multiple_features()[[input$fonction_1]])) + 
      geom_boxplot(aes(colour = !! rlang::sym(input$fonction_2)), na.rm = TRUE) + 
      ggtitle(glue("Distribution de fonction_1 par ville en fonction de fonction_2")) + 
      xlab("Ville") + 
      ylab(glue("{str_replace(input$fonction_1,'_', ' ')}"))
  })
  
  
# DEUXIEME ONGLET
  
  ############### REACTIVE ##############
  
  listings_city_tab2 <- reactive({
    if (is.null(input$villes2)){
      return(NULL)
    }
    
    listings_country() %>% 
      filter(city == input$villes2)
  })
  
  listings_city_selected_tab2 <- reactive({
    df3 <- listings_city_tab2()
    if (is.null(df3)) {
      return(NULL)
    }
    
    range <- input$dates_minmax2
    if (is.null(range)) {
      return(NULL)
    }
    
    df3$date <- as.Date(df3$date)
    result <- subset(df3, date >= range[1] & date <= range[2])
    return(result)
  })
  
  listings_city_tab2_feature <- reactive({
    if (is.null(listings_city_selected_tab2()) | is.null(input$min_f1) | is.null(input$max_f1)){
      return(NULL)
    }
    listings_city_selected_tab2() %>% 
      filter(!! rlang::sym(input$f1) <= input$max_f1 & !! rlang::sym(input$f1) >= input$min_f1)
  })
  
  ############# UI ##############
  
  output$villes_selection2 <- renderUI({ 
    choice_tab2 <- unique(listings_country()$city)
    selectInput("villes2", h3("Choisir une ville :"), choices = choice_tab2, selected = NULL)
  })
  
  output$dates2 <- renderUI({
    df4 <- listings_city_tab2()
    dates <- as.Date(df4$date)
    minmax <- range(df4$date, na.rm = TRUE)
    dateRangeInput(inputId="dates_minmax2", h3("Indiquer une date min et max :"), min = minmax[1], start = minmax[1], max = minmax[2], end = minmax[2])
  })
  
  output$f1_minmax <- renderUI({ 
    quant <- quantile(listings_city_selected_tab2()[[input$f1]], na.rm = TRUE)
    
    fluidRow(
      column(6, numericInput("min_f1", label = "Min (fonction 1):", value = quant[1])),
      column(6, numericInput("max_f1", label = "Max (fonction 1):", value = quant[3])),
      column(12, numericInput("nb_loc", label = "Nombre de localisation:", value = 40))
    )
  })
  
  output$sous_titre <- renderUI({
    minmax_feat <- range(listings_city_selected_tab2()[[input$f1]], na.rm = T)
    p(glue("Utilisez au minimum {nrow(listings_city_tab2_feature())} localisations et fonction_1 entre {minmax_feat[1]} et {minmax_feat[2]}."))
  })
  
  output$titre_carte <- renderUI({
    h4(glue("Carte de {input$villes2} dependant de la fonction_1"))
  })
  
  ############### PLOTS ##############
  
  output$carte <- renderGvis({
    #Sys.sleep(0.2)
    if (is.null(listings_city_tab2_feature())){
      return(NULL)
    }
    if(input$nb_loc > nrow(listings_city_tab2_feature())){
      return(NULL)
    }
    gvisMap(sample_n(listings_city_tab2_feature(), input$nb_loc), locationvar="latitudelongitude" , tipvar=input$f1, 
            options=list(width=200, 
                         height=300,
                         showTip=TRUE, 
                         showLine=TRUE, 
                         enableScrollWheel=TRUE,
                         mapType='terrain', 
                         useMapTypeControl=TRUE))
  })
  
  output$proportion <- renderGvis({
    if (is.null(listings_city_tab2_feature()) | is.null(input$f2)){
      return(NULL)
    }
    feature <- input$f2
    df5 <- listings_city_tab2_feature()[feature]
    listings_top <- top_n(as.data.frame(sort(table(na.omit(df5)), decreasing=T)), 20)
    if (length(names(listings_top)) != length(c(feature, 'freq'))){
      return(NULL)
    }
    names(listings_top) <- c(feature, 'freq')
    listings_top['freq'] <- listings_top['freq'] / sum(listings_top['freq'])
    
    gvisColumnChart(listings_top, xvar=feature, yvar='freq', 
                    options = list(width = 650, height = 350, title = glue("Proportion de chaque valeur de fonction_2"), colors="['#603913']"))
  })
  
}