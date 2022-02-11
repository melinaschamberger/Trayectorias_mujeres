# Trayectorias académicas y laborales de las mujeres en Argentinas
# Por Melina Schamberger y Natasha Siderman
# ui.R

#Librerías
library(leaflet)
library(shinydashboard)
library(collapsibleTree)
library(shinycssloaders)
library(DT)
library(tigris)

#Datos
#Sector productivo: valores anuales
df_juntos <- read.csv("https://raw.githubusercontent.com/melinaschamberger/Trayectorias_mujeres/main/Datos/Mujeres_sector_privado/Sector_privado_final.csv", 
                      encoding = "Latin1")

#Formatos


shinyUI(fluidPage(
  
  # load custom stylesheet
  includeCSS("www/style.css"),
  
  # load page layout
  dashboardPage(
    
    skin = "purple",
    
    dashboardHeader(title="", titleWidth = 350),
    
    dashboardSidebar(width = 350,
                     sidebarMenu(
                       HTML(paste0(
                         "<br>",
                         "<img style = 'display: block'; src='R_day_dos.png' width = '350'></a>",
                         "<br>",
                         "<p style = 'text-align: center;'><small><a href='https://www.linkedin.com/in/melina-schamberger' target='_blank'>Melina Schamberger</a></small></p>",
                         "<p style = 'text-align: center;'><small><a href='https://www.linkedin.com/in/natashasiderman' target='_blank'>Natasha Siderman</a></small></p>",
                         "<br>"
                       )),
                       menuItem("Inicio", tabName = "home", icon = icon("home")),
                       menuItem("Trayectorias académicas", tabName = "map", icon = icon("graduation-cap")),
                       menuItem("Trayectorias laborales", tabName = "table", icon = icon("suitcase"), 
                                menuSubItem("Inserción laboral", tabName = "sub_1"),
                                menuSubItem("Salarios", tabName = "sub_2")),
                       menuItem("Referencias", tabName = "releases", icon = icon("tasks")))), # end dashboardSidebar
    
    dashboardBody(
      
      tabItems(
        
        tabItem(tabName = "home",
                
                # home section
                includeMarkdown("www/home.md")),
        
        tabItem(tabName = "map",
                
                # parks map section
                leafletOutput("parksMap") %>% withSpinner(color = "green")),
        
        tabItem(tabName = "table", dataTableOutput("speciesDataTable") %>% withSpinner(color = "green")),
        
        tabItem(tabName = "releases", includeMarkdown("www/releases.md")),
        
        tabItem(tabName = "sub_1", 
          fluidRow(
            HTML(paste0(
            "<div style='text-align: justify; font-size:24pt; font-family:Encode Sans semiBold; color:#455B6C'><strong>Inserción laboral de las mujeres</strong></div>")),
            br(),     
            selectInput(inputId = "sector_productivo",
                             label = strong("Seleccione el sector productivo:"),
                             choices = unique(df_juntos$clae2_desc),
                             selected = "Todos"
                             ),
            plotlyOutput(outputId = "graf_uno_sp", #height = 300, 
                         #width = 500
                         )
                 )),
                
        tabItem(tabName = "sub_2", fluidRow(h1("jeje")))
        
        )
      
    ) # end dashboardBody
    
  )# end dashboardPage
  
))