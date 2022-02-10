# Trayectorias académicas y laborales de las mujeres en Argentinas
# Por Melina Schamberger y Natasha Siderman
# ui.R

library(leaflet)
library(shinydashboard)
library(collapsibleTree)
library(shinycssloaders)
library(DT)
library(tigris)

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
        
        tabItem(tabName = "sub_1", fluidRow(h1("jaja"))),
                
        tabItem(tabName = "sub_2", fluidRow(h1("jeje")))
        
        )
      
    ) # end dashboardBody
    
  )# end dashboardPage
  
))