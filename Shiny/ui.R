##################################
# Biodiversity in National Parks #
# by Alessio Benedetti           #
# ui.R file                      #
##################################

library(leaflet)
library(shinydashboard)
library(collapsibleTree)
library(shinycssloaders)
library(DT)
library(tigris)

###########
# LOAD UI #
###########

shinyUI(fluidPage(
  
  # load custom stylesheet
  includeCSS("www/style.css"),
  
  # load google analytics script
  tags$head(includeScript("www/google-analytics-bioNPS.js")),
  
  # remove shiny "red" warning messages on GUI
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),
  
  # load page layout
  dashboardPage(
    
    skin = "purple",
    
    dashboardHeader(title=" ", titleWidth = 350),
    
    dashboardSidebar(width = 350,
                     sidebarMenu(
                       HTML(paste0(
                         "<br>",
                         "<a href='https://www.nps.gov/index.htm' target='_blank'><img style = 'display: block'; src='R_day_dos.png' width = '350'></a>",
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
                       menuItem("Species Tree", tabName = "tree", icon = icon("random", lib = "glyphicon")),
                       menuItem("Species Charts", tabName = "charts", icon = icon("stats", lib = "glyphicon")),
                       menuItem("Species Choropleth Map", tabName = "choropleth", icon = icon("map marked alt")),
                       menuItem("Referencias", tabName = "releases", icon = icon("tasks")),
                       HTML(paste0(
                         "<br><br><br><br><br><br><br><br><br>",
                         "<table style='margin-left:auto; margin-right:auto;'>",
                         "<tr>",
                         "<td style='padding: 5px;'><a href='https://www.facebook.com/nationalparkservice' target='_blank'><i class='fab fa-facebook-square fa-lg'></i></a></td>",
                         "<td style='padding: 5px;'><a href='https://www.youtube.com/nationalparkservice' target='_blank'><i class='fab fa-youtube fa-lg'></i></a></td>",
                         "<td style='padding: 5px;'><a href='https://www.twitter.com/natlparkservice' target='_blank'><i class='fab fa-twitter fa-lg'></i></a></td>",
                         "<td style='padding: 5px;'><a href='https://www.instagram.com/nationalparkservice' target='_blank'><i class='fab fa-instagram fa-lg'></i></a></td>",
                         "<td style='padding: 5px;'><a href='https://www.flickr.com/nationalparkservice' target='_blank'><i class='fab fa-flickr fa-lg'></i></a></td>",
                         "</tr>",
                         "</table>",
                         "<br>"),
                         HTML(paste0(
                           "<script>",
                           "var today = new Date();",
                           "var yyyy = today.getFullYear();",
                           "</script>",
                           "<p style = 'text-align: center;'><small>&copy; - <a href='https://alessiobenedetti.com' target='_blank'>alessiobenedetti.com</a> - <script>document.write(yyyy);</script></small></p>")
                         ))
                     )
                     
    ), # end dashboardSidebar
    
    dashboardBody(
      
      tabItems(
        
        tabItem(tabName = "home",
                
                # home section
                includeMarkdown("www/home.md")
                
        ),
        
        tabItem(tabName = "map",
                
                # parks map section
                leafletOutput("parksMap") %>% withSpinner(color = "green")
                
        ),
        
        tabItem(
          # species data section
          tabName = "table", dataTableOutput("speciesDataTable") %>% withSpinner(color = "green")
          
        ),
        
        tabItem(tabName = "tree", 
                
                # collapsible species tree section
                includeMarkdown("www/tree.md"),
                column(3, uiOutput("parkSelectComboTree")),
                column(3, uiOutput("categorySelectComboTree")),
                collapsibleTreeOutput('tree', height='700px') %>% withSpinner(color = "green")
                
        ),
        
        tabItem(tabName = "charts",
                
                # ggplot2 species charts section
                includeMarkdown("www/charts.md"),
                fluidRow(column(3, uiOutput("categorySelectComboChart"))),
                column(6, plotOutput("ggplot2Group1") %>% withSpinner(color = "green")),
                column(6, plotOutput("ggplot2Group2") %>% withSpinner(color = "green"))
                
        ), 
        
        tabItem(tabName = "choropleth",
                
                # choropleth species map section
                includeMarkdown("www/choropleth.md"),
                fluidRow(
                  column(3, uiOutput("statesSelectCombo")),
                  column(3, uiOutput("categorySelectComboChoro"))
                ),
                fluidRow(
                  column(3,tableOutput('stateCategoryList') %>% withSpinner(color = "green")),
                  column(9,leafletOutput("choroplethCategoriesPerState") %>% withSpinner(color = "green"))
                )
                
        ),
        
        tabItem(tabName = "releases", includeMarkdown("www/releases.md")),
        
        tabItem(tabName = "sub_1",
                fluidRow(
                  h1("jaja")),
                
        tabItem(tabName = "sub_2",
                fluidRow(
                h1("jeje")))
        
      ))
      
    ) # end dashboardBody
    
  )# end dashboardPage
  
))