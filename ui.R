library(leaflet)
library(tidyverse)

# Choices for drop-downs
vars <- c(
  "Is SuperZIP?" = "superzip",
  "Centile score" = "centile",
  "College education" = "college",
  "Median income" = "income",
  "Population" = "adultpop"
)


navbarPage("Fajardo et al., (2019) VISUALIZER", id="nav",
           
           tabPanel("Interactive map",
                    div(class="outer",
                        
                        tags$head(
                          # Include our custom CSS
                          includeCSS("styles.css"),
                          includeScript("gomap.js")
                        ),
                        
                        # If not using custom CSS, set height of leafletOutput to a number instead of percent
                        leafletOutput("map", width = "100%", height = "100%"),
                        
                        # Shiny versions prior to 0.11 should use class = "modal" instead.
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = TRUE, top = 60, left = 60, right = "auto", bottom = "auto",
                                      width = 330, height = "auto",
                                      
                                      h2("Priority areas explorer"),
                                      
                                      # selectInput("solution", "Layer to show", 
                                      #             c("Represent sp in present" = "pre",
                                      #               "Represent sp in present and RCP 4.5" = "pre45",
                                      #               "Represent sp in present and RCP 8.5" = "pre85")),
                                      # 
                                      # hr(),
                                      
                                      numericInput("thresholds", "Number of levels",
                                                   min = 1, max = 2, value = 1),
                                      
                                      conditionalPanel("input.thresholds == 1",
                                                       sliderInput("thr1", "Priority level:",
                                                                   min = 1, max = 999, step = 1, 
                                                                   value = 200)),
                                      conditionalPanel("input.thresholds == 2",
                                                       sliderInput("thr2", "Priority level:",
                                                                   min = 1, max = 999, step = 1, 
                                                                   value = c(200, 400))),
                                      
                                      hr(),
                                      
                                      sliderInput("opacity", "Transparency:",
                                                  min = 0, max = 1, step = 0.1, value = 1),
                                      hr(),
                                      
                                      selectInput("color", "Choose a color palette",
                                                  c("Purple to yellow" = "viridis",
                                                    "Yellow to black" = "magma",
                                                    "Greens" = "Greens",
                                                    "Blue to purple" = "BuPu")),
                                      
                                      hr(),
                                      
                                      h5("Download the raster layer of the solution in the visualization:"),
                                      
                                      downloadButton("download_solution", label = "Download")
                                      # selectInput("size", "Size", vars, selected = "adultpop"),
                                      # conditionalPanel("input.color == 'superzip' || input.size == 'superzip'",
                                      #                  # Only prompt for threshold when coloring or sizing by superzip
                                      #                  numericInput("threshold", "SuperZIP threshold (top n percentile)", 5)
                                      # )#,
                                      
                                      # plotOutput("histCentile", height = 200),
                                      # plotOutput("scatterCollegeIncome", height = 250)
                        ),
                        
                        tags$div(id="cite",
                                 'Priority conservation sites from ', tags$em('TITLE OF PAPER'), ' by AUTHORS.'
                        )
                    )
           ),
           
           # tabPanel("Data explorer",
           #          fluidRow(
           #            column(3,
           #                   selectInput("states", "States", c("All states"="", structure(state.abb, names=state.name), "Washington, DC"="DC"), multiple=TRUE)
           #            ),
           #            column(3,
           #                   conditionalPanel("input.states",
           #                                    selectInput("cities", "Cities", c("All cities"=""), multiple=TRUE)
           #                   )
           #            ),
           #            column(3,
           #                   conditionalPanel("input.states",
           #                                    selectInput("zipcodes", "Zipcodes", c("All zipcodes"=""), multiple=TRUE)
           #                   )
           #            )
           #          ),
           #          fluidRow(
           #            column(1,
           #                   numericInput("minScore", "Min score", min=0, max=100, value=0)
           #            ),
           #            column(1,
           #                   numericInput("maxScore", "Max score", min=0, max=100, value=100)
           #            )
           #          ),
           #          hr(),
           #          DT::dataTableOutput("ziptable")
           # ),
           tabPanel("About",
                    fluidRow(
                      column(3, 
                             h4("A description of the research and a link to the paper will appear here")
                             )
                      )
                    ),
                    
           
           conditionalPanel("false", icon("crosshair"))
)