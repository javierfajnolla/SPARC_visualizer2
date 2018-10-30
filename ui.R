library(leaflet)
library(tidyverse)
library(plotly)

# Choices for drop-downs
vars <- c(
  "Is SuperZIP?" = "superzip",
  "Centile score" = "centile",
  "College education" = "college",
  "Median income" = "income",
  "Population" = "adultpop"
)


navbarPage("SPARC VISUALIZER", id="nav",
           
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
                                      
                                      checkboxInput("layA", label = "LAYER 1", value = T),
                                      
                                      column(6, numericInput("thresholds_A", "Number Colors",
                                                   min = 2, max = 3, value = 2)),
                                      
                                      column(6, selectInput("color_A", "Palette",
                                                            c("Purple to yellow" = "viridis",
                                                              "Yellow to black" = "magma",
                                                              "Greens" = "Greens",
                                                              "Blue to purple" = "BuPu",
                                                              "Oranges" = "Oranges",
                                                              "Blues" = "Blues",
                                                              "Greys" = "Greys",
                                                              "Reds" = "Reds",
                                                              "Purples" = "Purples"),
                                                            selected = "Oranges",
                                                            width = 800)),
                                      
                                      sliderInput("thr1", "Solution threshold:",
                                                  min = 1, max = 100, step = 1,
                                                  value = 17),
                                      
                                      hr(),
                                      
                                      sliderInput("thr2", "Carbon offset",
                                                  min = 0, max = 100, step = 1,
                                                  value = tbl %>% 
                                                    filter(abs(perc_pixels - 0.17) == min(abs(perc_pixels - 0.17))) %>% 
                                                    pull(prop_carbon_strg) %>% 
                                                    `*` (100) %>% 
                                                    round(0)),
                                      
                                      plotlyOutput("plotly"),
                                      
                                      # uiOutput("thresholdUI"),
                                      # 
                                      # hr(),
                                      # 
                                      # uiOutput("carbonUI"),
                                      # 
                                      # conditionalPanel("input.thresholds_A == 2",
                                      #                  sliderInput("thr1_A", "Priority level:",
                                      #                              min = 1, max = 999, step = 1, 
                                      #                              value = 200)),
                                      # conditionalPanel("input.thresholds_A == 3",
                                      #                  sliderInput("thr2_A", "Priority level:",
                                      #                              min = 1, max = 999, step = 1, 
                                      #                              value = c(200, 400))),
                                      
                                      # sliderInput("opacity_A", "Transparency:",
                                      #             min = 0, max = 1, step = 0.1, value = 0.5),
                                      
                                      # downloadButton("download_solution_A", label = "Download raster"),
                                      
                                      # hr(),
                                      
                                      # checkboxInput("layB", label = "LAYER 2", value = T),
                                      # 
                                      # column(6, numericInput("thresholds_B", "Number Colors",
                                      #                        min = 2, max = 3, value = 2)),
                                      # 
                                      # column(6, selectInput("color_B", "Palette",
                                      #                       c("Purple to yellow" = "viridis",
                                      #                         "Yellow to black" = "magma",
                                      #                         "Greens" = "Greens",
                                      #                         "Blue to purple" = "BuPu",
                                      #                         "Oranges" = "Oranges",
                                      #                         "Blues" = "Blues",
                                      #                         "Greys" = "Greys",
                                      #                         "Reds" = "Reds",
                                      #                         "Purples" = "Purples"),
                                      #                       selected = "Purples",
                                      #                       width = 800)),
                                      # 
                                      # conditionalPanel("input.thresholds_B == 2",
                                      #                  sliderInput("thr1_B", "Priority level:",
                                      #                              min = 1, max = 999, step = 1, 
                                      #                              value = 200)),
                                      # conditionalPanel("input.thresholds_B == 3",
                                      #                  sliderInput("thr2_B", "Priority level:",
                                      #                              min = 1, max = 999, step = 1, 
                                      #                              value = c(200, 400))),
                                      # 
                                      # sliderInput("opacity_B", "Transparency:",
                                      #             min = 0, max = 1, step = 0.1, value = 0.5),
                                      # 
                                      # downloadButton("download_solution_B", label = "Download raster"),
                                      
                                      hr()
                                      
                                      
                                      
                                      
                                      # selectInput("size", "Size", vars, selected = "adultpop"),
                                      # conditionalPanel("input.color == 'superzip' || input.size == 'superzip'",
                                      #                  # Only prompt for threshold when coloring or sizing by superzip
                                      #                  numericInput("threshold", "SuperZIP threshold (top n percentile)", 5)
                                      # )#,
                                      
                                      # plotOutput("histCentile", height = 200),
                                      # plotOutput("scatterCollegeIncome", height = 250)
                        ),
                        
                        tags$div(id="cite",
                                 'Priority conservation sites from ', tags$em('SPARC'))
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
