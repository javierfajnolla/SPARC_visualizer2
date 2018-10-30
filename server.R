library(tidyverse)
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(plotly)

# Leaflet bindings are a bit slow; for now we'll just sample to compensate
set.seed(100)

# Reactive values object storage
rvs <- reactiveValues()

function(input, output, session) {
  
  ## Interactive Map ###########################################
  
  output$map <- renderLeaflet({
    # leaflet(TAC_border) %>% 
    leaflet() %>% 
      addTiles() %>%
      fitBounds(lng1 = extent(solution)[1],    # zoom to raster extent
                lat1 = extent(solution)[3],
                lng2 = extent(solution)[2],
                lat2 = extent(solution)[4]) #%>%
  })
  
  # Create map proxy to make further changes to existing map
  map <- leafletProxy("map")
  
  # thrA_value <- 17
  
  observeEvent(input$thr1, {
    rvs$sel_row <- tbl %>% 
      filter(abs(perc_pixels - (input$thr1) / 100) == min(abs(perc_pixels - (input$thr1) / 100)))
    ## Update Carbon offset slider
    updateSliderInput(session = session, inputId = "thr2", 
                      value = rvs$sel_row %>% pull(prop_carbon_strg) %>% `*` (100) %>% round(0))
  })
  observeEvent(input$thr2, {
    rvs$sel_row <- tbl %>% 
      filter(abs(prop_carbon_strg - (input$thr2) / 100) == min(abs(prop_carbon_strg - (input$thr2) / 100)))
    ## Update threshold slider
    updateSliderInput(session = session, inputId = "thr1", 
                      value = rvs$sel_row %>% pull(perc_pixels) %>% `*` (100))
  })
  
  observeEvent(rvs$sel_row, {
    # Threshold and reclassify
    sel_thr <- rvs$sel_row %>% pull(cutoff_sol)
    
    sol_rcl <- solution %>% 
      reclassify(tibble(from = c(0, sel_thr),
                        to = c(sel_thr, max(values(solution), na.rm = T)),
                        becomes = c(NA, 1)))
    
    # Leaflet
    pal_A <- colorFactor(
      palette = input$color_A,
      levels = c(0.5, 1),
      na.color = "transparent",
      reverse = FALSE)
    
    map %>% 
      clearControls() %>%    # Refreshes the legend
      clearGroup("solutions_A") %>%
      addRasterImage(sol_rcl,
                     colors = pal_A,
                     # opacity = input$opacity_A,
                     group = "solutions_A") %>%
      addLegend(pal = pal_A,
                values = c(NA, 1),
                group = "solutions_A",
                labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)),
                position = "topright")
  })
  
  ### Plotly graph
  observeEvent(input$thr1, {
    plot1 <- tbl %>% 
      mutate(perc_pixels = 100 * perc_pixels,
             prop_carbon_strg = 100 * prop_carbon_strg) %>% 
      ggplot +
      geom_area(aes(x = perc_pixels, y = prop_carbon_strg)) +
      geom_area(data = tbl %>% 
                  mutate(perc_pixels = 100 * perc_pixels,
                         prop_carbon_strg = 100 * prop_carbon_strg) %>% 
                  filter(perc_pixels <= input$thr1),
                aes(x = perc_pixels, y = prop_carbon_strg),
                fill = "#E45621") +
      xlab("Proportion of carbon storage saved") +
      ylab("Proportion of study area protected") +
      theme_minimal()
    
    output$plotly <- renderPlotly(
      plotly::ggplotly(plot1)  
    )
  })
  
  
  
  # ## 1) Update map and carbon offset slider when the threshold slider is used
  # observeEvent(input$thr1, {
  #   glue::glue("msg1") %>% print
  #   
  #   sel_row <- tbl %>% 
  #     filter(abs(perc_pixels - (input$thr1) / 100) == min(abs(perc_pixels - (input$thr1) / 100)))
  #   
  #         thr1 <- 50
  #         sel_row <- tbl %>%
  #           filter(abs(perc_pixels - (thr1) / 100) == min(abs(perc_pixels - (thr1) / 100)))
  #         
  #   print(sel_row)
  #   
  #   sel_thr <- sel_row %>% pull(cutoff_sol)
  #   
  #   sol_rcl <- solution %>% 
  #     reclassify(tibble(from = c(0, sel_thr),
  #                       to = c(sel_thr, max(values(solution), na.rm = T)),
  #                       becomes = c(NA, 1)))
  #   
  #   sol_rcl %>% plot
  #   
  #   ## Update Carbon offset slider
  #   updateSliderInput(session = session, inputId = "thr2", 
  #                     value = sel_row %>% pull(prop_carbon_strg) %>% `*` (100) %>% round(0))
  #   
  #   # glue::glue("msg1_c, {perc_carb_off %>% round(0)}") %>% print
  #   
  #   ## Map
  #   pal_A <- colorFactor(#palette = c("#c0002d", "#f8f5f5", "#0069a8"),
  #     palette = input$color_A,
  #     # domain = range_values,
  #     levels = c(0.5, 1),
  #     na.color = "transparent",
  #     reverse = FALSE)
  #   
  #   map %>% 
  #     clearControls() %>%    # Refreshes the legend
  #     clearGroup("solutions_A") %>%
  #     addRasterImage(sol_rcl,
  #                    colors = pal_A,
  #                    # opacity = input$opacity_A,
  #                    group = "solutions_A") %>%
  #     addLegend(pal = pal_A,
  #               values = c(NA, 1),#c(input$thr1_A, 1000),
  #               group = "solutions_A",
  #               labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)),
  #               position = "topright")
  # })
  # 
  # 
  # ## 2) Update map and Threshold slider when the carbon offset slider is used
  # observeEvent(input$thr2, {
  #   glue::glue("msg2") %>% print
  #   
  #   # Find the proportion of pixels that I need to select to get that % of carbon offset
  #   sel_row <- tbl %>% 
  #     filter(abs(prop_carbon_strg - (input$thr2) / 100) == min(abs(prop_carbon_strg - (input$thr2) / 100)))
  #   # pull(cutoff_sol)
  #   
  #         thr2 <- 52
  #         sel_row <- tbl %>% 
  #           filter(abs(prop_carbon_strg - (thr2) / 100) == min(abs(prop_carbon_strg - (thr2) / 100))) #%>% 
  #   
  #   sel_row %>% print
  #   
  #   sel_thr <- sel_row %>% pull(cutoff_sol)
  #   
  #   sol_rcl <- solution %>% 
  #     reclassify(tibble(from = c(0, sel_thr),
  #                       to = c(sel_thr, max(values(solution), na.rm = T)),
  #                       becomes = c(NA, 1)))
  #   
  #   sol_rcl %>% plot
  #   
  #   glue::glue("msg2_d") %>% print
  #   ## Update Threshold slider
  #   updateSliderInput(session = session, inputId = "thr1", 
  #                     # value = tbl %>% 
  #                     #   filter(abs(prop_carbon_strg - (input$thr2) / 100) == min(abs(prop_carbon_strg - (input$thr2) / 100))) %>% 
  #                     #   pull(perc_pixels))
  #                     value = sel_row %>% pull(perc_pixels) %>% `*` (100))
  #   
  #   
  #   ## Map
  #   pal_A <- colorFactor(#palette = c("#c0002d", "#f8f5f5", "#0069a8"),
  #     palette = input$color_A,
  #     # domain = range_values,
  #     levels = c(0.5, 1),
  #     na.color = "transparent",
  #     reverse = FALSE)
  #   
  #   map %>% 
  #     clearControls() %>%    # Refreshes the legend
  #     clearGroup("solutions_A") %>%
  #     addRasterImage(sol_rcl,
  #                    colors = pal_A,
  #                    # opacity = input$opacity_A,
  #                    group = "solutions_A") %>%
  #     addLegend(pal = pal_A,
  #               values = c(NA, 1),#c(input$thr1_A, 1000),
  #               group = "solutions_A",
  #               labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)),
  #               position = "topright")
  #   
  # })#, suspended = T)
}