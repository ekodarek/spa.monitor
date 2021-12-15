library("tidyverse")
library("leaflet")
library("data.table")
library("hans")
library("spa.monitor")


spa_poland <- fread("data/Wykaz_uzdrowisk_short_11.08.2021.csv", sep=",",
                                 select = c("MIEJSCOWOSC","LECZENIE",
                                            "WOJEWODZTWO",
                                            "LON","LAT"))
spa_poland <- spa_poland %>% mutate(CATEGORY = "SPA Poland")

spa_ex_de <- fread("data/Wykaz_uzdrowisk_dawne_niemieckie_w_PL.csv", sep=",",
                                 select = c("MIEJSCOWOSC","LECZENIE",
                                            "WOJEWODZTWO",
                                            "LON","LAT"))
spa_ex_de <- spa_ex_de %>% mutate(CATEGORY = "Ex-SPA")

spa_all <- rbind(spa_poland, spa_ex_de)
spa_all %>% write_csv("data/all_spa.csv")

regions.and.categories <- spa_all %>%
   select("CATEGORY","WOJEWODZTWO") %>% unique()
regions.and.categories %>% write_csv("data/regions.and.categories.csv")

# regions.and.categories <- list_attract

function(input, output, session) {

   # update choices based on selections
   update_dropdown_input(session,
                     "selected_category",
                     choices = unique(regions.and.categories$CATEGORY))

   observeEvent(c(input$selected_category),
                {
                   regions.and.category <- regions.and.categories %>%
                      filter(CATEGORY == input$selected_category) %>%
                      pull(WOJEWODZTWO)

                   update_dropdown_input(session,
                                     "selected_region",
                                     choices = regions.and.category)
                })




   output$touristic_map <- renderLeaflet({

        spa_all %>%
        filter(WOJEWODZTWO == input$selected_region) %>%
        filter(CATEGORY == input$selected_category) %>%
#        find.min.dist(df_filtered_tmp, 51.1, 17.03) %>%
        leaflet() %>%
        addTiles() %>%
        addCircleMarkers(lng = ~LON,
                         lat = ~LAT) %>%
        addLegend(colors = "blue",
                  labels = paste("SPA in province \"",
                                 input$selected_region,
                                 "\" with status ", input$selected_category),
                  position = "bottomleft") %>%
        addMeasure(primaryLengthUnit = "meters",
                   primaryAreaUnit = "sqmeters",
                   thousandsSep = "")


   })

   output$dist_output <- renderText({

     df_filtered_tmp <- spa_all %>%
     filter(WOJEWODZTWO == input$selected_region) %>%
     filter(CATEGORY == input$selected_category)
     dist_calc_result <- find.min.dist(df_filtered_tmp, 51.1, 17.03)
     sprintf("Min distance: %.1f (meters) for coordinates: %.5f %.5f and %.5f %.5f",
              dist_calc_result[1,3],dist_calc_result[1,1],dist_calc_result[1,2],
              dist_calc_result[2,1],dist_calc_result[2,2])

   })
}
