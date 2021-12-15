library("leaflet")
library("shiny")
library("shiny.semantic")
library("shinycustomloader")
library("shinyWidgets")

myGridTemplate <- grid_template(
  default = list(
    areas = rbind(
      c("title", "title"),
      c("user", "map"),
      c("user", "info")
    ),
    cols_width = c("300px", "1fr"),
    rows_height = c("100px", "auto", "200px")
  ),
  mobile = list(
    areas = rbind(
      "title",
      "user",
      "map",
      "info"
    ),
    rows_height = c("50px", "100px", "auto", "200px"),
    cols_width = c("100%")
  )
)

semanticPage(
  grid(
    myGridTemplate,
    title = h1("SPA monitor"),
    info =    div(class = "ui raised segment",
                  div(
                    a(class="ui blue ribbon label", "Comment:"),
                    p(),
                    textOutput("dist_output"),
                  )
    ),
#   info = uiOutput("sidebarStatistics"),
    map = withLoader(leafletOutput("touristic_map")),
    user = div(
        dropdown_input("selected_category",
                       choices = NULL,
                       default_text = "Select category"),
      #                 type = "selection fluid"),
        dropdown_input("selected_region",
                       choices = NULL,
                       default_text = "Select Province")
      #                 type = "selection fluid"),
    )
  )
)
