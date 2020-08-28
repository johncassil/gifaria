#to do list

# get the API book/chapter schema
# put into dropdowns
# customize DT appearance
# connect DT cell click to gif api
# make the UI pretty
# push to shinyapps.io



library(shiny)
library(shinymaterial)
library(DT)
library(dplyr)
library(httr)
library(jsonlite)
library(purrr)
library(tibble)
library(stringr)


#Function to get the texts from Sefaria's API:
grab_data <- function(text){
    
    #Get the data:
    res <- httr::GET(paste0('https://www.sefaria.org/api/texts/', text, '?context=0&pad=0'))
    
    #Unpack it:
    data <- jsonlite::fromJSON(rawToChar(res$content)) %>% .$text
}


ui <- fluidPage(
    
    # App title ----
    titlePanel("Hello Shiny!"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
            
            # Input: Slider for the number of bins ----
            textInput(inputId = "chapter_input",
                        label = "Chapter:",
                        value = "Proverbs.1")
            
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            
            # Output: Histogram ----
            dataTableOutput(outputId = "read_chapter")
            
        )
    )
)


# Define server logic required to draw a histogram ----
server <- function(input, output, session) {
    
    output$read_chapter <- renderDataTable({
        input$chapter_input %>% 
            as.character() %>% 
            grab_data() %>% 
            unlist() %>% 
            unlist() %>% 
            tibble::as_tibble() %>%
            mutate(verse = row_number()) %>% 
            DT::datatable()
    }
    )
    
}

shinyApp(ui = ui, server = server)


# 
# 
# 
# 
# 
# 
# 
# ui <- material_page(
#     title = "ג gipharia",
#     nav_bar_fixed = TRUE,
#     include_fonts = T,
#     nav_bar_color = "indigo darken-4",
#     material_side_nav(
#         image_source = "/icon.png"),
#     material_row(
#         material_column(
#             width = 7,
#             material_dropdown(input_id = "chapter_picker", 
#                               label = "Chapter", 
#                               choices = "Proverbs.1", 
#                               selected = "Proverbs.1")
#             )
#         ),
#     textOutput(outputId = 'read_chapter')
# )
# 
# server <- function(input, output, session) {
#   
#     
#     output$read_chapter <- renderText({ 
#         input$var %>% purrr::map(grab_data) %>% unlist()
#             
#             # #Clean it up a bit:
#             # chapter_data <- tibble::as_tibble(unlist(data)) %>%
#             #     mutate(verse = row_number())
#             
#         
#     })
#     
# }
# 
