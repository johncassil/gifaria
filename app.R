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
library(giphyr)
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
            
            dataTableOutput(outputId = "read_chapter"),
            br(),
            uiOutput("gif_view")
            
        )
    )
)

find_ele <- function(x, name) {
  x_names <- names(x)
  return(x[which(x_names == name)][[1]])
}

# Define server logic required to draw a histogram ----
server <- function(input, output, session) {
    
data <- reactive({
    input$chapter_input %>% 
        as.character() %>% 
        grab_data() %>% 
        unlist() %>% 
        unlist() %>% 
        tibble::as_tibble() %>%
        mutate(verse = row_number())  
})
    
output$read_chapter <- renderDataTable({
    req(data())
      data() %>% 
            DT::datatable(rownames = F,selection=list(mode="single", target="row"),
                          options = list(dom="tipr"))
    })
    
prev_gifs <- reactive({
       req(search_text())
        out <- suppressWarnings(
            gif_search(search_text(),
                       img_format = c("fixed_height_small", "downsized",
                                      "downsized_medium", "original")
                      ))
        if (is.null(out)) return(NULL)
        return(out)
    })
        
search_text <- reactive({
    req(data)
    req(input$read_chapter_rows_selected)
    data()[input$read_chapter_rows_selected,] %>% 
        pull(value)
})

output$gif_view <- renderUI({
            req(prev_gifs())
            apply(prev_gifs(), 1, function(x) {
                actionLink(find_ele(x, "id"), title = find_ele(x, "slug"),
                           label = NULL, class = "gifpreview", icon = NULL,
                           tags$img(src = find_ele(x, "fixed_height_small")))
            })
        })
}

shinyApp(ui = ui, server = server)
