#to do list
# customize DT appearance
# connect DT cell click to gif api
# make the UI pretty
# push to shinyapps.io

library(shiny)
library(DT)
library(dplyr)
library(httr)
library(jsonlite)
library(purrr)
library(tibble)
library(stringr)


## On startup:

#Get the books for the dropdown:

#Make API Call:
res <- httr::GET('https://www.sefaria.org/api/index/')

#Unpack it:
data <- jsonlite::fromJSON(rawToChar(res$content)) %>% 
    filter(category == 'Tanakh')
tanakh_only <- data$contents[[1]] %>% 
    filter(category %in% c('Torah', 'Prophets', 'Writings')) %>% 
    select(contents) 
extract_titles <- function(data){data %>% select(title)}
tanakh_titles <- tanakh_only$contents %>% purrr::map_dfr(extract_titles)




##########################################



ui <- fluidPage(
    
    # App title ----
    titlePanel("ג gipharia"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
            # Input: Slider for the number of bins ----
            selectInput(inputId = "text_input",
                        label = "Select a text:",
                        choices = tanakh_titles$title),
            
            uiOutput("chapter_input_dropdown")
            
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
            grab_data(as.character(input$text_input), 
                      as.character(input$chapter_input)) %>% 
            unlist() %>% 
            unlist() %>% 
            tibble::as_tibble() %>%
            mutate(verse = row_number()) %>% 
            DT::datatable()
    }
    )
    
    get_options <- function(input){
        res <- httr::GET(paste0('https://www.sefaria.org/api/shape/', input))
        #Unpack it:
        data <- jsonlite::fromJSON(rawToChar(res$content))
        options <- 1:data$length %>% as.integer()
    }
    
    
    chapter_dropdown_values <- reactive({
        input$text_input %>% get_options()
    })
    
    
    output$chapter_input_dropdown <- renderUI({
        selectInput(inputId = "chapter_input",
                    label = "Select a chapter: ",
                    choices = chapter_dropdown_values()
        )
        })

    #Function to get the texts from Sefaria's API:
    grab_data <- function(text, chapter){
        #Get the data:
        res <- httr::GET(paste0('https://www.sefaria.org/api/texts/',
                                text,".", chapter, 
                                '?context=0&pad=0'))
        #Unpack it:
        data <- jsonlite::fromJSON(rawToChar(res$content)) %>% .$text
    }
    
    
}

shinyApp(ui = ui, server = server)
