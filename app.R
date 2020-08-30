#to do list
# make the UI pretty
# push to shinyapps.io

library(shiny)
library(DT)
library(dplyr)
library(giphyr)
library(httr)
library(jsonlite)
library(shinythemes)
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



ui <- navbarPage(theme = shinytheme("sandstone"),
    
    title = "ג",
    tabPanel("gipharia",
            # icon  = icon("tasks"),
             sidebarLayout(
               
               sidebarPanel(
                 
                 selectInput(inputId = "text_input",
                             label = "Select a text:",
                             choices = tanakh_titles$title),
                 uiOutput("chapter_input_dropdown"),
                 br(),
                 HTML('<center><h3>Click on a verse to get GIFs!</h3></center>'),
                 
                 dataTableOutput(outputId = "read_chapter")
               ),
               
               
               mainPanel(
                 
                 
                 
                 uiOutput("gif_view")
                 
               ))
    )
)

find_ele <- function(x, name) {
  x_names <- names(x)
  return(x[which(x_names == name)][[1]])
}
get_options <- function(input){
  res <- httr::GET(paste0('https://www.sefaria.org/api/shape/', input))
  #Unpack it:
  data <- jsonlite::fromJSON(rawToChar(res$content))
  options <- 1:data$length %>% as.integer()
}


#Function to get the texts from Sefaria's API:
grab_data <- function(text, chapter){
  #Get the data:
  res <- httr::GET(paste0('https://www.sefaria.org/api/texts/',
                          text,".", chapter, 
                          '?context=0&pad=0'))
  #Unpack it:
  data <- jsonlite::fromJSON(rawToChar(res$content)) %>% .$text
}

server <- function(input, output, session) {
    

data <- reactive(grab_data(as.character(input$text_input), 
                      as.character(input$chapter_input)) %>% 
            unlist() %>% 
            unlist() %>% 
              tibble::as_tibble_col(column_name = "Text") %>%
              mutate(verse = row_number(),
                     Text = stringr::str_replace_all(Text, '<i>', ''),
                     Text = stringr::str_replace_all(Text, '</i>', '')) %>% 
              select(Verse = verse, Text)
)
   
output$read_chapter <- renderDataTable({
      req(data())
  
      data() %>% 
        DT::datatable(rownames = F,selection=list(mode="single", target="row"),
                      options = list(dom="tipr"))
   })
    
    
    
    chapter_dropdown_values <- reactive({
        input$text_input %>% get_options()
    })
    
    
    output$chapter_input_dropdown <- renderUI({
        selectInput(inputId = "chapter_input",
                    label = "Select a chapter: ",
                    choices = chapter_dropdown_values()
        )
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
        pull(Text)
})

output$gif_view <- renderUI({
            req(prev_gifs())
            apply(prev_gifs(), 1, function(x) {
                actionLink(find_ele(x, "id"), title = find_ele(x, "slug"),
                           label = NULL, class = "gifpreview", icon = NULL,
                           tags$img(src = find_ele(x, "downsized_medium")))
            })
        })

}

shinyApp(ui = ui, server = server)
