#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tidyr)
library(readr)
library(dplyr)
library(DT)
library(shinythemes)

# Load dataset from the 'data' directory
dataset <- read_csv("data/ingredients-effects.csv")  # Adjust the filename and path as needed
effect_dataset <- read_csv("data/effects-clean.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
     theme = shinytheme('sandstone'),
    # Application title
    titlePanel("Skyrim Ingredient Effect Finder"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          width=3,
          selectInput("ingredient1",
                      "Ingredient", 
                      choices = c('', sort(unique(dataset$Ingredient))), 
                      selected = NULL,  # No default selection
                      multiple = FALSE,  # Allow only single selection
                      selectize = TRUE,  # Make it searchable (optional)
                      ),            
            helpText("Enter an ingredient name to find it in the dataset"), 
          selectInput("ingredient2",
                      "+ Ingredient", 
                      choices = c('', sort(unique(dataset$Ingredient))), 
                      selected = NULL,  # No default selection
                      multiple = FALSE,  # Allow only single selection
                      selectize = TRUE,  # Make it searchable (optional)
          ),
            helpText("Enter a second ingredient to see joint effects"), 
          selectInput("ingredient3",
                      "+ Ingredient", 
                      choices = c('', sort(unique(dataset$Ingredient))), 
                      selected = NULL,  # No default selection
                      multiple = FALSE,  # Allow only single selection
                      selectize = TRUE,  # Make it searchable (optional)
          ),
            helpText("Enter a third ingredient to see joint effects")
        ),

        # Show a plot of the generated distribution
        mainPanel(
          h3("Effects of your ingredient"),
          p('Use this tool to search for ingredients, their effects, 
            and shared effects to make potions and posions from Elder Scrolls V: Skyrim.'),
          p(HTML("The table below shows ingredients and effects found in Skyrim. Data taken from <a href='https://elderscrolls.fandom.com/wiki/Created_Potions_(Skyrim)' target='_blank'>elderscrolls.fandom.com</a> -- including ingredients found in add ons/expansions.")),
          br(), 
          h3('Ingredient database'),
          DTOutput("result_table"),
          h3('Other ingredients that share effects with your first ingredient'),
          tableOutput('ingredients_with_shared_effects'),
          br(),
          h3("Combining ingredients & their shared effects"),
          br(),
          p('What shared effects do each ingredient you selected bring to the table?'),
          tableOutput('effects_of_ingredients'),
          h5('All shared effects:'), 
          textOutput('potion_name'),
          br(),
          h4('To create a potion/poison with the following effect:'),
          textOutput('potion_decription'), 
          br(),
          br()
        )
    )
)

# Define server logic
server <- function(input, output) {
  
  # Observe the search query and filter the dataset
  filtered_data <- reactive({
    query1 <- input$ingredient1
    query2 <- input$ingredient2
    query3 <- input$ingredient3
    
    if (query1 != '' & query2 == '' & query3 == ''){
      dataset %>%
        filter(Ingredient == query1)
    } else if (query1 != '' & query1 != '' & query3 ==''){
      dataset %>%
        filter(Ingredient == query1 | Ingredient == query2)
    } else if (query1 != '' & query1 != '' & query3 !=''){
      dataset %>%
        filter(Ingredient == query1 | Ingredient == query2 | Ingredient == query3)
    } else {
      return(dataset)
    }
  })
  
  long_data <- reactive({
    query1 <- input$ingredient1
    dataset %>% 
      pivot_longer(cols=contains('Effect'), names_to='Level', values_to='Effect') 
  })
  
  ingredients_with_shared_effects_data <- reactive({
    query1 <- input$ingredient1
    all_data <- long_data()
    ingredient1_data <- long_data() %>%
      filter(grepl(query1, Ingredient, ignore.case = TRUE))
    
    effects <- unique(ingredient1_data$Effect)
    
    ingredients_by_effects <- all_data %>%
      filter(!grepl(query1, Ingredient, ignore.case = TRUE)) %>%
      dplyr::select(-Level) %>%
      filter(Effect %in% effects) %>%   
      group_by(Effect) %>%
      arrange(Effect, Ingredient) %>%  
      summarise(Ingredients = paste(Ingredient, collapse = ", ")) %>%
      ungroup()
    
      return(ingredients_by_effects)
  })
  
  shared_effects_data <- reactive({
    query1 <- input$ingredient1
    query2 <- input$ingredient2
    query3 <- input$ingredient3
    
    # Make sure both ingredients are non-empty
    if (query1 != "" & query2 != "" & query3 == '') {
      shared_data <- long_data() %>%
        filter(Ingredient == query1 |  Ingredient == query2) %>%
        group_by(Effect) %>%
        filter(n_distinct(Ingredient) > 1) %>%
        ungroup()  
      
      return(shared_data)
      
    } else if (query1 != "" & query2 != "" & query3 !='') {
      shared_data <- dataset %>% 
        pivot_longer(cols = contains('Effect'), 
                     names_to = 'Level', 
                     values_to = 'Effect') %>%
        filter(Ingredient == query1 |  Ingredient == query2 | Ingredient == query3) %>%
        group_by(Effect) %>%
        filter(n_distinct(Ingredient) > 1) %>%
        ungroup()  
      
      return(shared_data) 
      
    } else {
      return(NULL)   
    }
  })
  
  potion_ingredients_effects <- reactive({
    shared_data <- shared_effects_data() 
    if (!is.null(shared_data) && nrow(shared_data) > 0) {
      potion_ingredients_only <- shared_data %>%
        ungroup() %>%
        group_by(Ingredient) %>%
        summarize(Effects = paste(Effect, collapse =', '), .groups='drop')
      
      return(potion_ingredients_only)
    } else {
      return(NULL)
    }
  })
  
  potion_output <- reactive({
    shared_data <- shared_effects_data()
    
    if (!is.null(shared_data) && nrow(shared_data) > 0) {
      effects <- unique(shared_data$Effect)
      potion_name <- paste(effects, collapse = " + ")
      
      descriptions <- effect_dataset$Description[effect_dataset$Effect %in% effects]
      descriptions <- paste(descriptions, collapse=' ')
      
      potion_results <- tibble(potion_name, descriptions)
      return(potion_results)
    } else {
      return(NULL)
    }
  })
  
  output$result_table <- renderDT({
    datatable(filtered_data(), 
              options = list(scrollY = 300,
                             scrollX = TRUE,
                             pageLength = 25))
  })
  
  output$shared_effects <- renderTable({
    shared_effects_data()
  })
  
  output$potion_name <- renderText({
    potion_output()$potion_name})
  
  output$potion_decription <- renderText({
    potion_output()$descriptions
  })
  
  output$ingredients_with_shared_effects <- renderTable({
    ingredients_with_shared_effects_data()})
  
  output$effects_of_ingredients <- renderTable({
    potion_ingredients_effects()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
