#
# This is a Shiny web application to help search for ingredients with matching effects in Skyrim.
#
# You can run the application by clicking
# the 'Run App' button above.
#
# Author: Jess Graves
#

library(shiny)
library(tidyr)
library(readr)
library(dplyr)
library(DT)
library(shinythemes)

# Load dataset from the 'data' directory
dataset <- read_csv("data/ingredients-effects.csv") # Adjust the filename and path as needed
effect_dataset <- read_csv("data/effects-clean.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
  tags$style(HTML("
    /* Remove the height restriction from the scroll body */
    .dataTables_scrollBody {
      height: auto !important;  /* Override the default height */
      max-height: 200px !important; /* Remove any max-height */
      overflow: auto; /* Enable scrolling if needed */
    }
  ")),
  theme = shinytheme("sandstone"),
  # Application title
  titlePanel("Skyrim Alchemy Assistant"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      width = 3,
      p("Select an ingredient to see its effects."),
      p("Combining 2-3 ingredients creates a potion/poison."),
      selectInput("ingredient1",
        "First ingredient",
        choices = c("", sort(unique(dataset$Ingredient))),
        selected = NULL, # No default selection
        multiple = FALSE, # Allow only single selection
        selectize = TRUE, # Make it searchable (optional)
      ),
      helpText("Enter an ingredient name to find it in the dataset"),
      selectInput("ingredient2",
        "+ Ingredient",
        choices = c("", sort(unique(dataset$Ingredient))),
        selected = NULL, # No default selection
        multiple = FALSE, # Allow only single selection
        selectize = TRUE, # Make it searchable (optional)
      ),
      helpText("Enter a second ingredient to see joint effects"),
      selectInput("ingredient3",
        "+ Ingredient",
        choices = c("", sort(unique(dataset$Ingredient))),
        selected = NULL, # No default selection
        multiple = FALSE, # Allow only single selection
        selectize = TRUE, # Make it searchable (optional)
      ),
      helpText("Enter a third ingredient to see joint effects"),
      tags$a(
        href = "https://github.com/JessLGraves/skyrim-alchemy/", # Replace with your GitHub URL
        target = "_blank", # Opens the link in a new tab
        tags$img(
          src = "https://upload.wikimedia.org/wikipedia/commons/thumb/9/91/Octicons-mark-github.svg/1024px-Octicons-mark-github.svg.png", # GitHub logo URL
          height = "30px", # Set a custom size
          width = "30px" # Set a custom size
        )
      ),
    ),

    # Show a plot of the generated distribution
    mainPanel(
      tags$blockquote(
        "How can I serve you, my Thane?",
        style = "margin-left: 6px; font-style: italic; color: #555; font-size: 16px"
      ),
      p("Use this tool to search for raw ingredients, their effects,
            and shared effects to make potions or poisons from The Elder Scrolls V: Skyrim.
            Selecting combinations of ingredients that share effects will result in a potion (or poison) with those shared effects."),
      p("Below is a database* of raw ingredients found in Skyrim and their effects.
          As you enter ingredients on the left, the table will dynamically filter the results."),
      p("After you have entered your first ingredient,
            you will see a second table that shows all other ingredients share effects
            with your first ingredient."),
      p(HTML("<p style='font-size: 10px; margin-right: 10px; text-align: right;'>*Data source: <a href='https://elderscrolls.fandom.com/wiki/Created_Potions_(Skyrim)' target='_blank'>elderscrolls.fandom.com</a>")),
      hr(),
      h3("Ingredient database"),
      DTOutput("result_table"),
      h3("Other ingredients that share effects with your first ingredient"),
      tableOutput("ingredients_with_shared_effects"),
      hr(),
      h3("Resulting Potion/Poison"),
      uiOutput("potion_name"),
      br(),
      uiOutput("potion_description"),
      br(),
      p(HTML("<p style='font-size: 10px; margin-right: 10px; text-align: right;'>*In-game descriptions of effects were taken from
            <a href='https://en.uesp.net/wiki/Skyrim:Alchemy_Effects#Effect_List',
            target='_blank'>UESP</a>.")),
      p(HTML("Your Alchemy skill level, Alchemy perks, and Fortify Alchemy
            enchantments all go into determining the strength and duration of your potions or poisons.
            Visit UESP's <a href='https://en.uesp.net/wiki/Skyrim:Alchemy_Effects', target='_blank'>Alchemy Effects page</a> for more information.")),
      hr(),
      h3("Summary of shared effects across all ingredients"),
      p("After entering your ingredients above, you will see a summary of the shared effects between all of
            the ingredients you've selected -- the shared effects that will contribute to your potion.
            For 2-ingredient potions/poisons, it is redundant with the above tables.
            But for 3-ingredient potions, it can be fun to see where the shared effects are."),
      tableOutput("effects_of_ingredients"),
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

    if (query1 != "" & query2 == "" & query3 == "") {
      dataset %>%
        filter(Ingredient == query1)
    } else if (query1 != "" & query1 != "" & query3 == "") {
      dataset %>%
        filter(Ingredient == query1 | Ingredient == query2)
    } else if (query1 != "" & query1 != "" & query3 != "") {
      dataset %>%
        filter(Ingredient == query1 | Ingredient == query2 | Ingredient == query3)
    } else {
      return(dataset)
    }
  })

  long_data <- reactive({
    query1 <- input$ingredient1
    dataset %>%
      pivot_longer(cols = contains("Effect"), names_to = "Level", values_to = "Effect")
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
    if (query1 != "" & query2 != "" & query3 == "") {
      shared_data <- long_data() %>%
        filter(Ingredient == query1 | Ingredient == query2) %>%
        group_by(Effect) %>%
        filter(n_distinct(Ingredient) > 1) %>%
        ungroup()

      return(shared_data)
    } else if (query1 != "" & query2 != "" & query3 != "") {
      shared_data <- dataset %>%
        pivot_longer(
          cols = contains("Effect"),
          names_to = "Level",
          values_to = "Effect"
        ) %>%
        filter(Ingredient == query1 | Ingredient == query2 | Ingredient == query3) %>%
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
        summarize(Effects = paste(Effect, collapse = ", "), .groups = "drop")

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
      descriptions <- paste(
        paste0("'", descriptions, collapse = " "),
        "'"
      )

      potion_results <- tibble(potion_name, descriptions)
    } else {
      potion_results <- tibble(
        potion_name = " Nothing, until you add more ingredients!",
        descriptions = "Nothing, until you add more ingredients!"
      )
    }
    return(potion_results)
  })

  output$result_table <- renderDT({
    datatable(filtered_data(),
      options = list(
        scrollY = 300,
        scrollX = TRUE,
        pageLength = 25
      )
    )
  })

  output$shared_effects <- renderTable({
    shared_effects_data()
  })

  output$potion_name <- renderUI({
    tags$span(
      tags$b("Potion/poison will include: "),
      paste(potion_output()$potion_name)
    )
  })

  output$potion_description <- renderUI({
    tags$span(
      tags$b("In-game description*: "),
      paste(potion_output()$descriptions)
    )
  })

  output$ingredients_with_shared_effects <- renderTable({
    ingredients_with_shared_effects_data()
  })

  output$effects_of_ingredients <- renderTable({
    potion_ingredients_effects()
  })
}

# Run the application
shinyApp(ui = ui, server = server)
