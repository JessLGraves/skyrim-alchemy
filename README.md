# Skyrim Alchemy Assistant

This is a [Shiny web application](https://jessgraves.shinyapps.io/skyrim-ingredients/) designed to help users explore the effects of different ingredients in The Elder Scrolls V: Skyrim. The tool allows players to search for individual ingredients, find their effects, and combine multiple ingredients to see their shared effects for crafting potions or poisons.

## Features

* **Ingredient Search**: Search for an individual ingredient and see its effects in Skyrim.
* **Combined Ingredients**: Enter up to three ingredients to see the shared effects between them.
* **Effect Details**: Discover the shared effects across ingredients and learn what potions or poisons can be created from those effects.
* **Interactive Table**: A dynamic table displays the effects of selected ingredients, allowing players to explore the data interactively.
* **Potion/Poison Creation**: For any combination of ingredients, the app will show the resulting potion/poison effect along with its description.

# Getting Started

## Prerequisites
Before running the app, ensure that you have the following software installed:

* R
* RStudio
  
## Setting Up the Project Environment
This project uses renv to manage R package dependencies. Follow the steps below to set up your environment:

1. Clone or download this repository to your local machine.
2. Open the project in RStudio by double-clicking the .Rproj file (`skyrim-ingredients.Rproj`).
3. If you're using **renv** for the first time, you'll need to install the required R packages listed in the renv.lock file. To do this, run the following command in the RStudio console:

```{r}
renv::restore() 
```

This command will read the `renv.lock` file and install all the necessary packages for the project.

If renv is not yet installed on your system, you can install it by running:

```{r} 
install.packages("renv")
```

Then, proceed with the `renv::restore()` step.

## Running the Application

Once your environment is set up:
1. Open the project in RStudio.
2. Make sure you have restored the dependencies using renv::restore().
3. In RStudio, click the Run App button, or run the following command in the R console:

```{r}
shiny::runApp()
```

## Data Files
The app uses two CSV files located in the `data/` directory:
* **ingredients-effects.csv**: Contains the list of Skyrim ingredients and their effects.
* **effects-clean.csv**: Contains a description of each effect.
Make sure these files are in the correct directory, or adjust the paths in the code accordingly.

# Usage

1. **Select an Ingredient**: Use the dropdown menus on the sidebar to choose up to three ingredients.
2. **View Effects**: The app will display the effects of the selected ingredients in the main panel.
3. **See Shared Effects**: If you select more than one ingredient, the app will show the effects shared between them.
4. **Create Potions/Poison**: The app will provide a list of possible potions/poisons based on the combined effects of the selected ingredients, along with their descriptions.

## Example Workflow

1. Select the first ingredient from the dropdown menu (e.g., Wheat).
2. Select a second ingredient to combine (e.g., Blue Mountain Flower).
3. The app will display the effects of Wheat and Blue Mountain Flower, as well as any shared effects between them.
4. If additional ingredients are selected, their shared effects will also be displayed.

# License

This project is licensed under the MIT License - see the LICENSE file for details.
Acknowledgements

The data used in this app is sourced from Elder Scrolls Fandom, including ingredients found in official expansions and add-ons.
Special thanks to the Skyrim community for contributing detailed ingredient and effect data.
Contact

For any issues or feature requests, please feel free to open an issue on this repository or contact me at [jessica.lynn.graves@example.com].
