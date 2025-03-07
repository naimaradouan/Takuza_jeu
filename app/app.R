# Chargement des librairies nécessaires
library(shiny)

# Charger la bibliothèque Takuzu
source("lib/takuzu.R")  # Assurez-vous que le chemin du fichier est correct

# Interface utilisateur (UI)
ui <- fluidPage(
  
  # Titre de l'application
  titlePanel("Jeu Takuzu"),
  
  # Description du jeu
  p("Bienvenue dans le jeu Takuzu. Remplissez la grille en respectant les règles."),
  
  # Affichage de la grille
  tableOutput("grid_output"),
  
  # Entrée pour remplir la grille (les utilisateurs peuvent interagir avec chaque case)
  actionButton("fill_zero", "Remplir avec 0"),
  actionButton("fill_one", "Remplir avec 1"),
  
  # Bouton pour vérifier la grille
  actionButton("check", "Vérifier la grille")
)

# Serveur
server <- function(input, output, session) {
  
  # Initialiser la grille à vide
  grid <- reactiveVal(init_grid())
  
  # Afficher la grille dans l'interface
  output$grid_output <- renderTable({
    grid()
  })
  
  # Action quand l'utilisateur clique sur "Remplir avec 0"
  observeEvent(input$fill_zero, {
    current_grid <- grid()
    current_grid[is.na(current_grid)] <- 0  # Remplir les cases vides avec 0
    grid(current_grid)
  })
  
  # Action quand l'utilisateur clique sur "Remplir avec 1"
  observeEvent(input$fill_one, {
    current_grid <- grid()
    current_grid[is.na(current_grid)] <- 1  # Remplir les cases vides avec 1
    grid(current_grid)
  })
  
  # Vérifier si la grille est correcte
  observeEvent(input$check, {
    current_grid <- grid()
    result <- check_grid(current_grid)  # Appeler la fonction de vérification
    showModal(modalDialog(
      title = "Résultat",
      result,
      easyClose = TRUE
    ))
  })
}

# Lancer l'application
shinyApp(ui, server)
