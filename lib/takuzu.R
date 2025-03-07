#1.Structure de la bibliothèque R pour le jeu Takuzu


# Fonction pour initialiser la grille Takuzu (8x8)
init_grid <- function(n = 8) {
  grid <- matrix(NA, nrow = n, ncol = n)  # Créer une matrice vide de dimension n x n
  return(grid)
}
# Fonction pour afficher la grille sous forme lisible
display_grid <- function(grid) {
  print(grid)
}

# Vérifier si une ligne ou une colonne a un nombre égal de 0 et 1
check_balance <- function(grid) {
  n <- nrow(grid)
  for (i in 1:n) {
    row <- grid[i, ]
    col <- grid[, i]
    
    if (sum(row == 0, na.rm = TRUE) != n / 2 || sum(col == 0, na.rm = TRUE) != n / 2) {
      return(FALSE)  # Si l'équilibre n'est pas respecté
    }
  }
  return(TRUE)
}

# Vérifier si une ligne ou une colonne contient trois chiffres identiques consécutifs
check_consecutive <- function(grid) {
  n <- nrow(grid)
  for (i in 1:n) {
    row <- grid[i, ]
    col <- grid[, i]
    
    if (any(rle(row)$lengths == 3) || any(rle(col)$lengths == 3)) {
      return(FALSE)  # Si une ligne ou colonne contient trois 0 ou trois 1 consécutifs
    }
  }
  return(TRUE)
}

# Vérifier si deux lignes ou colonnes sont identiques
check_identical <- function(grid) {
  n <- nrow(grid)
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      if (all(grid[i, ] == grid[j, ]) || all(grid[, i] == grid[, j])) {
        return(FALSE)  # Si des lignes ou colonnes sont identiques
      }
    }
  }
  return(TRUE)
}

# Fonction pour vérifier si la grille respecte toutes les règles
check_grid <- function(grid) {
  if (!check_balance(grid)) {
    return("Erreur : Les lignes ou colonnes ne sont pas équilibrées.")
  }
  if (!check_consecutive(grid)) {
    return("Erreur : Trois chiffres identiques consécutifs détectés.")
  }
  if (!check_identical(grid)) {
    return("Erreur : Des lignes ou colonnes identiques détectées.")
  }
  return("Grille valide !")
}
