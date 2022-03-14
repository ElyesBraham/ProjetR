
# bloc 1 : isPossible()
# Détermine si on peut assigner une valeur à la place de NA
# 3 sous blocs:

# sous-bloc 1 : isInRow()
isInRow <- function(grille2, row, num){
  if(any(grille2[row, ] ==num, na.rm = TRUE)){
    return(TRUE)
  }
  return(FALSE)
}


# sous-bloc 2: isInCol()
isInCol <- function(grille2, col, num){
  if(any(grille2[, col] == num, na.rm = TRUE)){
    return(TRUE)
  }
  return(FALSE)
}


# sous-bloc 3 : isInBox()
isInBox <- function(grille2, row, col, num){
  box_row <- ((row -1) %/% 3)*3
  box_col <- (((col - 1) %/% 3)*3) + 1:3
  if(any(grille2[(box_row + 1), box_col] == num, na.rm = TRUE) ||
     any(grille2[(box_row + 2), box_col] == num, na.rm = TRUE) ||
     any(grille2[(box_row + 3), box_col] == num, na.rm = TRUE)){
    return(TRUE)
  }
  return(FALSE)

}


# Mettre tout ensemble : isPossible()

isPossible <- function(grille2, row, col, num){
  if(isFALSE(isInRow(grille2, row, num)) &&
     isFALSE(isInCol(grille2, col, num)) &&
     isFALSE(isInBox(grille2, row, col, num))){

    return(TRUE)
  }
}

# Bloc 2 : possibleNumbers()
# Récupérer les nombres possibles à mettre

possibleNumbers <- function(grille2, row, col){
  n <- 1:9
  p <- NA
  for(num in n){

    if(isTRUE(isPossible(grille2, row, col, num)))
      p <- append(p, num)
  }
  p <- p[complete.cases(p)]

  return(p)

}

# sudoku_solver()
# fonction complète pour résoudre le sudoku

sudoku_solver <- function(grille2){

  if(all(!is.na(grille2))){

    return(grille2)

  }

  df <- which(is.na(grille2), arr.ind = TRUE) # retrouver les valeurs manquantes

  row <- df[1, 1]
  col <- df[1, 2]

  p <- possibleNumbers(grille2, row, col)

  for(i in p){

    grille2[row, col] <- i
    soluce <- sudoku_solver(grille2) # fonction récursive --> vérifier l'emplacement des zéros

    if(!is.null(soluce)){# si le nombre isNULL est placé dans la mauvaise case --> essaye un nouveau nombre
      return(soluce)
      }
  }

  return(NULL)
}

sudoku_solver
res <- sudoku_solver(sudoku)
res


