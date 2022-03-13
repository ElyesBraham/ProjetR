sudoku <- matrix( c(NA,NA,NA,NA,NA,6,NA,NA,NA,
                    NA,9,5,7,NA,NA,3,NA,NA,
                    4,NA,NA,NA,9,2,NA,NA,5,
                    7,6,4,NA,NA,NA,NA,NA,3,
                    NA,NA,NA,NA,NA,NA,NA,NA,NA,
                    2,NA,NA,NA,NA,NA,9,7,1,
                    5,NA,NA,2,1,NA,NA,NA,9,
                    NA,NA,7,NA,NA,5,4,8,NA,
                    NA,NA,NA,8,NA,NA,NA,NA,NA),
                  byrow = T, ncol = 9)



# bloc 1 : isPossible()
# Détermine si on peut assigner une valeur à la place de NA
# 3 sous blocs:

# sous-bloc 1 : isInRow()
isInRow <- function(x, row, num){
  if(any(x[row, ] ==num, na.rm = TRUE)){
    return(TRUE)
  }
  return(FALSE)
}


# sous-bloc 2: isInCol()
isInCol <- function(x, col, num){
  if(any(x[, col] == num, na.rm = TRUE)){
    return(TRUE)
  }
  return(FALSE)
}


# sous-bloc 3 : isInBox()
isInBox <- function(x, row, col, num){
  box_row <- ((row -1) %/% 3)*3
  box_col <- (((col - 1) %/% 3)*3) + 1:3
  if(any(x[(box_row + 1), box_col] == num, na.rm = TRUE) ||
     any(x[(box_row + 2), box_col] == num, na.rm = TRUE) ||
     any(x[(box_row + 3), box_col] == num, na.rm = TRUE)){
    return(TRUE)
  }
  return(FALSE)

}


# Mettre tout ensemble : isPossible()

isPossible <- function(x, row, col, num){
  if(isFALSE(isInRow(x, row, num)) &&
     isFALSE(isInCol(x, col, num)) &&
     isFALSE(isInBox(x, row, col, num))){

    return(TRUE)
  }
}

# Bloc 2 : possibleNumbers()
# Récupérer les nombres possibles à mettre

possibleNumbers <- function(x, row, col){ #x = sudoku qu'on veut résoudre
  n <- 1:9
  p <- NA
  for(num in n){

    if(isTRUE(isPossible(x, row, col, num)))
      p <- append(p, num)
  }
  p <- p[complete.cases(p)]

  return(p)

}

# sudoku_solver()
# fonction complète pour résoudre le sudoku

sudoku_solver <- function(x){

  if(all(!is.na(x))){

    return(x)

  }

  df <- which(is.na(x), arr.ind = TRUE) # retrouver les valeurs manquantes

  row <- df[1, 1]
  col <- df[1, 2]

  p <- possibleNumbers(x, row, col)

  for(i in p){

    x[row, col] <- i
    sol <- sudoku_solver(x) # fonction récursive --> vérifier l'emplacement des zéros

    if(!is.null(sol)){# si le nombre isNULL est placé dans la mauvaise case --> essaye un nouveau nombre
      return(sol)
      }
  }

  return(NULL)
}

sudoku_solver
res <- sudoku_solver(sudoku)
res
