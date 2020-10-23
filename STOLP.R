distance_of_Euclid <- function(a, b){
  
  return(sqrt(sum((a - b)^2)))
  
}


K <- function(x, y, h) {
  
  r <- distance_of_Euclid(x, y)/h
  E <- 3 * (1 - r^2) / 4
  Q <- 15 * (1 - r^2) / 16
  G <- ((2 * pi) ^ (-1 / 2)) * exp(-(r ^ 2) / 2)
  P <- 1/2
  Tr <- 1 - abs(r)
  
  return(G)
}


M <- function(point, set) {
  
  h <- 0.1
  weights <- matrix(0, 1, 3)
  row <- dim(set)[1]
  class <- c("setosa", "versicolor", "virginica")
  
  for (i in 1:row) {
    tmp <- K(set[i, 1:2], point, h)
    if (set[i, 3] == "setosa")      weights[1] <- weights[1] + tmp  
    if (set[i, 3] == "versicolor")  weights[2] <- weights[2] + tmp
    if (set[i, 3] == "virginica")   weights[3] <- weights[3] + tmp
  }
  
  
  if (class[which.max(weights)] == "setosa") {
    
    return(weights[1] - max(weights[2], weights[3]))
      
  }
  
  if (class[which.max(weights)] == "versicolor") {
    
    return(weights[2] - max(weights[1], weights[3]))
    
  }
  
  if (class[which.max(weights)] == "virginica") {
    
    return(weights[3] - max(weights[2], weights[1]))
    
  }
  
}


STOLP <- function(arr) {
  
  row <- dim(arr)[1]
  margins <- matrix(NA, row, 1)
  
  for (i in 1:row) {
    
    margins[i] <- M(arr[i, 1:2], arr[-i, ])
    
    if (margins[i] < 0) {
      
      arr <- arr[-i, ]
      
    }
    
  }
  
  print(margins)
  
  plot(1:150, margins, type = "l", xlab = "Цветок", ylab = "Отступ")
  
  row <- dim(arr)[1]
  
  index_of_et_set <- 1
  index_of_et_ver <- 1
  index_of_et_vir <- 1
  et_set <- arr[index_of_et_set]
  et_ver <- arr[index_of_et_ver]
  et_vir <- arr[index_of_et_vir]
  
  for (i in 2:row) {
    
    margin <- M(arr[i, 1:2], arr[-i, ])
    
    if (arr[i, 3] == "setosa" && et_set < margin) {
      
      et_set <- margin
      index_of_et_set <- i
      
    }
    
    if (arr[i, 3] == "versicolor" && et_ver < margin) {
      
      et_ver <- margin
      index_of_et_ver <- i
      
    }

    if (arr[i, 3] == "virginica" && et_vir < margin) {
      
      et_vir <- margin
      index_of_et_vir <- i
      
    }
    
  }
  
  Omega <- c(arr[index_of_et_set, 1], arr[index_of_et_set, 2], arr[index_of_et_set, 3])
  Omega <- rbind(Omega, c(arr[index_of_et_ver, 1], arr[index_of_et_ver, 2], arr[index_of_et_ver, 3]))
  Omega <- rbind(Omega, c(arr[index_of_et_vir, 1], arr[index_of_et_vir, 2], arr[index_of_et_vir, 3]))
  
  print(Omega)
  
  return()
  
}




train_set <- iris[ , 3:5]
STOLP(train_set)

