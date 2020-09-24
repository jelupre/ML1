distance_of_Euclid <- function(a, b){
  
  return(sqrt(sum((a - b)^2)))
  
}


sort <- function(arr, point){
  
  row <- dim(arr)[1]
  col <- dim(arr)[2]
  
  ## 2 столбца - номер и расстояние
  distances <- matrix(NA, row, 2)
  
  for (i in 1:row) {
    distances[i, ] <- c(i, distance_of_Euclid(point, arr[i, 1:col - 1])) 
  }
  
  ordered_arr <- arr[order(distances[, 2]), ]
  
  return(ordered_arr)
  
}


kNN <- function(k, ordered_arr, point){
  
  ind1 <- 0;
  ind2 <- 0;
  ind3 <- 0;
  
  for (i in 1:k) {
    if (ordered_arr[i, 3] == 'setosa') {
      ind1 <- ind1 + 1
    }
    if (ordered_arr[i, 3] == 'versicolor') {
      ind2 <- ind2 + 1
    }
    if (ordered_arr[i, 3] == 'virginica') {
      ind3 <- ind3 + 1
    }
  }
  
  if (ind1 >= ind2 && ind1 >= ind3) {
    return(1)
  }
  if (ind2 >= ind1 && ind2 >= ind3) {
    return(2)
  }
  if (ind3 >= ind1 && ind3 >= ind2) {
    return(3)
  }
  
}


LOO <- function(arr){
  
  row <- dim(arr)[1]
  
  Q <- matrix(0, (row - 1), 1)
  
  for (i in 1:row) {
    
    point <- arr[i, 1:2]
    new_arr <- arr
    new_arr <- new_arr[-i, ]
    ordered_arr <- sort(new_arr, point)
    
    tmp <- 0
    
    if (arr[i, 3] == "setosa") {
      tmp <- 1
    }
    if (arr[i, 3] == "versicolor") {
      tmp <- 2
    }
    if (arr[i, 3] == "virginica") {
      tmp <- 3
    }
    
    
    for (k in 1:(row - 1)) {
      
      class <- kNN(k, ordered_arr, point)
      
      if (class != tmp) {
        Q[k] <- Q[k] + 1
      }
      
    }
    
  }
  
  min_k <- 1
  min_v <- Q[min_k]
  
  for (i in 1:(row - 1)) {
    if (min_v > Q[i]) {
      min_v <- Q[i]
      min_k <- i
    }
  }
  
  
  return(min_k)
  
}


main <- function(){
  ## создание тренировочной выборки по ширине и длине лепестка и виду ириса
  train_set <- iris[, 3:5]
  
  ## кол-во точек в тестовой выборке
  n <- as.numeric(readline(prompt = "Enter number of points in test set: "))
  
  ## создание набора тестовых точек
  points <- cbind(runif(n, 1, 7), runif(n, 0, 2.5))
  
  ## рисуем выборку
  colors <- c("setosa" = "red", "versicolor" = "green4", "virginica" = "blue")
  plot(iris[, 3:4], pch = 21, bg = colors[iris$Species], asp = 1)
  
  k = LOO(train_set)
  print(k)
  
  ## рисуем точки
  for (i in 1:n) {
    ordered_set <- sort(train_set, points[i, 1:2])
    points(points[i, 1], points[i, 2], pch = 24, bg = colors[kNN(k, ordered_set, points[i, 1:2])], asp = 1)
  }
}

main()
