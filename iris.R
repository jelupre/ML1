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


kNN <- function(k, ordered_arr){
  
  col_class <- dim(ordered_arr)[2]
  
  class <- names(which.max(table(ordered_arr[1:k, col_class])))
  
  return(class)
  
}

kwNN <- function(k, ordered_arr, weights){
  
  order_and_weight <- cbind(ordered_arr, weights)
  classes <- order_and_weight[1:k, 3:4]
  
  w1 <- sum(classes[classes$Species == "setosa", 2])
  w2 <- sum(classes[classes$Species == "versicolor", 2])
  w3 <- sum(classes[classes$Species == "virginica", 2])
  
  answer <- matrix(c(w1, w2, w3), nrow = 1, ncol = 3, byrow = TRUE, list(c(1), c(1, 2, 3)))
  
  class <- c("setosa", "versicolor", "virginica")
  
  return(class[which.max(answer)])
  
}


LOO <- function(arr){
  
  row <- dim(arr)[1]
  
  Q <- matrix(0, (row - 1), 99)
  
  for (i in 1:row) {
    
    point <- arr[i, 1:2]
    new_arr <- arr
    new_arr <- new_arr[-i, ]
    ordered_arr <- sort(new_arr, point)
    
    for (q in seq(0.01, 0.99, 0.01)) {
      
      weights <- matrix(NA, (row - 1), 1)
      
      for (p in 1:(row - 1)) {
        weights[p] <- q^p
        ##weights[p] <- (k - p + 1)/k
      }
      
      for (k in 1:(row - 1)) {
        
        class <- kwNN(k, ordered_arr, weights)
        
        if (class != arr[i, 3]) {
          Q[k, (q * 100)] <- Q[k, (q * 100)] + 1
        }
      }
        
      
    }
    
  }
  
  print(Q)
  
  tmp <- Inf
  
  for (i in 1:(row - 1)) {
    if (min(Q[i, ]) < tmp) {
      tmp <- min(Q[i, ])
      ans <- c(i, which.min(Q[i, ]))
    }
  }
    
  print(ans)
  
  
  return(ans)
  
}


oneNN <- function(set, point){
  
  ## возьмём за ближайшего соседа первую точку в наборе
  min_distance <- distance_of_Euclid(set[1, 1:2], point)
  number_of_nearest <- 1
  
  N <- dim(set)[1]
  
  ## попробуем найти соседа ближе
  for (i in 2:N) {
    if (distance_of_Euclid(set[i, 1:2], point) < min_distance) {
      min_distance <- distance_of_Euclid(set[i, 1:2], point)
      number_of_nearest <- i
    }
  }
  
  ## возвращаем вид ириса ближайшего соседа
  return(set[number_of_nearest, 3])
  
}


main <- function(){
  ## создание тренировочной выборки по ширине и длине лепестка и виду ириса
  train_set <- iris[, 3:5]
  
  ## кол-во точек в тестовой выборке
  ##n <- as.numeric(readline(prompt = "Enter number of points in test set: "))
  n <- 10
  
  ## создание набора тестовых точек
  points <- cbind(runif(n, 1, 7), runif(n, 0, 2.5))
  
  ## рисуем выборку
  colors <- c("setosa" = "red", "versicolor" = "green4", "virginica" = "blue")
  
  tmp <- LOO(train_set)
  k <- tmp[1]
  q <- tmp[2]
  cat("Best of k is", k, " ", q)
  ##k <- 6
  
  plot(
    iris[, 3:4],
    xlim = c(1, 7),
    ylim = c(0, 2.5),
    pch = 21, 
    bg = colors[iris$Species],
    xlab = "Длина лепестка",
    ylab = "Ширина лепестка",
    main = "Карта классификации kNN для k = 6 (Ирисы Фишера)"
  )
  
  ## рисуем точки
  ##for (i in 1:n) {
    ##ordered_set <- sort(train_set, points[i, 1:2])
    ##points(points[i, 1], points[i, 2], pch = 1, col = colors[kNN(k, ordered_set)])
  ##}
  
  weights <- matrix(NA, 150, 1)
  
  for (i in seq(0.8, 7.2, 0.1)) {
    for (j in seq(-0.3, 2.9, 0.1)) {
      
      ordered_set <- sort(train_set, c(i, j))
      
      for (p in 1:150) {
        weights[p] <- q^p
        ##weights[p] <- (k - p + 1)/k
      }
      
      ##points(i, j, pch = 1, col = colors[oneNN(train_set, c(i, j))])
      
      ##points(i, j, pch = 1, col = colors[kNN(k, ordered_set)])
      
      ##points(i, j, pch = 1, col = colors[kwNN(k, ordered_set)])
      
    }
  }
  
  ##points(iris[, 3:4], pch = 21, bg = colors[iris$Species])
  
  legend(
    "bottomright",
    pch = c(21, 21, 21),
    col = c("red", "green4", "blue"),
    legend = c("setosa", "versicolor", "virginica")
  )
  
}

main()