distance_of_Euclid <- function(a, b){
  
  return(sqrt(sum((a - b)^2)))
  
}

oneNN <- function(set, point){
  
  ## возьмём за ближайшего соседа первую точку в наборе
  min_distance <- distance_of_Euclid(set[1, 1:2], point)
  number_of_nearest <- 1
  
  ## попробуем найти соседа ближе
  for(i in 2:150){
    if (distance_of_Euclid(set[i, 1:2], point) < min_distance){
      min_distance <- distance_of_Euclid(set[i, 1:2], point)
      number_of_nearest <- i
    }
  }
  
  ## возвращаем вид ириса ближайшего соседа
  return(set[number_of_nearest, 3])
  
}

## создание тренировочной выборки по ширине и длине лепестка и виду ириса
train_set <- iris[, 3:5]

## создание набора тестовых точек
points <- cbind(runif(15, 0, 7), runif(15, 0, 2.5))

## рисуем выборку
colors <- c("setosa" = "red", "versicolor" = "green4", "virginica" = "blue")
plot(iris[, 3:4], pch = 21, bg = colors[iris$Species], asp = 1)

## рисуем точки
for (i in 1:15){
  points(points[i, 1], points[i, 2], pch = 24, bg = colors[oneNN(train_set, points[i])], asp = 1)
}

