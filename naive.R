naive_Bayes <- function(Py, n, m, mu, sigma, point){
  
  p <- rep(0, m)
  
  for (i in 1:m) {
    
    p[i] <- Py[i]
    
    for (j in 1:n) {
      
      p[i] <- p[i] * exp((-(point[j] - mu[i, j])^2 * (1/sigma[i, j]))/2) / sqrt(2 * pi * sigma[i, j])
      
    }
    
  }
  
  return(classes[which.max(p)])
  
}

set <- iris[ , 3:5]

row <- dim(set)[1]
col <- dim(set)[2]

num_classes <- table(set[3])
classes <- unique(set[, 3])
colors <- c("setosa" = "red", "versicolor" = "green", "virginica" = "blue")

# количество признаков и количество классов
n <- col - 1 
m <- dim(num_classes)

# априорная вероятность
Py <- rep(0, m) 

for (i in 1:m) {
  Py[i] <- num_classes[i] / row
}

# матожидание и дисперсия
mu <- matrix(0, m, n)
sigma <- matrix(0, m, n)


for (i in 1:m) {
  
  for (j in 1:n) {
    
    #print(set[set[, 3] == classes[i], ][, j])
    
    mu[i, j] <- mean(set[set[, 3] == classes[i], ][ , j])
    sigma[i, j] <- var(set[set[, 3] == classes[i], ][ , j])
    
  }
  
}



plot(
    set[ , 1], set[ , 2], 
    pch = 21, bg = colors[set[, 3]], col = colors[set[, 3]],
    xlab = "Длина лепестка", ylab = "Ширина лепестка",
    main = "Карта классификации (Наивный нормальный Байесовский классификатор)",
    xlim = c(1, 7), ylim = c(0, 2.5)
)


# карта классификации
for (i in seq(0.8, 7.2, 0.1)) {
  
  for (j in seq(-0.3, 2.9, 0.1)) {
    
    points(i, j, pch = 1, col = colors[naive_Bayes(Py, n, m, mu, sigma, c(i, j))])
    
  }
  
}


legend(
  "bottomright",
  pch = c(21, 21, 21),
  col = c("red", "green4", "blue"),
  legend = c("setosa", "versicolor", "virginica")
)
