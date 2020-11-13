plugin <- function(Py, lambda, n, m, mu, sigma, point) {
  
  point <- as.numeric(point)
  
  p <- rep(0, m)
  
  for (i in 1:m) {
    
    p[i] <- Py[i] * lambda[i]
    
    S <- matrix(0, n, n)
    for (i1 in 1:n) {
      for (i2 in 1:n) {
        
        S[i1, i2] <- sigma[i1, i2 + (i - 1) * n]
        
      }
    }
    
    p[i] <- p[i] *  exp(-(1/2) * t(point - mu[i, ]) %*% solve(S) %*% (point - mu[i, ])) / sqrt((2 * pi)^n * det(S))
    
  }
  
  return(classes[which.max(p)])
  
}


Gauss_distribution <- function(Sigma, mu, x) {
  
  n <- 2
  
  numerator <- exp((-1/2) %*% t(x - mu) %*% solve(Sigma) %*% (x - mu))
  denominator <- sqrt(det(Sigma) * (2 * pi)^n)
  
  #print(numerator)
  #print(denominator)
  
  return(numerator/denominator)
}


set <- iris[ , 3:5]

row <- dim(set)[1]
col <- dim(set)[2]

# количество признаков и количество классов
n <- col - 1 
num_classes <- table(set[n + 1])
m <- dim(num_classes)


classes <- unique(set[, n + 1])
colors <- c("setosa" = "red", "versicolor" = "green", "virginica" = "blue")

# априорная вероятность
Py <- rep(0, m) 

for (i in 1:m) {
  Py[i] <- num_classes[i] / row
}

# "штраф" за ошибку
lambda <- c(1, 1, 1)

# матожидание и дисперсия
mu <- matrix(0, m, n)
sigma <- matrix(0, n, n * m)

# вычисление матожидания 
for (i in 1:m) {
  for (j in 1:n) {
    
    mu[i, j] <- mean(set[set[, n + 1] == classes[i], ][ , j])
    
  }
}

#print(mu)

temp <- rep(0, n)

# вычисление дисперсии
for (k in 1:m){
  
  subset <- set[set[, n + 1] == classes[k], ][ , ]
  l <- dim(subset)[1]
  
  for (j in 1:l){
    for (i in 1:n) {
      temp[i] <- subset[j, i] - mu[k, i] 
    }
    
    S <- temp %*% t(temp)
    
    for (i1 in 1:n) {
      for (i2 in 1:n) {
        
        sigma[i1, i2 + (k - 1) * n] <- sigma[i1, i2 + (k - 1) * n] + S[i1, i2]
        
         
      }
    }
    
  }
  
  for (i1 in 1:n) {
    for (i2 in 1:n) {
      
      sigma[i1, i2 + (k - 1) * n] <- sigma[i1, i2 + (k - 1) * n] / (l - 1)
      
    }
  }
  
}


#print(sigma)


plot(
  set[ , 1], set[ , 2], 
  pch = 21, bg = colors[set[, 3]], col = colors[set[, 3]],
  xlab = "Длина лепестка", ylab = "Ширина лепестка",
  main = "Квадратичная разделяющая поверхность (Подстановочный алгоритм)",
  xlim = c(1, 7), ylim = c(0, 2.5)
)


# карта классификации
for (i in seq(0.8, 7.2, 0.1)) {
  
  for (j in seq(-0.3, 2.9, 0.1)) {
    
    points(i, j, pch = 1, col = colors[plugin(Py, lambda, n, m, mu, sigma, c(i, j))])
    
  }
  
}


legend(
  "bottomright",
  pch = c(21, 21, 21),
  col = c("red", "green4", "blue"),
  legend = c("setosa", "versicolor", "virginica")
)


Q <- 0

for (i in 1:row) {
  
  point <- set[i, 1:n]
  class <- plugin(Py, lambda, n, m, mu, sigma, point)
  
  if (class != set[i, n + 1]) {
    
    Q <- Q + 1
    
  }
}

Q <- Q / 150

print(Q)


plot(
  set[ , 1], set[ , 2], 
  pch = 21, bg = colors[set[, 3]], col = "black",
  xlab = "Длина лепестка", ylab = "Ширина лепестка",
  main = "Линии уровня нормального распределения (Подстановочный алгоритм)",
  xlim = c(1, 7), ylim = c(0, 2.5)
)

for (k in 1:m){

  S <- matrix(0, n, n)
  for (i1 in 1:n) {
    for (i2 in 1:n) {
      
      S[i1, i2] <- sigma[i1, i2 + (k - 1) * n]
      
    }
  }
  
  x <- seq(1, 7, 0.1)
  y <- seq(0, 3, 0.1)
  
  
  for (i in x) {
    
    for (j in y) {
      if (i == 0  && j == 0) {
        print(Gauss_distribution(S, mu[k, ], c(i, j)))
      }
      
      
      color <- adjustcolor(colors[classes[k]], Gauss_distribution(S, mu[k, ], c(i, j)) * 2)
      points(i, j, pch = 21,col = color, bg = color)
      
    }
    
  }
  
  z = outer(x, y, function(x, y) {
    
    sapply(1:length(x), function(i) Gauss_distribution(S, mu[k, ], c(x[i], y[i])))
    
  })
  
  contour(x,y,z,add = T ,asp = 1,lwd = 1)
}

legend(
  "bottomright",
  pch = c(21, 21, 21),
  col = c("red", "green4", "blue"),
  legend = c("setosa", "versicolor", "virginica")
)



plot(
  set[ , 1], set[ , 2], 
  pch = 21, bg = colors[set[, 3]], col = colors[set[, 3]],
  xlab = "Длина лепестка", ylab = "Ширина лепестка",
  main = "Линии уровня нормального распределения (Подстановочный алгоритм)",
  xlim = c(1, 7), ylim = c(0, 2.5)
)


x = seq(0,7,0.1)
y = seq(-1,2.5,0.1)
for (i in 1:m) {
 for (j in (i + 1):m) {
   if (j > m) break
   
   form = function(x,y,mu,S) {
     a = S[1,1]
     b = S[1,2]
     c = S[2,1]
     d = S[2,2]
     a*x*x + (b+c)*x*y + d*y*y + 
     (-2*a*mu[1]-b*mu[2]-c*mu[2])*x +
     (-b*mu[1]-c*mu[1]-2*d*mu[2])*y +
     (a*mu[1]*mu[1]+(b+c)*mu[1]*mu[2]+d*mu[2]*mu[2])
   }
   
   z = outer(x,y,function(x,y) 
     form(x,y,mu[i, ],solve(sigma[ , ((i - 1) * n + 1):(i * n)])) -
     form(x,y,mu[j, ],solve(sigma[ , ((j - 1) * n + 1):(j * n)]))
   )
   contour(x,y,z,levels=0,add=T)
 }
}


legend(
  "bottomright",
  pch = c(21, 21, 21),
  col = c("red", "green4", "blue"),
  legend = c("setosa", "versicolor", "virginica")
)