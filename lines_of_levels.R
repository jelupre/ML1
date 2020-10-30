Gauss_distribution <- function(Sigma, mu, x) {
  
  n <- 2
  
  numerator <- exp((-1/2) %*% t(x - mu) %*% solve(Sigma) %*% (x - mu))
  denominator <- sqrt(det(Sigma) * (2 * pi)^n)
  
  #print(numerator)
  #print(denominator)
  
  return(numerator/denominator)
}

Sigma <- matrix(NA, 2, 2)
mu <- c(0, 0)

Sigma[1, 1] <- 1
Sigma[2, 2] <- 2
Sigma[1, 2] <- 1
Sigma[2, 1] <- 0

x <- seq(-5, 5, 0.1)
y <- seq(-5, 5, 0.1)


plot(-5:5, -5:5, type = "n",asp = 1)


for (i in x) {
  
  for (j in y) {
    
    color <- adjustcolor("white", Gauss_distribution(Sigma, mu, c(i, j)))
    points(i, j, pch = 21,col = color, bg = color)
    
  }
  
}


z = outer(x, y, function(x, y) {
  
  sapply(1:length(x), function(i) Gauss_distribution(Sigma, mu, c(x[i], y[i])))
  
})

contour(x,y,z,add = T ,asp = 1,lwd = 1)
