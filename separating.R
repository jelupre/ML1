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


set <- matrix(0, 6, 3)

set[1, ] <- c(as.numeric(-2), as.numeric(1), "blue")
set[2, ] <- c(as.numeric(-1), as.numeric(0), "blue")
set[3, ] <- c(as.numeric(0), as.numeric(1), "blue")
set[4, ] <- c(as.numeric(-1), as.numeric(4), "green")
set[5, ] <- c(as.numeric(-3), as.numeric(2), "green")
set[6, ] <- c(as.numeric(1), as.numeric(2), "green")

row <- dim(set)[1]
col <- dim(set)[2]

# количество признаков и количество классов
n <- col - 1
num_classes <- table(set[ , n + 1])
m <- dim(num_classes)

classes <- unique(set[, n + 1])

lambda <- c(1, 1)
P <- rep((1/m), m)

mu <- matrix(0, m, n)
sigma <- matrix(0, n, n * m)

# вычисление матожидания 
for (i in 1:m) {
  for (j in 1:n) {
    
    mu[i, j] <- mean(as.numeric(set[set[, n + 1] == classes[i], ][ , j]))
    
  }
}


print(mu)

temp <- rep(0, n)

# вычисление дисперсии
for (k in 1:m){
  
  subset <- set[set[, n + 1] == classes[k], ][ , ]
  l <- 3
  for (j in 1:l){
    for (i in 1:n) {
      temp[i] <- as.numeric(subset[j, i]) - mu[k, i] 
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

print(sigma)



plot(
  as.numeric(set[ , 1]), as.numeric(set[ , 2]), 
  pch = 21, bg = set[, 3], col = set[, 3],
  xlab = "x", ylab = "y",
  main = "Эллипс",
  xlim = c(-5, 5), ylim = c(-5, 5),
  asp = 1
)


x = seq(-50, 50, 0.1)
y = seq(-50, 50, 0.1)
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

