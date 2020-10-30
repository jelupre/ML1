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
  
  return(E)
}


PF <- function(set, point, h, potentials){
  
  row <- dim(set)[1]
  w <- matrix(0, 3, 1)
  names(w) = c("setosa", "versicolor", "virginica")
  
  for (i in 1:row) {
    
    if (distance_of_Euclid(set[i, 1:2], point) <= h) {
      
      tmp <- K(set[i, 1:2], point, h)
      
      w[set[i, 3]] <- w[set[i, 3]] + potentials[i] * tmp
      
    }
    
  }
  
  if (max(w) == 0) {
    
    return("NA")
    
  }
  else {
    
    return(names(which.max(w)))
    
  }
  
}



find_potentials <- function(set, h, eps) {
  
  row <- dim(set)[1]
  err <- row
  potentials <- matrix(0, row, 1)
  
  while (err > eps) {
    
    err <- 0
    j <- 1
    
    while (TRUE) {
      
      class <- PF(set, set[j, 1:2], h, potentials)
      
      if (set[j, 3] != class) {
        
        ##print(j)
        #print(class)
        #print(set[j, 3])
        
        potentials[j] <- potentials[j] + 1
        break
        
      }
      
      j <- j %% row + 1
      
    }
    
    for (i in 1:row) {
      
      class <- PF(set[-i, ], set[i, 1:2], h, potentials)
      
      if (set[i, 3] != class) {
        
        err <- err + 1
        
      }
      
    }
    
    print(err)
    #print(potentials)
    
    
  }
  
  return(potentials)
  
}

install.packages("plotix")
require("plotix")

h <- 1
err <- 10

POT <- find_potentials(iris[ , 3:5], h, err)

row <- dim(POT)[1]

colors <- c("setosa" = "red", "versicolor" = "green", "virginica" = "blue", "NA" = "white")

plot(
    iris[ , 3], iris[ , 4], 
    pch = 21, bg = colors[iris[ , 5]], 
    xlim = c(0, 7), ylim = c(0, 3), 
    xlab = "Длина лепестка", 
    ylab = "Ширина лепестка", 
    main = "Карта потенциалов (Ядро Епанечникова, h = 1)"
)


for (i in 1:row) {
  
  if (POT[i] != 0) {
    
    points(iris[i, 3], iris[i, 4], pch = 21, bg = "black")
    draw.circle(iris[i, 3], iris[i, 4], h, 50, col = adjustcolor(colors[iris[i, 5]], 0.5 + 0.1 * POT[i]))
    
  }
  
}

legend(
  "bottomright",
  pch = c(21, 21, 21),
  col = c("red", "green4", "blue"),
  legend = c("setosa", "versicolor", "virginica")
)


plot(
  iris[ , 3], iris[ , 4], 
  pch = 21, bg = colors[iris[ , 5]], 
  xlim = c(0, 7), ylim = c(0, 3), 
  xlab = "Длина лепестка", 
  ylab = "Ширина лепестка", 
  main = "Карта классификации (Метод потенциальных функций, ядро Епанечникова, h = 1)"
)


for (i in seq(0, 7, 0.1)) {
  
  for (j in seq(0, 3, 0.1)) {
    
    points(i, j, pch = 1, col = colors[PF(iris[ , 3:5], c(i, j), h, POT)])
    
  }
  
}

legend(
  "bottomright",
  pch = c(21, 21, 21),
  col = c("red", "green4", "blue"),
  legend = c("setosa", "versicolor", "virginica")
)
