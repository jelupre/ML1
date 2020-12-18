L_logic <- function(M) {
  return(log2(1 + exp(-M)))
}

L_ADALINE <- function(M) {
  return((M - 1)^2)
}

L_perceptron <- function(M) {
  return(max(-M, 0))
}

SigmoidFunction <- function(x) {
  return(1/(1 + exp(-x)))
}

Xl <- iris[1:100, 3:4]

l <- dim(Xl)[1]
n <- dim(Xl)[2] + 1

classes <- rep(NA, l)

for (i in 1:l) {
  ifelse(iris[i, 5] == "versicolor", classes[i] <- as.numeric(-1), classes[i] <- as.numeric(1))
}

Xl <- cbind(Xl, rep(-1, l), classes)

# нормализация
for (j in 1:(n - 1)){
  Xl[ , j] <- (Xl[ , j] - min(Xl[ , j])) / (max(Xl[ , j]) - min(Xl[ , j]))
}

plot(
  Xl[, 1], Xl[, 2],
  pch = 21,
  bg = ifelse(Xl[, n + 1] == -1, "green", "blue"),
  col = ifelse(Xl[, n + 1] == -1, "green", "blue"),
  main = "Сравнение",
  #main = "ADALINE",
  #main = "Персептрон Розенблатта",
  #main = "Логистическая регрессия",
  xlab = "Длина лепестка", ylab = "Ширина лепестка"
)


#ADALINE
# ------------------------------
# начальный вес
w <- c(runif(n, -1/(2*n), 1/(2*n)))

#лямбда
lambda <- 1/l

#  начальное значение функционала Q
Q <- 0
for (i in 1:l) {
  Q <- Q + L_ADALINE(crossprod(w, as.numeric(Xl[i, 1:n])) * Xl[i, n + 1])
}

# номер шага
it <- 0

# массив функционалов ошибок
Q_min <- Q
cnt <- 0
better_w <- w

# Стохастический градиентный спуск
repeat {
  
  it <- it + 1
  
  if (cnt < 100) {
    
    # выбор объекта
    i <- runif(1, 1, l) %/% 1
    xi <- as.numeric(Xl[i, 1:n])
    yi <- as.numeric(Xl[i, n + 1])
    
    # ошибка на объекте
    eps <- L_ADALINE(crossprod(w, xi) * yi)
    
    # шаг градиентного спуска
    eta <- as.numeric(1 / (sqrt(xi[1]^2 + xi[2]^2 + xi[3]^2)))
    w[1] <- w[1] - (crossprod(w, xi) - yi) * xi[1] * eta
    w[2] <- w[2] - (crossprod(w, xi) - yi) * xi[2] * eta
    w[3] <- w[3] - (crossprod(w, xi) - yi) * xi[3] * eta
    
    # пересчёт Q
    Q <- (1 - lambda) * Q + lambda * eps
    
    # print(c(it, Q, cnt))
    
    # проверка
    # если мы не встречаем минимальный Q через 100 шагов, то будем считать,
    # что Q сошёлся к Q_min
    if (Q >= Q_min) {
      cnt <- cnt + 1
    }
    else {
      cnt <- 0
      Q_min <- Q
      better_w <- w
    }
    
    # abline(a = w[3]/w[2], b = -w[1]/w[2], lwd = 1, col = "black")
    
  }
  else {
    break
  }
}

w <- better_w

abline(a = w[3]/w[2], b = -w[1]/w[2], lwd = 3, col = "red")

print(c(1, it, Q_min, better_w))

# ------------------------------



# персептрон
# ------------------------------
# начальный вес
w <- c(runif(n, -1/(2*n), 1/(2*n)))

#лямбда
lambda <- 1/l

# начальное значение функционала Q
Q <- 0
for (i in 1:l) {
  Q <- Q + L_perceptron(crossprod(w, as.numeric(Xl[i, 1:n])) * Xl[i, n + 1])
}


# номер шага
it <- 0

# массив функционалов ошибок
Q_min <- Q
cnt <- 0
better_w <- w

repeat {
  
  it <- it + 1
  
  if (cnt < 1000) {
    
    # выбор объекта
    i <- runif(1, 1, l) %/% 1
    xi <- as.numeric(Xl[i, 1:n])
    yi <- as.numeric(Xl[i, n + 1])
    
    # ошибка на объекте
    eps <- L_perceptron(crossprod(w, xi) * yi)
    
    # шаг градиентного спуска
    eta <- as.numeric(1 / (sqrt(xi[1]^2 + xi[2]^2 + xi[3]^2)))
    w[1] <- w[1] + xi[1] * yi * eta
    w[2] <- w[2] + xi[2] * yi * eta
    w[3] <- w[3] + xi[3] * yi * eta
    
    # пересчёт Q
    Q <- (1 - lambda) * Q + lambda * eps
    
    # print(c(it, Q, cnt))
    
    # проверка
    # если мы не встречаем минимальный Q через 1000 шагов, то будем считать,
    # что Q сошёлся к Q_min
    if (Q >= Q_min) {
      cnt <- cnt + 1
    }
    else {
      cnt <- 0
      Q_min <- Q
      better_w <- w
    }
    
    # abline(a = w[3]/w[2], b = -w[1]/w[2], lwd = 1, col = "black")
    
  }
  else {
    break
  }
}

w <- better_w

abline(a = w[3]/w[2], b = -w[1]/w[2], lwd = 3, col = "orange")

print(c(2, it, Q_min, better_w))

# ------------------------------




# логистический
# ------------------------------
# начальный вес
w <- c(runif(n, -1/(2*n), 1/(2*n)))

#лямбда
lambda <- 1/l

#  начальное значение функционала Q
Q <- 0
for (i in 1:l) {
  Q <- Q + L_logic(crossprod(w, as.numeric(Xl[i, 1:n])) * Xl[i, n + 1])
}

# номер шага
it <- 0

# массив функционалов ошибок
Q_min <- Q
cnt <- 0
better_w <- w

repeat {
  
  it <- it + 1
  
  if (cnt < 100) {
    
    # выбор объекта
    i <- runif(1, 1, l) %/% 1
    xi <- as.numeric(Xl[i, 1:n])
    yi <- as.numeric(Xl[i, n + 1])
    
    # ошибка на объекте
    eps <- L_logic(crossprod(w, xi) * yi)
    
    # шаг градиентного спуска
    eta <- as.numeric(1 / (sqrt(xi[1]^2 + xi[2]^2 + xi[3]^2)))
    w[1] <- w[1] + xi[1] * yi * eta * SigmoidFunction(crossprod(w, xi)*(-yi))
    w[2] <- w[2] + xi[2] * yi * eta * SigmoidFunction(crossprod(w, xi)*(-yi))
    w[3] <- w[3] + xi[3] * yi * eta * SigmoidFunction(crossprod(w, xi)*(-yi))
    
    # пересчёт Q
    Q <- (1 - lambda) * Q + lambda * eps
    
    # print(c(it, Q, cnt))
    
    # проверка
    # если мы не встречаем минимальный Q через 100 шагов, то будем считать,
    # что Q сошёлся к Q_min
    if (Q >= Q_min) {
      cnt <- cnt + 1
    }
    else {
      cnt <- 0
      Q_min <- Q
      better_w <- w
    }
    
    # abline(a = w[3]/w[2], b = -w[1]/w[2], lwd = 1, col = "black")
    
  }
  else {
    break
  }
}

w <- better_w

abline(a = w[3]/w[2], b = -w[1]/w[2], lwd = 3, col = "purple")

print(c(3, it, Q_min, better_w))

# ------------------------------


points(
  Xl[, 1], Xl[, 2],
  pch = 21,
  bg = ifelse(Xl[, n + 1] == -1, "green", "blue"),
  col = ifelse(Xl[, n + 1] == -1, "green", "blue")
)