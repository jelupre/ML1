library(kernlab)

# ирисы
x <- iris[1:100, 3:4]
y <- c(rep(1,50), rep(-1, 50))
n <- dim(x)[2]

# нормализация
for (j in 1:n){
   x[ , j] <- (x[ , j] - min(x[ , j])) / (max(x[ , j]) - min(x[ , j]))
}

# SVM
d <- data.frame(x = x,y = y)
names(d) <- c("x1", "x2", "y")
svp <- ksvm(y ~ x1 + x2, data = d, type = "C-svc", C = 10, kernel = "polydot", scaled = c())
ymat <- ymatrix(svp)

# "каркас" графика
plot(
  c(min(x[,1]), max(x[,1])), c(min(x[,2]), max(x[,2])),
  type='n',xlab = "Длина лепестка", ylab = "Ширина лепестка", 
  main = "SVM. Полиномиальное ядро. С = 10"
)

# объекты выборки
points(x[-SVindex(svp),1], x[-SVindex(svp),2], pch = 1, col = ifelse(ymat[-SVindex(svp)] < 0, "red", "blue"))
points(x[SVindex(svp),1], x[SVindex(svp),2], pch = 16, col = ifelse(ymat[SVindex(svp)] < 0, "red", "blue"))

w <- colSums(coef(svp)[[1]] * x[SVindex(svp),])
b <- b(svp)

# разделяющая полоса
abline(b/w[2], -w[1]/w[2])
abline((b + 1)/w[2], -w[1]/w[2], lty = 2)
abline((b - 1)/w[2], -w[1]/w[2], lty = 2)

# ROC-кривая
l <- dim(d)[1]

# считаем количество элементов из положительного и отрицательного классов
l_neg <- length(which(d[1:l, 3] == -1))
l_pos <- length(which(d[1:l, 3] == 1))

FRP <- vector()
TRP <- vector()

# сортируем выборку по значению классификатора
a <- matrix(0,0,2)
for (i in 1:l) {
   a <- rbind(a, c(i, as.double(w %*% as.double(d[i, 1:2]))))
}
d <- cbind(d, a[,2])
d <- d[order(d[,4], decreasing = TRUE), ]

# устанавливаем начальные значения
FRP[1] <- 0
TRP[1] <- 0
AUC <- 0

# последовательность точек для ROC-кривой
for (i in 1:l) {
   if (d[i, 3] ==  -1) {
     FRP[i + 1] <- FRP[i] + 1 / l_neg
     TRP[i + 1] <- TRP[i]
     AUC <- AUC + TRP[i + 1] / l_neg
   } 
   else {
     FRP[i + 1] <- FRP[i]
     TRP[i + 1] <- TRP[i] + 1 / l_pos
   }
}

# отрисовка
plot(c(0, 1), c(0, 1), type = "n", xlab = "FRP", ylab = "TRP", main = "ROC-кривая")
points(FRP, TRP, type = "l")
points(c(0, 1), c(0, 1), type = "l")
legend("bottomright", paste("AUC = ", AUC))