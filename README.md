<h1>Метод k-ближайших соседей</h1>

<p>
  kNN расшифровывается как k Nearest Neighbor или k Ближайших Соседей — это один из самых простых алгоритмов классификации, также иногда используемый в задачах       
  регрессии. Благодаря своей простоте, он является хорошим примером, с которого можно начать знакомство с областью Machine Learning. 
</p>

<p>
  Задача классификации в машинном обучении — это задача отнесения объекта к одному из заранее определенных классов на основании его формализованных признаков. 
  Каждый из объектов в этой задаче представляется в виде вектора в N-мерном пространстве, каждое измерение в котором представляет собой описание одного из признаков 
  объекта. 
</p>

<p>
  Рассмотрим задачу kNN при k = 1 на языке R, используя датасет "Ирисы Фишера". 
  Посмотрим на карту классификаций.
</p>

![screenshot of sample](https://raw.githubusercontent.com/jelupre/ML1/master/iris_map.png)

<p>
  Теперь выберем тренировочную выборку по ширине и длине лепестка и виду ириса. В нашей выборке 150 ирисов.
</p>

![screenshot of sample](https://raw.githubusercontent.com/jelupre/ML1/master/iris_plot.png)

<p>
  Введём число точек в тестовой выборке (n). Теперь создадим n точек с ограничениями по длине и ширине лепестка при помощи функций cbind и runif. Отбразим 
  тренировочную выборку. Рисуя тестовые точки, запускаем алгоритм 1NN для определения принадлежности одному из трёх существующих классов. 
  В самой функции 1NN ищем ближайшего по Евклидову расстоянию соседа для текущей точки и возвращаем вид ириса для неё же. 
  На рисунке ниже показан результат пяти случайно выбранных точек.
</p>

distance_of_Euclid <- function(a, b){
  
  return(sqrt(sum((a - b)^2)))
  
}

oneNN <- function(set, point){
  
  ## возьмём за ближайшего соседа первую точку в наборе
  min_distance <- distance_of_Euclid(set[1, 1:2], point)
  number_of_nearest <- 1
  
  ## попробуем найти соседа ближе
  for(i in 2:N){
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

## кол-во точек в тренировочной выборке
N <- dim(train_set)[1]

## кол-во точек в тестовой выборке
n <- as.numeric(readline(prompt = "Enter number of points in test set: "))

## создание набора тестовых точек
points <- cbind(runif(n, 0.1, 6.9), runif(n, 0.1, 2.4))

## рисуем выборку
colors <- c("setosa" = "red", "versicolor" = "green4", "virginica" = "blue")
plot(iris[, 1:5], pch = 21, bg = colors[iris$Species], asp = 1)

## рисуем точки
for (i in 1:n){
  points(points[i, 1], points[i, 2], pch = 24, bg = colors[oneNN(train_set, points[i, 1:2])], asp = 1)
}

![screenshot of sample](https://raw.githubusercontent.com/jelupre/ML1/master/iris_done.png)


