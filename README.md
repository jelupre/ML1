# Задачи классификации

<h1>Метрические алгоритмы классификации</h1>


<table>
  <tr>
    <th>Метод</th>
    <th>Параметры</th>
    <th>Точность</th>
  </tr>
  <tr>
    <td>1nn</td>
    <td>k = 1</td>
    <td>0.0467</td>
  </tr>
  <tr>
    <td><strong>knn</strong></td>
    <td><strong>k = 6</strong></td>
    <td><strong>0.0333</strong></td>
  </tr>
  <tr>
    <td>kwnn</td>
    <td>k = 6, q = 0.56</td>
    <td>0.04</td>
  </tr>
  <tr>
    <td>PW (all kernels, except Gauss)</td>
    <td>h = 0.4</td>
    <td>0.04</td>
  </tr>
  <tr>
    <td>PW Gauss</td>
    <td>h = 0.1</td>
    <td>0.04</td>
  </tr>
  <tr>
    <td>MPF (all kernels, except Gauss)</td>
    <td>h = 1</td>
    <td>0.0467</td>
  </tr>
</table> 

<p>
  Все метрические алгоритмы классификации будем рассматривать на датасете "Ирисы Фишера", а конкретнее - тренировочная выборка по ширине и длине лепестка и виду ириса. В выборке будет 150 цветков. Вот так, собственно, выглядит наш обучающий набор.
</p>

![set_of_irises](https://github.com/jelupre/ML1/blob/master/images/irises.png)

<p>
  kNN расшифровывается как k Nearest Neighbor или k Ближайших Соседей — это один из самых простых алгоритмов классификации, также иногда используемый в задачах       
  регрессии. Благодаря своей простоте, он является хорошим примером, с которого можно начать знакомство с областью Machine Learning. 
</p>

<p>
  Задача классификации в машинном обучении — это задача отнесения объекта к одному из заранее определенных классов на основании его формализованных признаков. 
  Каждый из объектов в этой задаче представляется в виде вектора в N-мерном пространстве, каждое измерение в котором представляет собой описание одного из признаков 
  объекта. 
</p>

<h2>Метод ближайшего соседа</h2>

<p>
  Рассмотрим задачу kNN при k = 1 на языке R.
</p>

<h3>Алгоритм</h3>

Для классификации каждого из объектов тестовой выборки необходимо последовательно выполнить следующие операции:
<ul>
  <li>Вычислить расстояние до каждого из объектов обучающей выборки</li>
  <li>Найти объект из тренировочной выборки, от которого расстояние до классифицируемого объекта будет минимальным</li>
  <li>Класс классифицируемого объекта — это класс, ближайшего к нему объекта из обучающей выборки</li>
</ul>

<p>
  Введём число точек в тестовой выборке (n). Теперь создадим n точек с ограничениями по длине и ширине лепестка при помощи cbind и runif. Отбразим 
  тренировочную выборку. Рисуя тестовые точки, запускаем алгоритм 1NN для определения принадлежности одному из трёх существующих классов. 
  В самой функции 1NN ищем ближайшего по Евклидову расстоянию соседа для текущей точки и возвращаем вид ириса для неё же. 
  На рисунке ниже показан результат 10 случайно выбранных точек.
</p>

![1nn_10points](https://github.com/jelupre/ML1/blob/master/images/1nn_10points.png)

<p>
  Рассмотрим программную реализацию функции 1NN на языке программирования R.  
</p>


```R
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
```

Посмотрим на карту классификации для 1NN.

![1nn_map](https://github.com/jelupre/ML1/blob/master/images/1nn_map.png)

<h2>Метод k-ближайших соседей</h2>

<h3>Алгоритм</h3>

<p>Для классификации каждого из объектов тестовой выборки необходимо последовательно выполнить следующие операции:</p>

<ul>
    <li>Вычислить расстояние до каждого из объектов обучающей выборки</li>
    <li>Отобрать k объектов обучающей выборки, расстояние до которых минимально</li>
    <li>Класс классифицируемого объекта — это класс, наиболее часто встречающийся среди k ближайших соседей</li>
</ul>

<p>
  Работа алгоритма k-ближайших соседей начинается с сортировки всех элементов обучающей выборки по расстоянию относительно тестовой точки, далее отправляем в саму функцию kNN обучающую выборку, k и точку, которую нужно классифицировать. Проходим циклом от 1 до k и смотрим, каких ирисов больше. Относим классифицируемую точку к тому классу, которого среди этих k элементов больше. Ниже демонстрируется код на языке R.
</p>

```R
## сортировка тренировочного набора
ordered_set <- sort(train_set, points[i, 1:2])
## функция kNN
kNN <- function(k, ordered_arr){
  
  col_class <- dim(ordered_arr)[2]
  
  class <- names(which.max(table(ordered_arr[1:k, col_class])))
  
  return(class)
  
}
```

<p>
  Прежде чем запустить алгоритм kNN для тестовой выборки необходимо выбрать k, для этого будем использовать LOO (leave-one-out CV). Как он работает? На вход мы получаем тренировочную выборку. Поочерёдно "вытаскиваем" из набора по одной точке и для каждой точки и выборки без неё запускаем kNN, изменяя k от 1 до длины "новой" обучающей выборки. Сравниваем полученный класс точки с классом этой же точки из изначальной тренировочной выборки, если классы не совпадают, то увеличиваем количество ошибок на данном k на 1. Пройдя все точки по всем k, ищем минимальное количество ошибок, и индекс этого элемента и будет лучшим k для этой выборки.
</p>

```R
LOO <- function(arr){
  
  row <- dim(arr)[1]
  
  Q <- matrix(0, (row - 1), 1)
  
  for (i in 1:row) {
    
    point <- arr[i, 1:2]
    new_arr <- arr
    new_arr <- new_arr[-i, ]
    ordered_arr <- sort(new_arr, point)
    
    for (k in 1:(row - 1)) {
      
      class <- kNN(k, ordered_arr)
      
      if (class != arr[i, 3]) {
        Q[k] <- Q[k] + 1
      }
      
    }
    
  }
  
  min_k <- which.min(Q[1:(row - 1)])
  min_v <- Q[min_k]
  
  
  I <- matrix(1:(row - 1), (row - 1), 1)
  
  for (i in 1:(row - 1)) {
    Q[i] <- Q[i]/(row - 1)
  }
  
  ## график LOO и k
  plot(
    I[1:(row - 1)], 
    Q[1:(row - 1)], 
    type = "l", xlab = "k", ylab = "LOO",
    main = "LOO(k)"
  )
  points(min_k, min_v/(row - 1), pch = 21, bg = "black")
  
  
  range <- 5
  while (min_k - range < 0 || min_k + range > 149) {
    range <- range - 1
  }
  ## график LOO и k увеличенный масштаб
  plot(
    I[(min_k - range):(min_k + range)], 
    Q[(min_k - range):(min_k + range)], 
    xlim = c((min_k - range), (min_k + range)), 
    ylim = c(min_v/(row - 1) - 0.1, min_v/(row - 1) + 0.1), 
    type = "l", xlab = "k", ylab = "LOO",
    main = "LOO(k) (Окрестность точки)"
  )
  points(min_k, min_v/(row - 1), pch = 21, bg = "black")
  
  return(min_k)
  
}
```

![LOO_6nn](https://github.com/jelupre/ML1/blob/master/images/LOO_6nn.png) 

![LOO_6nn_near](https://github.com/jelupre/ML1/blob/master/images/LOO_6nn_near.png)

Для данной выборки LOO возвращает k, равный 6. Теперь запустим 6NN для 10 случаной выбранных точек.

![6nn_10points](https://github.com/jelupre/ML1/blob/master/images/6nn_10points.png)

Посмотрим на карту классификации для 6NN.

![6nn_map](https://github.com/jelupre/ML1/blob/master/images/6nn_map.png)

<h2>Метод k-ближайших взвешенных соседей</h2>

Метод kwNN отличается от kNN тем, что в нём добавляется вес к каждой точке в выборке, потом этот вес суммируется относительно классов, и, по итогу, вес какого класса будет больше, к тому классу и будет отнесена классифицируемая точка. Будем использовать весовую функцию q^i, где q = (0, 1), i = \[1, k\]. Возьмём k = 6 и для него найдём оптимальный q с точностью 0.01.

```R
LOO_q <- function(arr, k) {
  
  row <- dim(arr)[1]
  
  Q <- matrix(0, 99, 1)
  
  for (i in 1:row) {
    
    point <- arr[i, 1:2]
    new_arr <- arr
    new_arr <- new_arr[-i, ]
    ordered_arr <- sort(new_arr, point)
    
    weights <- matrix(0, (row - 1), 1)
    
    for (q in 1:99) {
      
      for (p in 1:(row - 1)) {
        weights[p] <- (q / 100)^p
      }
      
      class <- kwNN(k, ordered_arr, weights)
      
      if (class != arr[i, 3]) {
        Q[q] <- Q[q] + 1
      }
      
    }
    
  }
  
  min_q <- which.min(Q[1:99])
  min_v <- min(Q[1:99])
  
  I <- matrix(seq(0.01, 0.99, 0.01), 99, 1)
  
  for (i in 1:99) {
    Q[i] <- Q[i]/100
  }
  
  ## график LOO и q при k = 6
  plot(
    I[1:99], 
    Q[1:99], 
    type = "l", xlab = "q", ylab = "LOO",
    main = "LOO(q) при k = 6"
  )
  points(min_q/100, min_v/100, pch = 21, bg = "black")
  
  ## график LOO и q при k = 6 увеличенный масштаб
  plot(
    I[50:59], 
    Q[50:59], 
    type = "l", xlab = "q", ylab = "LOO",
    main = "LOO(q) при k = 6 (Окрестность точки)"
  )
  points(min_q/100, min_v/100, pch = 21, bg = "black")
  
  return(min_q/100)
  
}
```

![LOO_6wNN](https://github.com/jelupre/ML1/blob/master/images/LOO_6wnn.png)

![LOO_6wNN_near](https://github.com/jelupre/ML1/blob/master/images/LOO_6wnn_near.png)

Сам алгоритм kwNN выглядит следующим образом:

```R
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
```

Отобразим 10 случайно выбранных точек с помощью алгоритма kwNN, при k = 6, а q = 0.56.

![6wnn_10points](https://github.com/jelupre/ML1/blob/master/images/6wnn_10points.png)

Теперь посмотрим на карту классификации.

![6wnn_map](https://github.com/jelupre/ML1/blob/master/images/6wnn_map.png)

<h2>Преимущества kwNN</h2>
Недостаток kNN в том, что максимум может достигаться сразу на нескольких классах. В задачах с двумя классами этого можно избежать, если взять нечётное k. Более общая тактика, которая годится и для случая многих классов — ввести строго убывающую последовательность вещественных весов, задающих вклад i-го соседа в классификацию. Наглядно это видно на этом примере:

![knn_first_error](https://github.com/jelupre/ML1/blob/master/images/knn_first_error.png)

Мы выбрали k = 4, наш алгоритм берёт 2 красные и 2 синие ближайшие точки и возвращает тот класс, на котором максимум встретился раньше. Рассмотрим тот же случай методом ближайших взвешенных соседей, k = 4 и q = 0.5.

![kwnn_first_error](https://github.com/jelupre/ML1/blob/master/images/kwnn_first_error.png)

Второй значимый недостаток kNN перед kwNN можно заметить, когда среди k точек, большинство точек одного класса находятся дальше от классифицируемой, чем меньшинство точек. В таком случае kNN присвоит классифицуруемой точке класс большинства.

![knn_second_error](https://github.com/jelupre/ML1/blob/master/images/knn_second_error.png)

Однако kwNN учитывает близость точек отностельно классифицируемой, поэтому такой ошибки он не допускает.

![kwnn_second_error](https://github.com/jelupre/ML1/blob/master/images/kwnn_second_error.png)

<h2>Метод парзеновского окна</h2>

Давайте пойдем дальше и посмотрим, как можно еще обобщить метод ближайших соседей, чтобы справиться вот с теми двумя недостатками, которые были отмечены. Мы вольны выбирать функцию весов соседей, как мы хотим. Давайте сделаем так, чтобы вес соседа убывал по мере возрастания расстояния до него. Введем два новых понятия: это ядро, это функция положительная, не возрастающая на отрезке \[0, 1\], и ширина окна. И вот если мы функцию веса зададим как такую конструкцию — ядро от расстояния поделить на ширину окна, то получим такую вот взвешенную функцию, которая придает меньшие веса тем соседям, которые находятся дальше. И этот метод называется методом окна Парзена. Рассмотрим типы ядер. 

Прямоугольное ядро

![Formula_Rect](https://github.com/jelupre/ML1/blob/master/images/Formula_Rect.png)

Треугольное ядро

![Formula_Triangle](https://github.com/jelupre/ML1/blob/master/images/Formula_Triangle.png)

Ядро Епанечникова

![Formula_Epanech](https://github.com/jelupre/ML1/blob/master/images/Formula_Epanech.png)

Квартическое ядро

![Formula_Quadratic](https://github.com/jelupre/ML1/blob/master/images/Formula_Quadratic.png)

Гауссовское ядро

![Formula_Gauss](https://github.com/jelupre/ML1/blob/master/images/Formula_Gauss.png)

где
![Formula_r](https://github.com/jelupre/ML1/blob/master/images/Formula_r.png)

Рассмотрим программную реализацию:

```R
PW <- function(set, point, h) {
  
  weights <- matrix(0, 1, 3)
  row <- dim(set)[1]
  class <- c("setosa", "versicolor", "virginica")
  
  for (i in 1:row) {
    ##if (distance_of_Euclid(set[i, 1:2], point) <= h) {
      tmp <- K(set[i, 1:2], point, h)
      if (set[i, 3] == "setosa")      weights[1] <- weights[1] + tmp  
      if (set[i, 3] == "versicolor")  weights[2] <- weights[2] + tmp
      if (set[i, 3] == "virginica")   weights[3] <- weights[3] + tmp
    ##}
  }
  
  if (weights[1] + weights[2] + weights[3] == 0) {
    return("white")
  }
  else {
    return(class[which.max(weights)])
  }

}
```

Функция ядра:

```R
K <- function(x, y, h) {
  
  r <- distance_of_Euclid(x, y)/h
  E <- 3 * (1 - r^2) / 4
  Q <- 15 * (1 - r^2) / 16
  G <- ((2 * pi) ^ (-1 / 2)) * exp(-(r ^ 2) / 2)
  P <- 1/2
  Tr <- 1 - abs(r)
  
  #возращаем гауссовское ядро 
  return(G)
}
```

Так же, как и в прошлых алгоритмах, нам необходимо определить оптимальный h. Воспользуемся LOO.

```R
LOO_h <- function(arr) {
  
  row <- dim(arr)[1]
  
  Q <- matrix(0, 20, 1)
  
  for (i in 1:row) {
    
    point <- arr[i, 1:2]
    new_arr <- arr
    new_arr <- new_arr[-i, ]
    
    for (h in 1:20) {
      
      class <- PW(new_arr, point, h/10)
      
      if (class != arr[i, 3]) {
        
        Q[h] <- Q[h] + 1
        
      }
      
    }
    
  }
  
  print(Q)
  
  return(which.min(Q)/10)
  
}
```

Теперь посмотрим на карты классификаций и график LOO(h) для всех пяти ядер.

![PW_R_map](https://github.com/jelupre/ML1/blob/master/images/PW_R_map.png)
![LOO_Rect](https://github.com/jelupre/ML1/blob/master/images/LOO_Rect.png)

![PW_T_map](https://github.com/jelupre/ML1/blob/master/images/PW_T_map.png)
![LOO_Triangle](https://github.com/jelupre/ML1/blob/master/images/LOO_Triangle.png)

![PW_E_map](https://github.com/jelupre/ML1/blob/master/images/PW_E_map.png)
![LOO_Epanech](https://github.com/jelupre/ML1/blob/master/images/LOO_Epanech.png)

![PW_Q_map](https://github.com/jelupre/ML1/blob/master/images/PW_Q_map.png)
![LOO_Quadratic](https://github.com/jelupre/ML1/blob/master/images/LOO_Quadratic.png)

![PW_G_map](https://github.com/jelupre/ML1/blob/master/images/PW_G_map.png)
![LOO_Gauss](https://github.com/jelupre/ML1/blob/master/images/LOO_Gauss.png)


<h2>Метод потенциальных функций</h2>

Если в методе парзеновского окна центр окна поместить в классифицируемый
объект, то получим метод потенциальных функций.

Теперь ширина окна зависит не от классифицируемого объекта, а от обучающего.

Данный алгоритм имеет достаточно богатый набор из 2l параметров. Зафиксируем h = 0.4 и будем использовать те же ядра, что и в методе парзеновского окна, кроме гауссовского.

Рассмотрим функцию, которая находит потенциалы.

```R
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
```

Данная функция и есть основа метода потенциальных функций, она возвращает класс точки отностительно обучающей выборки.

```R
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
```

Теперь взглянем на результаты работы. Будем отображать карту потенциалов и карту классификации для каждого ядра.

![PFC_R_map.png](https://github.com/jelupre/ML1/blob/master/images/PFC_R_map.png)
![PFC_map_potential_Rect.png](https://github.com/jelupre/ML1/blob/master/images/PFC_map_potential_Rect.png)

![PFC_T_map.png](https://github.com/jelupre/ML1/blob/master/images/PFC_T_map.png)
![PFC_map_potential_Triangle.png](https://github.com/jelupre/ML1/blob/master/images/PFC_map_potential_Triangle.png)

![PFC_E_map.png](https://github.com/jelupre/ML1/blob/master/images/PFC_E_map.png)
![PFC_map_potential_Epanch.png](https://github.com/jelupre/ML1/blob/master/images/PFC_map_potential_Epanech.png)

![PFC_Q_map.png](https://github.com/jelupre/ML1/blob/master/images/PFC_Q_map.png)
![PFC_map_potential_Quartic.png](https://github.com/jelupre/ML1/blob/master/images/PFC_map_potential_Quartic.png)


<h1>Байесовские методы классификации</h1>

<h2>Линии нормального распределения</h2>

Рассмотрим многомерное нормальное распределение.
Вероятностное распределение с плотностью

![Formula_of_Gauss.png](https://github.com/jelupre/ML1/blob/master/images/Formula_of_Gauss.png)

называется <b>n-мерным многомерным нормальным (гауссовским) распределением</b> с математическим ожиданием (центром) µ (вектор из n вещественных чисел)  и ковариационной вещественной матрицей Σ размерностью n на n. Предполагается, что матрица Σ симметричная,
невырожденная, положительно определенная.


1) Рассмотрим случай, когда признаки некоррелированы, тогда линии уровня плотности распределения имеют форму эллипсоидов с центром µ и осями, параллельными осям координат.

2) Если признаки имеют одинаковые дисперсии, тогда эллипсоиды являются сферами.

3) Если признаки коррелированы, то матрица Σ не диагональна и линии уровня имеют форму эллипсоидов, оси которых повернуты (направлены вдоль собственных векторов матрицы Σ) относительно исходной системы координат.

