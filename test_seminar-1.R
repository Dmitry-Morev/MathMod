#Задача 1 - расчёт средних по столбцам
zan1 = list(mean(iris$Sepal.Length),mean(iris$Sepal.Width), mean(iris$Petal.Length), mean(iris$Petal.Width)); 
names(zan1)=c("Длина чашелистиков","Ширина Чашелистиков", "длина лепестков", "ширина лепестков"); zan1
#Задача 2
y=c()
for(i in 1:150)
{x=sum(iris[i,1:4])/4
y=c(x,y)
}
y
#Задача 3 - формирование последовательности из 1000 нуклеотидов и расчет долевого участия "А" и "Т"
DNA = sample((factor(rep(c("A","G","C","T"),c(1,1,1,1)))), size = 1000, replace = T); DNA
dna_at=c(summary(DNA)[c("A","T")], summary(DNA)["A"]/length(DNA), summary(DNA)["T"]/length(DNA))
#Задача 4 - вектор с 10000 буквами и подсчёт гласных
ABC = sample(letters, size = 10000, replace = T);ABC
Gl=0
for(i in 1:length(ABC))
{if (ABC[i]=="a"|ABC[i]=="e"|ABC[i]=="i"|ABC[i]=="o"|ABC[i]=="u"|ABC[i]=="y") {Gl=Gl+1} else {Gl=Gl+0}}
Gl
#Задача 5 - #Отсортируйте все виды в таблице iris по средней длинне лепестков.
#Результат должен быть фактором с градациями в виде имен видов с правильной последовательностью уровней. 
levels(iris$Species) = names(sort(tapply(iris$Petal.Length, iris$Species, mean)))
#Задача 6 функция для расчета медианы
  med = function(vector) 
    {l = length(vector)
    vector = sort(vector)
    nechetnost = (round((l/2),0)-l/2)
    med=0
    if (nechetnost == 0) {
      med = mean(c(vector[round((l/2),0)],vector[round((l/2),0)+1 ]))
    } else {
      med = vector[round((l/2),0)]
    }
        return(med)
  }
#Задача 7 Построить график зависимости для таблицы Ирисов
ggplot(iris, aes(x = Petal.Length, y = Sepal.Length, col = Species)) + geom_point(alpha = 0.4)
#Задача 8 
tapply(diamonds$price[diamonds$price >1000], diamonds$clarity[diamonds$price >1000], mean
#Задача 9
spir_cor = function(vec1, vec2)
  {
  vec1 = rank(vec1)
  vec2 = rank(vec2)
  delta = vec1 - vec2
  delta2 = delta^2
  sumsq = sum(delta2)
  r = 1 - ((6*sumsq)/(length(delta2)*(length(delta2)^2 - 1)))
  print("Коэф.кор.Спирмена")
  return(r)
}
#Задача 10. Линейная регрессия
Eddy = read.csv("eddypro.csv", header = T, dec = ".", sep = ",")
Eddy[Eddy == -9999] = NA
Eddy[Eddy == -9999.0] = NA
Eddy[,c(1,5,6,7,24,25,26, 32,33, 37, 38, 63, 70, 88:100)] = NULL
Eddy[,c(80:88)]=NULL
Eddy_red = subset(Eddy, DOY > 151 & DOY <243)
Eddy_red[,c(1,2)]=NULL
names(Eddy_red)
trend <- as.formula(paste("co2_flux~", paste(names(Eddy_red)[c(1:10,12:94)], collapse = "+")))
trend
fit <- lm(trend, data = Eddy_red)
summary(fit)
step = stepAIC(fit, direction = "forward")
summary(step)
round(cor(Eddy_red),2)
anova(step)
