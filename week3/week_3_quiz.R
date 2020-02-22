# Для выполнения следующих заданий c 11 по 20 понадобится R. 
# Необходимо подгрузить данные по бриллиантам (diamonds) из пакета ggplot2 (df <- diamonds).
# Помимо этого, для некоторых заданий Вам могут понадобиться пакеты memisc и lmtest
library("memisc")
library("ggplot2")
library("lmtest")
library("dplyr")
#  Q11:Укажите количество переменных в данных по бриллиантам.
data <- diamonds
glimpse(data)
# Q12:Оцените модель log(pricei)=B1+B2carat+Ei. 
# Если масса бриллианта в каратах вырастет на 0.01 карата, 
# то на сколько процентов примерно поднимется цена?
model <- lm(data = data, log(price)~carat)
summary(model)
# Q13:Оценили модель pricei=B1+B2carati+B3yi+B4xi+Ei. 
# Проверьте гипотезу о значимости регрессии в целом на уровне значимости 1%.
model1 <- lm(data=data, price~carat+y+x)
summary(model1)
# Q14: Чему равен скорректированный R_adj^2  в модели pricei=B1+B2carati+Ei?
model2 <- lm(data=data, price~carat)
summary(model2)
# Q15:Оцените следующую модель: pricei=B1+B2carati+B3depthi+Ei. Чему равен AIC?
model3 <- lm(data=data, price~carat+depth)
summary(model3)
AIC(model3)
# Q16:Оцените следующие две модели:
# pricei=B1+B2carati+B3depthi+Ei,
# pricei = B1+B2carati + B3depthi + [ дамми-переменные для уровней cuti]+Ei
# Cравните эти две модели, проведя тест на несколько линейных ограничений. 
# Чему равно значение F-статистики?
glimpse(data)
data <- mutate_at(data, vars(cut), factor)
glimpse(data)
glimpse(data$cut)
model_1 <- lm(data = data, price~carat+depth+cut)
summary(model_1)
model_0 <-lm(data = data, price~carat+depth)
summary(model_0)
waldtest(model_0, model_1)
mtable(model_0, model_1)
# Q18:Если провести тест Рамсея для модели pricei=B1+B2carati+B3depthi+Bi на уровне значимости 1%, 
# то он покажет, что
model4 <- lm(data = data, price~carat+depth)
waldtest(model4)
# Q19:С помощью какой из следующих команд можно построить этот график?
df <- data
qplot(data = df, log(price), fill=cut, geom = "density", alpha = 0.5) + facet_grid(~cut)
qplot(data = df, log(price), fill=cut, geom = "density", alpha = 0.5) + facet_wrap(~cut)
qplot(data = df, log(price), fill=cut, geom = "density", alpha = 0.5)
# Q20:Верно ли, что прозрачность уровня IF самая лучшая 
# и бриллианты с такой прозрачностью в среднем самые дорогие вне зависимости от огранки?
qplot(data=df, log(carat), log(price), color = clarity) + facet_wrap(~cut)
