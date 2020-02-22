library("pander")
library("knitr")
library("lmtest")
library("psych")
library("memisc")
library("psych")
library("dplyr")
library("knitr")
library("rlms")
library("sandwich")
library("ggplot2")
library("scales")


df <- rlms_read("r22i_os_31.sav")
glimpse(df)


data0 <- transmute(df,
               wage = rj13.2,
               age = 2013 - rh6,
               sex = rh5,
               educ = r_diplom,
               place = status,
               satisf = rj1.1.1)
glimpse(data)
             

       
data1 <- data0[data0$place %in% c(NA, '1','2'),]
glimpse(data1)

data2 <- data1[data1$satisf %in% c(NA, '1','2'),]
glimpse(data2)


data3 <- data2[data2$educ %in% c(NA, '1','2','3','4','5','6'),]
summary(data3)

data4 <- mutate(data3,
       city = ifelse(place=='2',1,0),
       satisf = ifelse(satisf=='1',1,0),
       sex = ifelse(sex=='1',1,0),
       educ_l = ifelse(educ<4,1,0),
       educ_m = ifelse(educ==4,1,0),
       educ_ms = ifelse(educ==5,1,0),
       educ_h = ifelse(educ==6,1,0)
       )

data5 <- select(data4,-educ,-place)
glimpse(data5)
dim(data5)
data_clear <- na.omit(data5)
dim(data_clear)
describe(data5)
summary(data5)

ggplot(data = data_clear, aes(wage/1000)) + 
  geom_histogram(fill = "pink", binwidth = 10, color = "red") + 
  labs(title = "Гистограмма по возрасту", 
       x = "Возраст (в годах)", y = "Частота")+
  facet_grid(~sex)
data_clear$wage

model <- lm(data = data_clear,wage ~ sex + age + educ_m + educ_ms + educ_h + city + satisf)
res <- summary(model)            
res$fstatistic
res$adj.r.squared
res
data_clear$sex
coefs2 <- coeftest(model, vcov. = vcovHC(model))
pander(coefs2[, 1:4])
coefs2
