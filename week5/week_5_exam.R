library('ggplot2')
library('dplyr')
library('Ecdat')
library('lmtest')
library('glmnet')
library('car')
#��� ������ �� ������� 5 - 7 ��� ����������� R � ����� ������ ChickWeight. 
# ����� ���������� ������ � ���� ������, � ����������� �� �������� 
# (� ���� � ������� ��������, ���������� Time) � ���� ����� (���� �� ������ �����, ���������� Diet)

# Q5:������� ������� ��� ������� �� 10-�� ���� � ������� �������� (��� ����������� �� ���� �����). 
# ������� ����� � ��������� �� 2 ����� ����� �������, �� �������� ��� ����������!
chick_data <- ChickWeight
glimpse(chick_data)

# Q6:������� �� ����� ������ ���� � ������� ����� ������ ����� �� 21-�� ����?
chick_21 = chick_data[chick_data$Time==21,]
mean(chick_21[chick_21$Diet == 1,]$weight)
mean(chick_21[chick_21$Diet == 2,]$weight)
mean(chick_21[chick_21$Diet == 3,]$weight)
mean(chick_21[chick_21$Diet == 4,]$weight)

# Q7:������� ��������� ���� ������� �� ��� ������� � ��� �����. 
# ����� ����������� ������������ R^2 �� ��������? 
# ������� ����� � ��������� �� 2 ����� ����� �������, �� �������� ��� ����������!
model_ch <- lm(data=chick_data,
               weight~Time+Diet)
summary(model_ch)

# ��� ������� �� ������� 12-14 ��� ����������� R � ���������� ������ diamonds �� ������ ggplot2
# Q12: � ������� ����� �� ��������� ������ ����� ��������� ���� ������?
diamonds <- diamonds
qplot(data = diamonds, price,fill=cut)+facet_wrap(~clarity)
qplot(data = diamonds, log(price),fill=cut)+facet_grid(~cut)
qplot(data = diamonds, log(price),color=cut)+facet_grid(~cut)
qplot(data = diamonds, price,fill=cut)+facet_grid(~clarity)

# Q13:������� ��������� ������: pricei=B1+B2carati+B3tablei+B4xi+B5yi+B6zi+B7depthi+Ei. 
# ���� ����� ���������� �������� ������� � t-����������, ����������� ���������� ���������� ������������?
model_diam <- lm(data=diamonds,
                 price~carat+table+x+y+z+depth)
summary(model_diam)

# Q14: ������� ��������� ������: pricei=B1+B2carati+B3tablei+B4xi+B5yi+B6depthi+Ei. 
# ��������� 90% ������������� �������� ��� ������������ ��� ������ ������� ���������� 
# ������������ ����� ������� ����� ����������. ������� � ������ ������ ����� ������� ���������.
help("diamonds")
model_diam_2 <- lm(data=diamonds,
                 price~carat+table+x+y+depth)
summary(model_diam_2)

# ��� ���������� �������� � 19 �� 21 ��� ����������� R. 
# ���������� ����� `Ecdat` � ���������� ��� �������� `library(Ecdat)`, 
# ����������� ����� ������ `BudgetFood` �������� `data("BudgetFood")`.
# Q19: ������� �������� ������ ����������� ���� �������� ������������� �� ��� (wfoodwfood)
# �� ����� ����� �������� (totexptotexp) � ������� ������������� (sizesize):
# wfoodi=B1+B2totexpi+B3sizei+Ei
# ��������� 90%-�� ������������ �������� ��� ���� �������� �� ���
# ������������� �� 4 ������� � ������ ��������� ������� 700000.
# � ����� ������� ������ ����� ������� ���������, ����������� �� ���� ������ ����� �������.
bf <- BudgetFood
glimpse(bf)
model_bf <- lm(wfood~totexp+size, data=bf)
new_data <- data.frame(size=4,totexp=700000)
predict(model_bf, newdata=new_data, interval="prediction", level=0.9)

# Q20:��������� ���� ������ ��� �������� �������� 
# � ������������ �������� ������������ ������ �� 5%-�� ������ ����������.
reset(model_bf)

# Q21: � ������� F-����� �� ��������� ������������ � �������������� ������� ��������� �������� 
# H_0: B1,B2,B3 �� ������� �� ���� ��������, ������������ ������. 
# �������������� �������� ������� � ���, ��� ���� �� ���� �� ��� ������������� ���� ����������.
h <- na.omit(BudgetFood)
model_r <- lm(data=h, wfood~totexp+size)
model_ur <- lm(data=h, wfood~totexp*sex+size*sex)
waldtest(model_r,model_ur)
# ��� ������ �� ������� 26 - 28 ��� ����������� R � ��� �������� ��� ����� ������ mtcars.
# Q26:��������� ��������� ���������� ������� ������� 
# (���������� mpg - ����� ���� � ����� �� ���� ������ �������) �� ������� ����� ��������� (disp),
# �������� (hp) � ��� ���������� (wt). ����������� ������������ ������� ��������� (VIF). 
# ����� ����� ����� ������� � ������� ��������������������? 
# � ����� ������� ���������� �� ����������� VIF � ��������� �� 2 ����� ����� �������, 
# �� �������� ��� ����������!
cars <- mtcars
model_car <- lm(mpg~disp+hp+wt,data=cars)
vif(model_car)

# Q27:������� ���������� ������� ���������� ��� �������������� � ���������� ������ ����������� 
# (������� ����� ��������� (disp), �������� (hp) � ��� ���������� (wt)). 
#� ����� ������� ����� ������ ������� ���������� � ��������� �� ���� ������ ����� �������, 
# �� �������� ��� ����������!
# ����������: ���������� ����� ������ ������� ���������, ������� �� �������� ��������� ����� scale=TRUE.
# ������ �����, �� ��������, ��� � ������� ������ �� ������ ���� ���������!
# ���������: ����� �������� �������� ������� ���������, 
# ���� ��������������� �������� d.pca$x, ��� d.pca - ��� ������ � �������� ������������.
# ���������: ����� ������ ������� `v` ����� ��������� ���� ������, `sqrt(sum(v^2))`,
# ���� ������� �������� `norm(v, type="2")`
X0 <- model.matrix(data = cars, mpg~0+disp+hp+wt)
X0
cars_pca <- prcomp(X0, scale = TRUE)
v <- cars_pca$x[,1]
norm(v, type="2")
sqrt(sum(v^2))
# Q28:���������� �������� � ���� �� �������� ������������. 
# �� ������� ���������� R^2 � ��������� ������� ������� �� ������� ���������� 
# ��� ���������� ������� ������� ���������� (�� ��������� � �������������� ������ ������ ����)?
model_PCA1 <- lm(mpg~cars_pca$x[,1],data=cars)
model_PCA2 <- lm(mpg~cars_pca$x[,1]+cars_pca$x[,2],data=cars)
model_PCA3 <- lm(mpg~cars_pca$x[,1]+cars_pca$x[,2]+cars_pca$x[,3],data=cars)

summary(model_PCA1)
summary(model_PCA2)
summary(model_PCA3)
