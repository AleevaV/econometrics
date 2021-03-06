# ��� ���������� ��������� ������� c 11 �� 20 ����������� R. 
# ���������� ���������� ������ �� ����������� (diamonds) �� ������ ggplot2 (df <- diamonds).
# ������ �����, ��� ��������� ������� ��� ����� ������������ ������ memisc � lmtest
library("memisc")
library("ggplot2")
library("lmtest")
library("dplyr")
#  Q11:������� ���������� ���������� � ������ �� �����������.
data <- diamonds
glimpse(data)
# Q12:������� ������ log(pricei)=B1+B2carat+Ei. 
# ���� ����� ���������� � ������� �������� �� 0.01 ������, 
# �� �� ������� ��������� �������� ���������� ����?
model <- lm(data = data, log(price)~carat)
summary(model)
# Q13:������� ������ pricei=B1+B2carati+B3yi+B4xi+Ei. 
# ��������� �������� � ���������� ��������� � ����� �� ������ ���������� 1%.
model1 <- lm(data=data, price~carat+y+x)
summary(model1)
# Q14: ���� ����� ����������������� R_adj^2  � ������ pricei=B1+B2carati+Ei?
model2 <- lm(data=data, price~carat)
summary(model2)
# Q15:������� ��������� ������: pricei=B1+B2carati+B3depthi+Ei. ���� ����� AIC?
model3 <- lm(data=data, price~carat+depth)
summary(model3)
AIC(model3)
# Q16:������� ��������� ��� ������:
# pricei=B1+B2carati+B3depthi+Ei,
# pricei = B1+B2carati + B3depthi + [ �����-���������� ��� ������� cuti]+Ei
# C������� ��� ��� ������, ������� ���� �� ��������� �������� �����������. 
# ���� ����� �������� F-����������?
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
# Q18:���� �������� ���� ������ ��� ������ pricei=B1+B2carati+B3depthi+Bi �� ������ ���������� 1%, 
# �� �� �������, ���
model4 <- lm(data = data, price~carat+depth)
waldtest(model4)
# Q19:� ������� ����� �� ��������� ������ ����� ��������� ���� ������?
df <- data
qplot(data = df, log(price), fill=cut, geom = "density", alpha = 0.5) + facet_grid(~cut)
qplot(data = df, log(price), fill=cut, geom = "density", alpha = 0.5) + facet_wrap(~cut)
qplot(data = df, log(price), fill=cut, geom = "density", alpha = 0.5)
# Q20:����� ��, ��� ������������ ������ IF ����� ������ 
# � ���������� � ����� ������������� � ������� ����� ������� ��� ����������� �� �������?
qplot(data=df, log(carat), log(price), color = clarity) + facet_wrap(~cut)
