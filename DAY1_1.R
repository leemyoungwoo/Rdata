x=rnorm(100,175,2)
x
hist(x,breaks = 5, probability = T)
lines(density(x), col=8, type='h',lwd=0.5)
shapiro.test(x)

a<-1
a
b<-2
b
c<-3
c
d<-4
d
a+b
a+b+c+d
c(1,2,3,4,5)
c
var1<-c(1,2,5,7,8)
var1
var2=c(1:5)
var2
var3<-seq(1,5)
var3
var4=seq(1,10,by=2)
var4
var4=var1+2
var4
str1<-"a"
str1="abcdefg"
str1
str5=c("a","b","c")
str5


x=c(1,4,7)
x
mean(x)
max(x)
min(x)
sd(x)
install.packages("ggplot2")
library(ggplot2)
x
x <- c("a","a","b","c")
x
qplot(x)
qplot(data = mpg, x = hwy)
qplot(data = mpg, x = cty)
qplot(data = mpg, x = drv, y = hwy)
qplot(data = mpg, x = drv, y = hwy, geom = "line")
qplot(data = mpg, x = drv, y = hwy, geom = "boxplot")
qplot(data = mpg, x = drv, y = hwy, geom = "boxplot", colour = drv)
?qplot
student=c(80,60,70,50,90)
student
mean(student)
sd(student)
english <- c(90, 80, 60, 70)  # 영어 점수 변수 생성english
english
math <- c(50, 60, 100, 20)
math
df_midterm <- data.frame(english, math)
df_midterm
class <- c(1, 1, 2, 2)
class
df_midterm <- data.frame(english, math, class)
df_midterm
mean(df_midterm$english)
mean(df_midterm$math)
df_midterm <- data.frame(english = c(90, 80, 60, 70),math = c(50, 60, 100, 20),class = c(1, 1, 2, 2))
df_midterm
sd(df_midterm$english)
var(df_midterm$english)
sqrt(var(df_midterm$english))
install.packages("readxl")
library(readxl)
sales <- data.frame(fruit = c("사과", "딸기", "수박"),                    price = c(1800, 1500, 3000),                    volume = c(24, 38, 13))
sales
mean(sales$price)
mean(sales$volume)
df_exam <- read_excel("excel_exam.xlsx")
setwd("d:/Rdata")
df_midterm
write.csv(df_midterm, file = "df_midterm.csv")
df_mid_test <- read.csv("df_midterm.csv")
df_mid_test

x=rnorm(100)
hist(x,probability=T)
lines(density(x), type='h',col=2)
shapiro.test(x)

plot.new()

hist(mpg$hwy,probability = T)
lines(density(mpg$hwy),col=2,type='h',lwd=1)
shapiro.test(mpg$hwy)


setwd("d:/Rdata/Rdata")
exam <- read.csv("csv_exam.csv")
exam

head(exam)
tail(exam)
head(exam,4)
head(exam,21)
View(exam)
dim(exam)
str(exam)
summary(exam)
boxplot(exam$math,horizontal=T, col=2)
hist(exam$math)

x=sample(0:100,80,replace = T)
plot(x,pch=ifelse(x>=60,7,15),col=ifelse(x>=60,2,5))
abline(h=60,col=2,lwd=2)


mpg <- as.data.frame(ggplot2::mpg)
head(mpg)
str(mpg)
View(mpg)


install.packages("dplyr")
library(dplyr)
df_raw <- data.frame(var1 = c(1, 2, 1),                     var2 = c(2, 3, 2))
df_raw
df_new <- df_raw 
df_new
df_new <- rename(df_new, v2 = var2)
df_new


mpg <- as.data.frame(ggplot2::mpg)
mpg_new <- mpg  

mpg_new <- rename(mpg_new, city = cty)
mpg_new <- rename(mpg_new, highway = hwy)
head(mpg_new)
setwd("d:/Rdata")
write.csv(mpg_new, file = "mpg_new.csv")


df <- data.frame(var1 = c(4, 3, 8),                 var2 = c(2, 6, 1))
df

df$var_sum <- df$var1 + df$var2
df

df$var_mean <- (df$var1 + df$var2)/2
df

mpg$total <- (mpg$cty + mpg$hwy)/2
head(mpg)


mean(mpg$total)
summary(mpg$total)
boxplot(mpg$total, horizontal = T, col=2)
hist(mpg$total)
mpg$test=ifelse(mpg$total>=20,"pass","fail")
head(mpg,20)
qplot(mpg$test)

table(mpg$test)

library(ggplot2)
qplot(mpg$test)


mpg$grade2 <- ifelse(mpg$total >= 30, "A",                     ifelse(mpg$total >= 25, "B",                            ifelse(mpg$total >= 20, "C", "D")))
qplot(mpg$grade2)

midwest <- as.data.frame(ggplot2::midwest)
head(midwest)
tail(midwest)
View(midwest)
dim(midwest)
str(midwest)
summary(midwest)

library(dplyr)
midwest <- rename(midwest, total = poptotal)
midwest <- rename(midwest, asian = popasian)

midwest$ratio <- midwest$asian/midwest$total*100
hist(midwest$ratio)

mean(midwest$ratio)

midwest$group <- ifelse(midwest$ratio > 0.4872462, "large", "small")


table(midwest$group)

library(ggplot2)

qplot(midwest$group)


library(dplyr)
setwd("d:/Rdata/Rdata")
exam <- read.csv("csv_exam.csv")
class13 = exam %>% filter(class==1 | class==3)
exam
class13


exam %>% filter(class !=1)


exam %>% filter(math>50)
exam %>% filter(math<50)
exam %>% filter(english>=98)

exam %>% filter (class %in% c(1,3,5))

10%%3
10%/%3

mpg <- as.data.frame(ggplot2::mpg)

mpg_a <-mpg %>% filter(displ<=4)
mpg_b <-mpg %>% filter(displ>=5)

mean(mpg_a$hwy)

mpg_audi <-mpg %>% filter(manufacturer =="audi")
mpg_toyota<-mpg %>% filter(manufacturer == "toyota")

mean(mpg_audi$cty)
mean(mpg_toyota$cty)


mpg_new2 <- mpg %>% filter(manufacturer %in% c("chevrolet", "ford", "honda"))
mean(mpg_new$hwy)
setwd("d:/Rdata")
write.csv(mpg_new2, file = "mpg_new2.csv")
qplot(mpg_new2)
qplot(mpg_new2)
qplot(mpg_new2)
qplot(mpg_new2)
qplot(mpg_new2)
