exam %>% select(math)
exam
exam %>% select(english)
exam %>% select(class, math, english)
exam %>% select(-math)
exam %>% 
  filter(class == 3) %>% 
  select(class,english,science)%>%
  head(3)


exam %>% arrange(math)
exam %>% arrange(desc(math))
exam %>% select(math,english,science)%>%
  arrange(desc(math))%>%
  tail
exam %>% select(class,math,english,science)%>%
  arrange(class,desc(math))
kk = exam %>% 
  mutate(total = math + english + science) %>%
  head(10)  
kk %>%
  mutate(avg = total/3) %>%
  head(10)
kk %>%
  arrange(total) %>%
  head(10)

exam %>% summarise(mean_math = mean(math))

#반 별 평균 제시하라
exam %>%
  group_by(class)%>%
  summarize(mean_math=mean(math))%>%
  head(10)


exam %>%
  group_by(class) %>%
  summarise(mean_english = mean(english),
            sum_english = sum(english),
            median_english = median(english),
            student_number = n())

mpg=as.data.frame(ggplot2::mpg)
mpg %>%
  group_by(manufacturer, drv)%>%
  summarise(mean_cty = mean(cty)) %>%
  arrange(desc(mean_cty))%>%
  head(10)

mpg %>%
  group_by(manufacturer)%>%
  filter(class == "suv")%>%
  mutate(tot = (cty+hwy)/2)%>%
  summarise(mean_tot = mean(tot))%>%
  arrange(desc(mean_tot))%>%
  head(5)

test1 <- data.frame(id = c(1, 2, 3, 4, 5),
                    midterm = c(60, 80, 70, 90, 85))

test2 <- data.frame(id = c(1, 2, 3, 4, 5),
                    final = c(70, 83, 65, 95, 80))

total = left_join(test1,test2,by="id")
total

name <- data.frame(class = c(1, 2, 3, 4, 5),
                            teacher = c("kim", "lee", "park", "choi", "jung"))
name

exam_new <- left_join(exam, name, by = "class")
exam_new

group_a <- data.frame(id = c(1, 2, 3, 4, 5),
                      test = c(60, 80, 70, 90, 85))
group_b <- data.frame(id = c(6, 7, 8, 9, 10),
                      test = c(70, 83, 65, 95, 80))
group_a
group_b

group_all <- bind_rows(group_a, group_b)
group_all

library(dplyr)
setwd("d:/Rdata/Rdata")
bever <-read.csv("sales_data.csv")
df<-data.frame(bever)
df
View(df)

bever <- df %>% filter (CATEGORY %in% c("건강음료","과즙음료"))
bever
library(ggplot2)
qplot(bever)
head(bever,10)
dim(bever)
str(bever)
summary(bever)
setwd("d:/Rdata/Rdata")
write.csv(bever, file = "bever.csv")
