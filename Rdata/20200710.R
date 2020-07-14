#Logistic Regression 
setwd(("c:/Rdata"))
data=read.csv("programming.csv")
head(data)
model=glm(Success~Experience,data=data,
          family = binomial(logit))
summary(model)
cbind(data$Experience, model$fitted.values)
plot(Success~Experience,data=data)
points(model$fitted.values~data$Experience,col=2)

table(data$Success,model$fitted.values>0.5)
c('민감도'=8/11,'특이도'=11/14)


data=read.csv("coupon.csv")
head(data)
model2=glm(cbind(N_redeemed,N-N_redeemed)~Price_reduc,data=data,
    family = binomial(logit))
summary(model2)
exp(0.096834)

data=read.csv("disease.csv")
head(data)
model3=glm(disease~.,data=data,family = binomial(logit))
summary(model3)


model4=glm(disease~age+sector,data=data,family = binomial(logit))
summary(model4)
anova(model3,model4,test='Chisq')


table(data$disease)
31/98
kk=table(data$disease,model4$fitted.values>0.3163265)
kk
sum(kk)
reduce_M=c('민감도'=23/31,'특이도'=47/(47+20))
kk1=table(data$disease,model3$fitted.values>0.3163265)
kk1
fulmode_M=c('민감도'=23/31,'특이도'=49/(49+18))
reduce_M
fulmode_M
err_m1=28/sum(kk)
kk1
err_m2=26/sum(kk1)
err_m1
err_m2
install.packages("Deducer")
library(Deducer)
rocplot(model3)


###연습문제풀이
data=read.csv("flushot.csv")
head(data)
log_model=glm(flushot~.,data=data,family = binomial(logit))
summary(log_model)
exp(0.07279)
exp(-0.09899)
exp(0.43397)
log_model2=glm(flushot~age+aware,data=data,family = binomial())
summary(log_model2)
table(data$flushot)
24/(134+24)
tab_01=table(data$flushot,log_model2$fitted.values>0.1)
tab_015=table(data$flushot,log_model2$fitted.values>0.15)
tab_02=table(data$flushot,log_model2$fitted.values>0.2)
c('민감도'=19/(5+19),'특이도'=95/(95+40),'에러율'=45/sum(tt))
rocplot(log_model2)

tab_01
tab_015
tab_02
res01=c(민감도=tab_01[2,2]/sum(tab_01[2,]),
           특이도=tab_01[1,1]/sum(tab_01[1,]),
            에러율=(tab_01[1,2]+tab_01[2,1])/sum(tab_01))
res01
res015=c(민감도=tab_015[2,2]/sum(tab_015[2,]),
           특이도=tab_015[1,1]/sum(tab_015[1,]),
           에러율=(tab_015[1,2]+tab_015[2,1])/sum(tab_015))
res015
res02=c(민감도=tab_02[2,2]/sum(tab_02[2,]),
           특이도=tab_02[1,1]/sum(tab_02[1,]),
           에러율=(tab_02[1,2]+tab_02[2,1])/sum(tab_02))
res02
res01
res015
res02

model4$fitted.values

jang=function(){
  k=seq(0.01,0.5,0.01)
  
  n=length(k)
  
  err_min=vector(length=n)
  sens=vector(length=n)
  spec=vector(length=n)
  
  for(i in 1:n){
    tab=table(data$flushot,log_model2$fitted.values>k[i])
    res=c(민감도=tab[2,2]/sum(tab[2,]),
             특이도=tab[1,1]/sum(tab[1,]),
             에러율=(tab[1,2]+tab[2,1])/sum(tab))
    
    err_min[i]=(tab[1,2]+tab[2,1])/sum(tab)
    sens[i]=tab[2,2]/sum(tab[2,])
    spec[i]=tab[1,1]/sum(tab[1,])
    print(res)
  }
  print(err_min)
  print(paste("최소의 Error Rate=",min(err_min),"이다"))
  index=which(err_min<=min(err_min))
  print(index)
  print(paste("해당하는 민감도=",sens[min(index)],"이다."))
  print(paste("해당하는 특이도=",spec[min(index)],"이다."))
  print(paste("해당하는 에러율=",err_min[min(index)],"이다."))
  print(paste("해당하는 cutoff=",k[min(index)],"이다."))
  
  plot(1-spec,sens,col=2)
}
jang()


###다변량분석
crime=crime = read.csv("http://datasets.flowingdata.com/crimeRatesByState-formatted.csv")
head(crime)
rownames(crime)=crime[,1]
rownames(crime)
stars(crime[,2:8],flip.labels = FALSE,
      draw.segments=TRUE,key.loc = c(15,2))

install.packages("aplpack")
library(aplpack)
faces(crime[,2:8])


education=read.csv("http://datasets.flowingdata.com/education.csv")
head(education)

library(lattice)
parallel(education[,2:7],horizontal.axis=FALSE,col=1)
summary(education$reading)
color=education$reading>523
color
color+1
parallel(education[,2:7],horizontal.axis=FALSE,col=color+1)
summary(education$dropout_rate)
color=education$dropout_rate>5.3
color
color+1
parallel(education[,2:7],horizontal.axis=FALSE,col=color+1)

data=read.csv(("20140528_baseball.csv"))
head(data)
model=prcomp(data[,2:6],scale=T)
model
summary(model)
plot(model)
head(data)
rownames(data)=data[,1]
head(data)
model=prcomp(data[,2:6],scale=T)
biplot(model)


##다변량연습문제
#1.data=read.csv("20140528_baseball.csv")
head(data)
rownames(data)=data[,1]
head(data)
stars(data[,2:6],flip.labels = F,key.loc=c(9,3),draw.segments = T)
faces(data[,2:6])
#2.
bb2013=read.csv("2013_baseball.csv")
head(bb2013)
position=bb2013$포지션
head(position)
base2_pos=bb2013[,c(2,4:11)]
base2_pos2=aggregate(base2_pos[,2:9],by=list(포지션=base2_pos$포지션),sum)
head(base2_pos2)
rownames(base2_pos2)=base2_pos2[,1]
head(base2_pos2)
library(lattice)
parallel(~bb2013[,4:11]|position,horizontal.axis=F,col=1)
#parallel(base2_pos2[,2:9],horizontal.axis=F,col=1)

#팀별평행좌표
team=bb2013$팀
head(team)
parallel(~bb2013[,4:11]|team,horizontal.axis=F,col=1)

#3.
rownames(bb2013)=bb2013[,1]
rownames(bb2013)
head(bb2013)
model=prcomp(bb2013[,4:11],scale=T)
plot(model)
summary(model)
biplot(model)
