library("ggplot2")
x<- c(1:50)
y<- c(1:50)
i<-0
for (val in x){
  r <- runif(1,-5,15)
  r2 <- runif(1,-5,15)
  y[i]<- x[i]+r
  x[i]<- x[i]+r2
  i<-i+1
}

df <- data.frame(x,y)

x2<- runif(50,0,50)
y2<-runif(50,0,60)
df2 <- data.frame(x2,y2)

#p <- 
ggplot(df, aes(x,y))+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_point()+
  xlab("oral dose (mg)")+
  ylab("blood concentration (ng/ml)")

#p2 <- 
ggplot(df2, aes(x2,y2))+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_point()+
  xlab("average driving speed (mph)")+
  ylab("# of Sienfeld episodes seen")

count<-0
xbar <- mean(x)
ybar <- mean(y)
x2bar <- mean(x2)
y2bar <- mean(y2)

top <- c(1:50)
bottom <- c(1:50)
top2 <- c(1:50)
bottom2 <- c(1:50)

for (val in x){
  top[count]<-(y[count]-ybar)*(x[count]-xbar)
  bottom[count]<-((x[count]-xbar)^2)
  count<- count+1
}

count<-0
for (val in x2){
  top2[count]<-(y2[count]-y2bar)*(x2[count]-x2bar)
  bottom2[count]<-((x2[count]-x2bar)^2)
  count<- count+1
}

m <- sum(top)/sum(bottom)
m2 <- sum(top2)/sum(bottom2)

b <- ybar - (m*xbar)
b2 <- y2bar - (m2*x2bar)

xlm <- c(1:60)
ylm <- c(1:60)
xlm2 <- c(1:60)
ylm2 <- c(1:60)

count<-1
for (val in xlm){
  ylm[count]<-xlm[count]*m + b
  count<-count+1
}

count<-1
for (val in xlm2){
  ylm2[count]<-xlm2[count]*m2 + b2
  count<-count+1
}

dflm<-data.frame(xlm,ylm)
dflm2<-data.frame(xlm2,ylm2)

ggplot(df, aes(x,y))+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_point()+
  geom_line(data = dflm, aes(xlm,ylm), colour="red")+
  xlab("oral dose (mg)")+
  ylab("blood concentration (ng/ml)")

ggplot(df2, aes(x2,y2))+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_point()+
  geom_line(data = dflm2, aes(xlm2,ylm2), colour="red")+
  xlab("average driving speed (mph)")+
  ylab("# of Sienfeld episodes seen")

se <- c(1:50)

count<-1
for (val in y){
  se[count]<-(y[count]-(m*x[count]+b))^2
  count<-count+1
}

se2<-c(1:50)

count<-1
for (val in y2){
  se2[count]<-(y2[count]-(m2*x2[count]+b2))^2
  count<-count+1
}

sse<- sum(se)
sse2<- sum(se2)

mse<- sse/length(se)
mse2<- sse2/length(se2)

rmse<- sqrt(mse)
rmse2<- sqrt(mse2)

lin_model<-lm(y~x)
summary(lin_model)

lin_model2<-lm(y2~x2)
summary(lin_model2)
#bottom