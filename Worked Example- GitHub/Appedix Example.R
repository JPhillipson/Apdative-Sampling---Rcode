
##########################
# Example Workflow
#############################
library(ggplot2)
library(bayesGAM)

set.seed(1234)
total.size <- 5e4
sample.size<- 3e1
working.range <- seq(-10,10,by=0.1)

x<- rnorm(n=total.size,mean=0,sd=3)
y<- tanh(x)+ rnorm(n=total.size, mean=0, sd=0.1)

random.selection <- sample(x=total.size, size=sample.size)
d1 <- data.frame(x[c(random.selection)],y[c(random.selection)])
colnames(d1) <- c("input","output")

plot.base <- ggplot()+
  geom_point(data= d1, aes(input, output),size=3)+
  scale_x_continuous( breaks=seq(-10,10,by=1),limits = c(-10,10), name ="X")+
  scale_y_continuous(breaks=seq(-1.5,3,by=0.5),limits = c(-1.5,3), name= "Y")
plot(plot.base)

f <- bayesGAM(output ~ np(input), data = d1,
              family = gaussian, iter=5000, spcontrol = list(qr = TRUE))

yy<- predict(f, newdata= data.frame(input=working.range),draws=5e3) 

d2 <- data.frame(working.range,apply(yy@pp$ypred,2,mean),apply(yy@pp$ypred,2,quantile, probs=c(0.975) ),apply(yy@pp$ypred,2,quantile, probs=c(0.025) ))
colnames(d2)<- c("x","mean","upper","lower")


plot.fit <- ggplot(data=d2,aes(x, mean))+
  scale_x_continuous( breaks=seq(-10,10,by=1),limits = c(-10,10), name ="X")+
  scale_y_continuous(breaks=seq(-1.5,3,by=0.5),limits = c(-1.5,3), name= "Y")+
  geom_line(aes(y= mean), size=1)+
  geom_line(aes(y= upper), col="grey",size=1)+
  geom_line(aes(y= lower), col="grey", size=1)+
  geom_point(data= d1, aes(input, output),size=3)

plot(plot.fit)
ale.bounds <- as.numeric(summary(f)[2,c(1,4,8)])

n.samples.UF <- 50
future.sample.sizes <- 30


probs.list<- list()
probs.list[[1]] <-  dnorm(working.range,mean=-9.5,sd=1) + dnorm(working.range,mean=9.5,sd=1)
probs.list[[2]] <-  dnorm(working.range,mean=0,sd=3)

###################################################y

set.seed(764333)
results.list<- list()
start.time.1 <- Sys.time()

for(i in 1:length(probs.list)){
  results.list.sub.matrix <- results.list.sub.matrix <- matrix(rep(0,n.samples.UF*length(working.range)),ncol=n.samples.UF)
  
  
  for(m in 1:n.samples.UF ){
    
    sample.hyp <- sample(length(working.range), future.sample.sizes, replace = FALSE,
                         prob = probs.list[[i]])    
    
    d.sub <- data.frame(working.range[sample.hyp], yy@pp$ypred[m,sample.hyp] )
    colnames(d.sub) <- c("input","output")
    
    
    d.hyp <- rbind(d,d.sub)
    f.hyp <- bayesGAM(output ~ np(input), data = d.hyp,
                      family = gaussian, iter=5000, spcontrol = list(qr = TRUE))
    
    yy.hyp <- predict(f.hyp, newdata= data.frame(input=working.range),draws=5e3) 
    results.list.sub.matrix[,m] <- apply(yy.hyp@pp$ypred,2,sd) 
    
    
  }
  results.list[[i]] <- results.list.sub.matrix
  #paste("design type",i,":","sim",m,":")
}

end.time <- Sys.time()
end.time-start.time.1


results.data <- data.frame( working.range,apply(yy@pp$ypred,2,sd ),
                            rep(ale.bounds[1],length(working.range)),
                            rep(ale.bounds[2],length(working.range)),
                            rep(ale.bounds[3],length(working.range)),
                            apply(results.list[[1]],1,mean),
                            apply(results.list[[1]],1,quantile, probs=c(0.025)),
                            apply(results.list[[1]],1,quantile, probs=c(0.975)),
                            apply(results.list[[2]],1,mean),
                            apply(results.list[[2]],1,quantile, probs=c(0.025)),
                            apply(results.list[[2]],1,quantile, probs=c(0.975)),
                            probs.list[[1]],
                            probs.list[[2]]
                            
)
colnames(results.data) <- c( "x", "current", 
                             "ale.mean", "ale.lower", "ale.upper",
                             "samp.1.mean", "samp.1.lower", "samp.1.upper",
                             "samp.2.mean", "samp.2.lower", "samp.2.upper",
                             "target.1", "target.2")                           
line.thickness <- 1

ale.plot.0 <-  ggplot(data= results.data, aes(x, current))+
  geom_line(aes(y= current ,col="Predictive posterior (SD)"), size=line.thickness)+
  geom_ribbon(aes(ymin=ale.lower,ymax=ale.upper,x=x), fill="red",alpha=0.1)+
  scale_x_continuous( breaks=seq(-10,10,by=1),limits = c(-10,10))+
  scale_y_continuous(breaks=seq(0,1,by=0.1),limits = c(0,1), name= "Standard deviation / Density")+
  geom_line(aes(y=ale.mean,col="Aleatoric (SD)"), size=line.thickness)+
  geom_line(aes(y=target.1,col="Targed design (Distribution)"), size=line.thickness, linetype= "dashed" )+
  geom_line(aes(y=target.2,col="Random sample (Distribution)"), size=line.thickness, linetype= "dashed")+
  scale_color_manual( name="",
                      values = c("Predictive posterior (SD)" = "black", "Aleatoric (SD)" = "red",
                                 "Targed design (Distribution)"= "blue", "Random sample (Distribution)"= "green"))+
  labs(x ="X", y = "Standard Deviation")+
  theme(legend.direction = "vertical")
plot(ale.plot.0)


ale.plot.1 <-  ggplot(data= results.data, aes(x, current))+
  geom_line(aes(y= current ,col="Predictive posterior"), size=line.thickness)+
  geom_ribbon(aes(ymin=ale.lower,ymax=ale.upper,x=x), fill="red",alpha=0.1)+
  scale_x_continuous( breaks=seq(-10,10,by=1),limits = c(-10,10))+
  scale_y_continuous(breaks=seq(0,1,by=0.1),limits = c(0,1))+
  geom_line(aes(y=ale.mean,col="Aleatoric"), size=line.thickness)+
  geom_line(aes(y=samp.1.mean,col="Targed design"), size=line.thickness)+
  geom_line(aes(y=samp.2.mean,col="Random sample"), size=line.thickness)+
  
  
  geom_ribbon(aes(ymin=samp.2.lower,ymax=samp.2.upper,x=x), fill="green",alpha=0.1)+
  geom_ribbon(aes(ymin=samp.1.lower,ymax=samp.1.upper,x=x), fill="blue",alpha=0.1)+
  
  scale_color_manual( name="",
                      values = c("Predictive posterior" = "black", "Aleatoric" = "red",
                                 "Targed design"= "blue", "Random sample"= "green"))+
  labs(x ="X", y = "Standard Deviation")+
  theme(legend.direction = "vertical")
plot(ale.plot.1)



############################################
# fit under new targeted design
############################################

set.seed(8888)

target.selection <- sample(x=total.size, size=sample.size,
                           prob= dnorm(x,mean=-9.5,sd=1) + dnorm(x,mean=9.5,sd=1))


d3 <-  data.frame(x[c(random.selection,target.selection)],
                  y[c(random.selection,target.selection)])
colnames(d3) <- c("input","output")


plot.newsamp <- ggplot()+
  geom_point(data= d3, aes(input, output),size=3, col="blue")+
  geom_point(data= d1, aes(input, output),size=3, col="black")+
  scale_x_continuous( breaks=seq(-10,10,by=1),limits = c(-10,10), name ="X")+
  scale_y_continuous(breaks=seq(-1.5,3,by=0.5),limits = c(-1.5,3), name= "Y")
plot(plot.newsamp)



f2 <- bayesGAM(output ~ np(input), data = d3,
               family = gaussian, iter=5000, spcontrol = list(qr = TRUE))

yy2<- predict(f2, newdata= data.frame(input=working.range),draws=5e3) 

d4 <- data.frame(working.range,apply(yy2@pp$ypred,2,mean),apply(yy2@pp$ypred,2,quantile, probs=c(0.975) ),apply(yy2@pp$ypred,2,quantile, probs=c(0.025) ))
colnames(d4)<- c("x","mean","upper","lower")


plot.fit <- ggplot(data=d4,aes(x, mean))+
  scale_x_continuous( breaks=seq(-10,10,by=1),limits = c(-10,10), name ="X")+
  scale_y_continuous(breaks=seq(-1.5,3,by=0.5),limits = c(-1.5,3), name= "Y")+
  geom_line(aes(y= mean), size=1)+
  geom_line(aes(y= upper), col="grey",size=1)+
  geom_line(aes(y= lower), col="grey", size=1)+
  geom_point(data= d3, aes(input, output),size=3, col="blue")+
  geom_point(data= d1, aes(input, output),size=3)

plot(plot.fit)






