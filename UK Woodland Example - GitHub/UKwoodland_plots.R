install.packages("devtools")
devtools::install_github("kassambara/ggpubr")


###################################
# Plots
##################################

library(readr)
library(ggpubr)
library(reshape2)
library(ggplot2)
library(plotly)


results.list.PredSD <- list()
results.list.PredSD[[1]] <- read_csv("E:/UK Prox work/Results_Sim_1.csv", 
                                     col_names = FALSE )[,-1]+0

results.list.PredSD[[2]] <- read_csv("E:/UK Prox work/Results_Sim_2.csv", 
                                     col_names = FALSE)[,-1]+0
results.list.PredSD[[3]] <- read_csv("E:/UK Prox work/Results_Sim_3.csv", 
                                     col_names = FALSE)[,-1]+0


######### remove Aleatroic competent from each simulation ##############

#epsilonlist<-  read_csv("C:/UK Prox work/epsilonlist.csv", 
#col_names = TRUE)[,-1]

d<- as.data.frame(cbind(priority.score,
                        woodland.pred,
                        woodland.true))


pred.mean <- rep(NA,length(length.dump))
pred.mean[which(length.dump>0)]  <-  fitting.data.means

pred.lower <- rep(NA,length(length.dump))
pred.lower[which(length.dump>0)]  <-  fitting.data.lower

pred.upper <- rep(NA,length(length.dump))
pred.upper[which(length.dump>0)]  <-  fitting.data.upper



fig <- plot_ly(showscale = FALSE, showlegend = TRUE)
fig <- fig %>% add_surface( x=~seq(0,1,by=0.05),y=~seq(0,7,by=0.5), 
                            z = ~matrix(pred.mean,ncol=21), name='Predicted Woodland (Posterior Mode)',
                            colorscale = list(c(0, 1), c("black", "black")),
                            opacity = 0.4  )

fig <- fig %>% add_surface( x=~seq(0,1,by=0.05),y=~seq(0,7,by=0.5), 
                            z = ~matrix(pred.lower,ncol=21),name='Predicted Woodland (Credible Surface)',
                            colorscale = list(c(0, 1), c("black", "black")),
                            opacity = 0.2 )




fig <- fig %>% add_markers(x=d[,2], y=d[,1], z=d[,3],name="Reference Data" ,marker = list(size = 5,color="blue"))
fig <- fig %>% add_surface( x=~seq(0,1,by=0.05),y=~seq(0,7,by=0.5), 
                            z = ~matrix(pred.upper,ncol=21),name='',
                            colorscale = list(c(0, 1), c("black", "black")),
                            opacity = 0.2 )


fig <- fig %>% layout(
  title = "",
  scene = list(
    yaxis = list(title = "Propensity Score ",titlefont = list(size = 30), tickfont = list(size = 15),tickvals = 0:7, range = c(0, 7) ),
    xaxis = list(title = "Mapped Woodland",titlefont = list(size = 30),tickfont = list(size = 15), range = c(0, 1),tickvals = seq(0,1, by = 0.2)),
    zaxis = list(title = "Reference Woodland",titlefont = list(size = 30) ,tickfont = list(size = 15), range = c(0, 1), tickvals = seq(0,1, by = 0.1 ))
  ))

fig



#########################
# Surface plots for uncertainty measures
#########################


prediction.uncerts.vector <- rep(NA,length(length.dump))
prediction.uncerts.vector[which(length.dump>0)]  <-  as.numeric(apply(s.1,2,sd))  

prediction.uncerts <- matrix(prediction.uncerts.vector,ncol=21)


# Aleatroic Uncertainty SET UPS Aleatroic satndard deviation.

aleatoric.sd.vector <-c(quantile(sqrt(fitkm$sigsq.eps[draw.refs]),probs=c(0.025,0.975)),
                        mean(sqrt(fitkm$sigsq.eps[draw.refs])))

aleatoric.mean <- rep(NA,length(length.dump))
aleatoric.mean[which(length.dump>0)]  <-  aleatoric.sd.vector[3]

aleatoric.upper <- rep(NA,length(length.dump))
aleatoric.upper[which(length.dump>0)]  <-  aleatoric.sd.vector[2]

aleatoric.lower <- rep(NA,length(length.dump))
aleatoric.lower[which(length.dump>0)]  <-  aleatoric.sd.vector[1]


samp.1 <- rep(NA,length(length.dump))
samp.1[which(length.dump>0)]  <-  apply(results.list.PredSD[[1]],1,mean)

samp.2 <- rep(NA,length(length.dump))
samp.2[which(length.dump>0)]  <-  apply(results.list.PredSD[[2]],1,mean)

samp.3 <- rep(NA,length(length.dump))
samp.3[which(length.dump>0)]  <-  apply(results.list.PredSD[[3]],1,mean)


#############################################
# Aleatroic Only
#############################################



fig <- plot_ly(showscale = FALSE, showlegend = TRUE)


fig <- fig %>% add_surface( x=~seq(0,1,by=0.05),y=seq(0,7,by=0.5),
                            z = ~prediction.uncerts, name = 'Current',
                            colorscale = list(c(0, 1), c("black", "black")), opacity = 0.6 )

fig <- fig %>% add_surface( x=~seq(0,1,by=0.05),y=seq(0,7,by=0.5), 
                            z = ~matrix(aleatoric.mean,ncol=21),  name = 'Aleatoric (Posterior Mode)',
                            colorscale = list(c(0, 1), c("red", "red")),
                            opacity = 0.6 )

fig <- fig %>% add_surface( x=~seq(0,1,by=0.05),y=seq(0,7,by=0.5), 
                            z = ~matrix(aleatoric.upper,ncol=21), name ='Aleatoric (Credible Surface)',
                            colorscale = list(c(0, 1), c("red", "red")),
                            opacity = 0.3 )


fig <- fig %>% add_surface( x=~seq(0,1,by=0.05),y=seq(0,7,by=0.5), 
                            z = ~matrix(aleatoric.lower,ncol=21),colorscale = list(c(0, 1), c("red", "red")),
                            opacity = 0.3 )

fig <- fig %>% layout(
  title = "",
  scene = list(
    yaxis = list(title = "Propensity Score" ,titlefont = list(size = 30), tickfont = list(size = 15),tickvals = 0:7, range = c(0, 7)  ),
    xaxis = list(title = "Mapped Woodland",titlefont = list(size = 30),tickfont = list(size = 15), range = c(0, 1),tickvals = seq(0,1, by = 0.2)),
    zaxis = list(title = "Precision",titlefont = list(size = 30) ,tickfont = list(size = 15), range = c(0, 0.1), tickvals = seq(0,0.1, by = 0.01 )
    )))


fig


###########################################
# All Designs
###########################################


fig <- fig %>% add_surface( x=~seq(0,1,by=0.05),y=seq(0,7,by=0.5), 
                            z = ~matrix(samp.1,ncol=21), name = 'design 1 (Posterior Mode)',
                            colorscale = list(c(0, 1), c("blue", "blue"))
                            , opacity = 0.6 )

fig <- fig %>% add_surface( x=~seq(0,1,by=0.05),y=seq(0,7,by=0.5), 
                            z = ~matrix(samp.2,ncol=21),name = 'design 2 (Posterior Mode)',
                            colorscale = list(c(0, 1), c("green", "green"))
                            , opacity = 0.6 )


fig <- fig %>% add_surface( x=~seq(0,1,by=0.05),y=seq(0,7,by=0.5), 
                            z = ~matrix(samp.3,ncol=21), name = 'design 3 (Posterior Mode)',
                            colorscale = list(c(0, 1), c("yellow", "yellow"))
                            , opacity = 0.6 )


#fig <- fig %>% add_markers(x= fitting.data[,2], y= fitting.data[,1], z= as.numeric(apply(s.1,2,sd)),showscale = FALSE)

######## Layout clean up

fig <- fig %>% layout(
  title = "",
  scene = list(
    yaxis = list(title = "Propensity Score" ,titlefont = list(size = 30),tickfont = list(size = 15), tickvals = 0:7, range = c(0, 7)  ),
    xaxis = list(title = "Mapped Woodland",titlefont = list(size = 30),tickfont = list(size = 15), range = c(0, 1),tickvals = seq(0,1, by = 0.2)),
    zaxis = list(title = "Precision",titlefont = list(size = 30) ,tickfont = list(size = 15), range = c(0, 0.1), tickvals = seq(0,0.1, by = 0.01 )
    )))


fig




################################
# Comparing uncertainty in prediction plots
#################################

samp.2 <- rep(NA,length(length.dump))
samp.2.lower <- rep(NA,length(length.dump))
samp.2.upper <- rep(NA,length(length.dump))


samp.2[which(length.dump>0)]  <-  apply(results.list.PredSD[[2]],1,mean)

samp.2.lower[which(length.dump>0)]  <-  apply(results.list.PredSD[[2]],1,quantile, probs=c(0.025))

samp.2.upper[which(length.dump>0)]  <-  apply(results.list.PredSD[[2]],1,quantile, probs=c(0.975))

#######################################

samp.1 <- rep(NA,length(length.dump))
samp.1.lower <- rep(NA,length(length.dump))
samp.1.upper <- rep(NA,length(length.dump))

samp.1[which(length.dump>0)]  <-  apply(results.list.PredSD[[1]],1,mean)
samp.1.lower[which(length.dump>0)]  <-  apply(results.list.PredSD[[1]],1,quantile, probs=c(0.025))

samp.1.upper[which(length.dump>0)]  <-  apply(results.list.PredSD[[1]],1,quantile, probs=c(0.975))

#plots

fig <- plot_ly(showscale = FALSE , showlegend = TRUE )


fig <- fig %>% add_surface( x=~seq(0,1,by=0.05),y=seq(0,7,by=0.5),
                            z = ~prediction.uncerts,name = 'Current',
                            colorscale = list(c(0, 1), c("black", "black"))
                            , opacity = 0.7 )

fig <- fig %>% add_surface( x=~seq(0,1,by=0.05),y=seq(0,7,by=0.5), 
                            z = ~matrix(aleatoric.mean,ncol=21), name = 'Aleatoric (Posterior Mode)',
                            colorscale = list(c(0, 1), c("red", "red"))
                            , opacity = 0.7 )

fig <- fig %>% add_surface( x=~seq(0,1,by=0.05),y=seq(0,7,by=0.5), 
                            z = ~matrix(aleatoric.upper,ncol=21),name = 'Aleatoric (Credible Surface)',
                            colorscale = list(c(0, 1), c("red", "red"))
                            , opacity = 0.4 )
fig <- fig %>% add_surface( x=~seq(0,1,by=0.05),y=seq(0,7,by=0.5), 
                            z = ~matrix(samp.1,ncol=21),name = 'design 1 (Posterior Mode)',
                            colorscale = list(c(0, 1), c("blue", "blue"))
                            , opacity = 0.7 )

fig <- fig %>% add_surface( x=~seq(0,1,by=0.05),y=seq(0,7,by=0.5), 
                            z = ~matrix(samp.1.upper,ncol=21),name = 'design 1 (Credible Surface)',
                            colorscale = list(c(0, 1), c("blue", "blue"))
                            , opacity = 0.4 )


fig <- fig %>% add_surface( x=~seq(0,1,by=0.05),y=seq(0,7,by=0.5), 
                            z = ~matrix(aleatoric.lower,ncol=21),colorscale = list(c(0, 1), c("red", "red"))
                            , opacity = 0.4)

fig <- fig %>% add_surface( x=~seq(0,1,by=0.05),y=seq(0,7,by=0.5), 
                            z = ~matrix(samp.1.lower,ncol=21),colorscale = list(c(0, 1), c("blue", "blue"))
                            , opacity = 0.4 )


fig <- fig %>% layout(
  title = "",
  scene = list(
    yaxis = list(title = "Propensity Score" ,titlefont = list(size = 30), tickvals = 0:7, range = c(0, 7)  ),
    xaxis = list(title = "Mapped Woodland",titlefont = list(size = 30), range = c(0, 1),tickvals = seq(0,1, by = 0.2)),
    zaxis = list(title = "Precision",titlefont = list(size = 30) , range = c(0, 0.1), tickvals = seq(0,0.1, by = 0.01 )
    )))


fig



##############################################
# Epistemic uncertainty plots
###############################################

epSD.1.mean <- rep(NA,length(length.dump))
epSD.1.upper<- rep(NA,length(length.dump))
epSD.1.lower<- rep(NA,length(length.dump))


epVar.1 <- sweep((results.list[[1]]^2),2,fitkm$sigsq.eps[draw.refs[1:70]])
epSD.1.sub <- sqrt(abs(epVar.1))
epSD.1.sub[which(epVar.1 < 0)] <- (-1)*epSD.1.sub[which(epVar.1<0)]

epSD.1.mean[which(length.dump>0)]<- apply(epSD.1.sub,1, mean)
epSD.1.upper[which(length.dump>0)] <-  apply(epSD.1.sub,1,quantile, probs=c(0.975)) 
epSD.1.lower[which(length.dump>0)] <- apply(epSD.1.sub,1,quantile, probs=c(0.025)) 


#######

epSD.2.mean <- rep(NA,length(length.dump))
epSD.2.upper<- rep(NA,length(length.dump))
epSD.2.lower<- rep(NA,length(length.dump))


epVar.2 <- sweep((results.list[[2]]^2),2,fitkm$sigsq.eps[draw.refs[1:70]])
epSD.2.sub <- sqrt(abs(epVar.2))
epSD.2.sub[which(epVar.2 < 0)] <- (-1)*epSD.2.sub[which(epVar.2<0)]

epSD.2.mean[which(length.dump>0)]<- apply(epSD.2.sub,1, mean)
epSD.2.upper[which(length.dump>0)] <-  apply(epSD.2.sub,1,quantile, probs=c(0.975)) 
epSD.2.lower[which(length.dump>0)] <- apply(epSD.2.sub,1,quantile, probs=c(0.025)) 

epSD.3.mean <- rep(NA,length(length.dump))
epSD.3.upper<- rep(NA,length(length.dump))
epSD.3.lower<- rep(NA,length(length.dump))

########
epVar.3 <- sweep((results.list[[3]]^2),2,fitkm$sigsq.eps[draw.refs[1:70]])
epSD.3.sub <- sqrt(abs(epVar.3))
epSD.3.sub[which(epVar.3 < 0)] <- (-1)*epSD.3.sub[which(epVar.3<0)]

epSD.3.mean[which(length.dump>0)]<- apply(epSD.3.sub,1, mean)
epSD.3.upper[which(length.dump>0)] <-  apply(epSD.3.sub,1,quantile, probs=c(0.975)) 
epSD.3.lower[which(length.dump>0)] <- apply(epSD.3.sub,1,quantile, probs=c(0.025)) 

########### Surface plots for epitemic uncert




#####################################
# Bubble plots
######################################

#used to change sizes of markers

####### add uncert bounds for samples  ######################

samp.1 <- rep(NA,length(length.dump))
samp.1.lower <- rep(NA,length(length.dump))
samp.1.upper <- rep(NA,length(length.dump))

samp.1[which(length.dump>0)]  <-  apply(results.list.PredSD[[1]],1,mean)
samp.1.lower[which(length.dump>0)]  <-  apply(results.list.PredSD[[1]],1,quantile, probs=c(0.025))
samp.1.upper[which(length.dump>0)]  <-  apply(results.list.PredSD[[1]],1,quantile, probs=c(0.975))

samp.2 <- rep(NA,length(length.dump))
samp.2.lower <- rep(NA,length(length.dump))
samp.2.upper <- rep(NA,length(length.dump))

samp.2[which(length.dump>0)]  <-  apply(results.list.PredSD[[2]],1,mean)
samp.2.lower[which(length.dump>0)]  <-  apply(results.list.PredSD[[2]],1,quantile, probs=c(0.025))
samp.2.upper[which(length.dump>0)]  <-  apply(results.list.PredSD[[2]],1,quantile, probs=c(0.975))


samp.3 <- rep(NA,length(length.dump))
samp.3.lower <- rep(NA,length(length.dump))
samp.3.upper <- rep(NA,length(length.dump))

samp.3[which(length.dump>0)]  <-  apply(results.list.PredSD[[3]],1,mean)
samp.3.lower[which(length.dump>0)]  <-  apply(results.list.PredSD[[3]],1,quantile, probs=c(0.025))
samp.3.upper[which(length.dump>0)]  <-  apply(results.list.PredSD[[3]],1,quantile, probs=c(0.975))


############# Make plots ########################

fd<- as.data.frame(cbind(fitting.data,prediction.uncerts[which(length.dump>0)],
                         aleatoric.mean[which(length.dump>0)],
                         aleatoric.upper[which(length.dump>0)],
                         aleatoric.lower[which(length.dump>0)],
                         samp.1[which(length.dump>0)],
                         samp.1.upper[which(length.dump>0)],
                         samp.1.lower[which(length.dump>0)],
                         samp.2[which(length.dump>0)],
                         samp.2.upper[which(length.dump>0)],
                         samp.2.lower[which(length.dump>0)],
                         samp.3[which(length.dump>0)],
                         samp.3.upper[which(length.dump>0)],
                         samp.3.lower[which(length.dump>0)]
))

colnames(fd) <- c("x","y","score dump","Pred.Uncert","A.mean","A.upper","A.lower",
                  "s1.mean","s1.upper","s1.lower",
                  "s2.mean","s2.upper","s2.lower",
                  "s3.mean","s3.upper","s3.lower")

fd.filter <- fd[intersect( which( fd$y %in%  seq(0,1,by=0.1)) , which( fd$x %in% seq(0,7,by=1))), ]


fd.melt.0<- melt(fd.filter[,c(1,2,4,5)], id = c("x","y"))
fd.melt.1 <- melt(fd.filter[,c(1,2,4,5,8,11,14)], id = c("x","y"))
fd.melt.2 <- melt(fd.filter[,c(1,2,4,5,8)], id = c("x","y"))


fd.melt.a.upper <- melt(fd.filter[,c(1,2,6)], id = c("x","y"))
fd.melt.a.lower <- melt(fd.filter[,c(1,2,7)], id = c("x","y"))


fd.melt.s1.upper <- melt(fd.filter[,c(1,2,9)], id = c("x","y"))
fd.melt.s1.lower <- melt(fd.filter[,c(1,2,10)], id = c("x","y"))

######### Aleatroic only ######################################

ggplot() +
  geom_point(data=fd.melt.a.upper, aes(x=y,y=x, size= value),colour = "red", fill="red",shape=22, stroke = 0.1 , alpha=0.5 )+
  geom_point(data=fd.melt.a.lower, aes(x=y,y=x , colour = variable, size= value),colour = "red" , fill="white",shape=22, stroke = 0.1 )+
  geom_point(data=fd.melt.0, aes(x=y,y=x , colour = variable, size= value),fill=NA,shape=22, stroke = 1.5)+
  guides(color = guide_legend(override.aes = list(size = 10, fill = c('black','red'))))+
  scale_size_continuous(name= "Precision", breaks=seq(0.02,0.09, by = 0.01) ,range = c(10,40))+
  scale_color_manual(name='Design',
                     labels=c('Current', 'Aleatroic'),
                     values=c('Pred.Uncert'='black',
                              'A.mean'='red'))+
  scale_x_continuous(name= "Mapped Woodland" ,breaks=seq(0,1,by=0.1))+
  scale_y_continuous(name= "Propensity Score", limits=c(-0.1,6.5) , breaks=seq(0,7,0.5))+
  theme(legend.direction = "vertical", legend.box = "horizontal",
        panel.background = element_rect(fill = 'white', color = 'black'),
        panel.grid.major = element_line(color = 'black', linetype = 'dotted'),
        panel.grid.minor = element_line(color = 'black', linetype = 'dotted'),legend.key.height= unit(2, 'cm'),
        legend.key.width= unit(4, 'cm'),
        axis.title=element_text(size=30))




########### Full ###############################
ggplot() +
  geom_point(data=fd.melt.a.upper, aes(x=y,y=x, size= value),colour = "red", fill="red",shape=22, stroke = 0.1 , alpha=0.5 )+
  geom_point(data=fd.melt.a.lower, aes(x=y,y=x , colour = variable, size= value),colour = "red" , fill="white",shape=22, stroke = 0.1 )+
  geom_point(data=fd.melt.1, aes(x=y,y=x , colour = variable, size= value),fill=NA,shape=22, stroke = 1.5)+
  guides(color = guide_legend(override.aes = list(size = 10, fill = c('black','red','blue','green','yellow'))))+
  scale_size_continuous(name= "Precision", breaks=seq(0.02,0.09, by = 0.01) ,range = c(10,40))+
  scale_color_manual(name='Design',
                     labels=c('Current', 'Aleatroic', 'design 1','design 2', 'design 3'),
                     values=c('Pred.Uncert'='black',
                              'A.mean'='red',
                              's1.mean' = 'blue', 's2.mean' = 'green', 's3.mean' = 'yellow'))+
  scale_x_continuous(name= "Mapped Woodland" ,breaks=seq(0,1,by=0.1))+
  scale_y_continuous(name= "Propensity Score", limits=c(-0.1,6.5) , breaks=seq(0,7,0.5))+
  theme(legend.direction = "vertical", legend.box = "horizontal",
        panel.background = element_rect(fill = 'white', color = 'black'),
        panel.grid.major = element_line(color = 'black', linetype = 'dotted'),
        panel.grid.minor = element_line(color = 'black', linetype = 'dotted'),legend.key.height= unit(2, 'cm'),
        legend.key.width= unit(4, 'cm'),
        axis.title=element_text(size=30))


##### Sample 1 Uncerts ####################################

ggplot() +
  
  geom_point(data=fd.melt.s1.upper, aes(x=y,y=x, size= value),colour = "blue", fill="blue",shape=22, stroke = 0.1 , alpha=0.5 )+
  geom_point(data=fd.melt.s1.lower, aes(x=y,y=x , colour = variable, size= value),colour = "blue" , fill="white",shape=22, stroke = 0.1 )+
  
  
  geom_point(data=fd.melt.a.upper, aes(x=y,y=x, size= value),colour = "red", fill="red",shape=22, stroke = 0.1 , alpha=0.5 )+
  geom_point(data=fd.melt.a.lower, aes(x=y,y=x , colour = variable, size= value),colour = "red" , fill="white",shape=22, stroke = 0.1 )+
  
  
  geom_point(data=fd.melt.2, aes(x=y,y=x , colour = variable, size= value),fill=NA,shape=22, stroke = 1.5)+
  guides(color = guide_legend(override.aes = list(size = 10, fill = c('black','red','blue'))))+
  scale_size_continuous(name= "Precision", breaks=seq(0.02,0.09, by = 0.01) ,range = c(10,40))+
  scale_color_manual(name='Design',
                     labels=c('Current', 'Aleatroic', 'design 1'),
                     values=c('Pred.Uncert'='black',
                              'A.mean'='red',
                              's1.mean' = 'blue'))+
  scale_x_continuous(name= "Mapped Woodland" ,breaks=seq(0,1,by=0.1))+
  scale_y_continuous(name= "Propensity Score", limits=c(-0.1,6.5) , breaks=seq(0,7,0.5))+
  theme(legend.direction = "vertical", legend.box = "horizontal",
        panel.background = element_rect(fill = 'white', color = 'black'),
        panel.grid.major = element_line(color = 'black', linetype = 'dotted'),
        panel.grid.minor = element_line(color = 'black', linetype = 'dotted'),legend.key.height= unit(2, 'cm'),
        legend.key.width= unit(4, 'cm'),
        axis.title=element_text(size=30))






###################################################
# Heat Maps
###################################################

#### Set up ######################

data.2d <- as.data.frame(cbind(rep(NA,length(samp.1)), expand.grid(y=seq(0,7,by=0.5),x=seq(0,1,by=0.05)) , prediction.uncerts.vector , aleatoric.mean,
                               samp.1,samp.2,samp.3))

colnames(data.2d)[1] <- "v1"


####  SUbplots #################


fig2d.1 <- ggplot(data= data.2d, aes(x = x, y = y))+
  geom_tile(color = "black") +
  geom_point( data= d ,aes(woodland.true, priority.score ),colour = 5, size = 3, shape=19)+
  scale_x_continuous(name= "Mapped Woodland", breaks=seq(0,1,by=0.1))+
  scale_y_continuous(name= "Propensity Score", breaks=seq(0,7,0.5))+
  geom_rect( inherit.aes = FALSE, aes(xmin=0, xmax=1, ymin=4, ymax=7, colour = "design 1"), fill = NA, size = 2 )+
  geom_rect( inherit.aes = FALSE, aes(xmin=0, xmax=1, ymin = 1.8, ymax = 2.2, colour = "design 2"), fill = NA, size = 3 )+
  geom_rect( inherit.aes = FALSE, aes(xmin=0.5, xmax=1, ymin = 1.8, ymax = 2.2, colour = "design 3"), fill = NA, size = 1.5 )+
  scale_color_manual(name='Design Area',
                     breaks=c('design 1', 'design 2', 'design 3'),
                     values=c('design 1'='blue', 'design 2'='green', 'design 3'='yellow'))+
  annotate("text", x = 0.55, y=6, label = "n = 120", colour ="blue", size = 10)+
  annotate("text", x = 0.5, y=3, label = "n = 20", colour ="green", size = 10)+
  annotate("text", x = 0.75, y=1, label = "n = 20", colour ="yellow", size = 10)

#fig2d.1

######################### Current Precision #############################################

fig2d.2 <- ggplot(data= data.2d, aes(x = x, y = y, fill = prediction.uncerts.vector))+
  geom_tile(color = "black") +
  scale_fill_gradientn(name= "Precision", colors = (hcl.colors(20, "Plasma")),limits=range(0.020,0.09), 
                       breaks = seq(0.020,0.09, 0.01), labels = seq(0.020,0.09, 0.01)
  )+
  guides(fill = guide_colourbar(barwidth = 1,
                                barheight = 20))+
  geom_point( data= d ,aes(woodland.true, priority.score, fill= woodland.true ),colour = 5, size = 3, shape=19)+
  scale_x_continuous(name= "Mapped Woodland", breaks=seq(0,1,by=0.1))+
  scale_y_continuous(name= "Propensity Score", breaks=seq(0,7,0.5))+
  theme ( legend.position="none")
#


#fig2d.2


############## Aleatroic Uncert ###################################################

fig2d.3 <- ggplot(data= data.2d, aes(x = x, y = y, fill = aleatoric.mean))+
  geom_tile(color = "black") +
  scale_fill_gradientn(name= "Precision", colors = (hcl.colors(20, "Plasma")),limits=range(0.020,0.09), 
                       breaks = seq(0.020,0.09, 0.01), labels = seq(0.020,0.09, 0.01)
  )+
  guides(fill = guide_colourbar(barwidth = 1,
                                barheight = 20))+
  geom_point( data= d ,aes(woodland.true, priority.score, fill= woodland.true ),colour = 5, size = 3, shape=19)+
  scale_x_continuous(name= "Mapped Woodland", breaks=seq(0,1,by=0.1))+
  scale_y_continuous(name= "Propensity Score", breaks=seq(0,7,0.5))

#


#fig2d.3

############## Sample 1  ###################################################

fig2d.4 <- ggplot(data= data.2d, aes(x = x, y = y, fill = samp.1))+
  geom_tile(color = "black") +
  scale_fill_gradientn(name= "Precision \n (Posterior Standard deviation)", colors = (hcl.colors(20, "Plasma")),limits=range(0.020,0.09), 
                       breaks = seq(0.020,0.09, 0.01), labels = seq(0.020,0.09, 0.01)
  )+
  guides(fill = guide_colourbar(barwidth = 1,
                                barheight = 20))+
  geom_point( data= d ,aes(woodland.true, priority.score, fill= woodland.true ),colour = 5, size = 3, shape=19)+
  scale_x_continuous(name= "Mapped Woodland", breaks=seq(0,1,by=0.1))+
  scale_y_continuous(name= "Propensity Score", breaks=seq(0,7,0.5))+
  theme ( legend.position="none")+ 
  geom_rect( inherit.aes = FALSE, aes(xmin=0, xmax=1, ymin=4, ymax=7), colour = "blue", fill = NA, size = 1 ) 
#


#fig2d.4

############## Sample 2  ###################################################

fig2d.5 <- ggplot(data= data.2d, aes(x = x, y = y, fill = samp.2))+
  geom_tile(color = "black") +
  scale_fill_gradientn(name= "Precision \n (Posterior Standard deviation)", colors = (hcl.colors(20, "Plasma")),limits=range(0.020,0.09), 
                       breaks = seq(0.020,0.09, 0.01), labels = seq(0.020,0.09, 0.01)
  )+
  guides(fill = guide_colourbar(barwidth = 1,
                                barheight = 20))+
  geom_point( data= d ,aes(woodland.true, priority.score, fill= woodland.true ),colour = 5, size = 3, shape=19)+
  scale_x_continuous(name= "Mapped Woodland", breaks=seq(0,1,by=0.1))+
  scale_y_continuous(name= "Propensity Score", breaks=seq(0,7,0.5))+
  theme ( legend.position="none")+ 
  geom_rect( inherit.aes = FALSE, aes(xmin=0, xmax=1, ymin=1.8, ymax=2.2), colour = "green", fill = NA, size = 1 ) 
#

#fig2d.5

############## Sample 3  ###################################################

fig2d.6 <- ggplot(data= data.2d, aes(x = x, y = y, fill = samp.3))+
  geom_tile(color = "black") +
  scale_fill_gradientn(name= "Precision", colors = (hcl.colors(20, "Plasma")),limits=range(0.020,0.09), 
                       breaks = seq(0.020,0.09, 0.01), labels = seq(0.020,0.09, 0.01)
  )+
  guides(fill = guide_colourbar(barwidth = 1,
                                barheight = 20))+
  geom_point( data= d ,aes(woodland.true, priority.score, fill= woodland.true ),colour = 5, size = 3, shape=19)+
  scale_x_continuous(name= "Mapped Woodland", breaks=seq(0,1,by=0.1))+
  scale_y_continuous(name= "Propensity Score", breaks=seq(0,7,0.5))+
  theme ( legend.position="none")+ 
  geom_rect( inherit.aes = FALSE, aes(xmin=0.5, xmax=1, ymin=1.8, ymax=2.2), colour = "yellow", fill = NA, size = 1 )


#fig2d.6


########## Combine Plots #################################


############# 1st wave
ggarrange( fig2d.2, fig2d.3,
           labels = c("Current", "Aleatroic"),
           ncol = 2, nrow = 1)


########### 2nd wave full

ggarrange( fig2d.2, fig2d.3, fig2d.1, fig2d.4, fig2d.5, fig2d.6,
           labels = c("Current", "Aleatroic", "Design Areas" ,"design 1" ,"design 2", "Desgin 3"),
           ncol = 3, nrow = 2)

