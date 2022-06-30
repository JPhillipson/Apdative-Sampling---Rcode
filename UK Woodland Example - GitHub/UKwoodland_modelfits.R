

library(raster)
library(sp)
library(rgdal)
library(readr)
library(plotly)
library(utils)
library(bkmr)
library(plotly)
library(foreign)
library(sf)
library(maptools)

##############################
# Read Data UK_Proxy_Full 
##############################

# Set your folder location here
setwd("E:/UK Prox work")

#############################################
# Load Data
#############################################

Input_Data <- read.csv("Input_Data.csv")
Validation_woodland_ENG <- read.csv("Validation_woodland_ENG.csv")

colnames(Input_Data)[1] <- "ID"
colnames(Input_Data)[5:25] <-c("Broadleaved woodland","Coniferous Woodland",
                               "Arable and Horticulture","Improved Grassland", "Neutral Grassland",
                               "Calcareous Grassland","Acid Grassland","Fen,Marsh and Swamp",
                               "Heather","Heather grassland","Bog","Inland Rock","Saltwater","Freshwater",
                               "Supra-littoral Rock" ,"Supra-littoral sediment","Littoral Rock",
                               "Littoral sediment","Saltmarsh","Urban","Suburban") 

Input_Data[5:25]<- Input_Data[5:25]/100
Input_Data$woodland <- Input_Data$`Broadleaved woodland` + Input_Data$`Coniferous Woodland`

set.seed(1111)

# Orginal Sample size
n.samps <- 30

scores.edit <- as.numeric(Validation_woodland_ENG$ProxScore > 4)

validation.sample.selection<- sample(nrow(Validation_woodland_ENG),size=n.samps,replace=FALSE,
                                     prob = scores.edit/sum(scores.edit))

woodland.true <-  Validation_woodland_ENG$Ref[validation.sample.selection]
woodland.pred <-  Validation_woodland_ENG$Pred[validation.sample.selection]

priority.score <- Validation_woodland_ENG$ProxScore[validation.sample.selection]



plot(woodland.pred,woodland.true,pch=19,xlim=c(0,1),ylim=c(0,1),col="blue",
     xlab=expression("Mapped Woodland"~" ( Km"^"2"~")"), ylab=expression("Reference Woodland"~" ( Km"^"2"~")") )
axis(1,at= seq(0,1, by=0.1))


plot(Validation_woodland_ENG$Pred,Validation_woodland_ENG$Ref,
     pch=19,xlim=c(0,1),ylim=c(0,1))

d<- as.data.frame(cbind(priority.score,
                        woodland.pred,
                        woodland.true))


fig <- plot_ly(x=d[,1], y=d[,2], z=d[,3], type="scatter3d", mode="markers",marker = list(size = 5,color="red"))
fig <- fig %>% layout(
  title = "UK woodland",
  scene = list(
    xaxis = list(title = "Cost Score"),
    yaxis = list(title = "Predicted woodland"),
    zaxis = list(title = "Reference")
  ))
axx <- list(
  nticks = 8,
  range = c(0,8)
)
axy <- list(
  nticks = 10,
  range = c(0,1)
)
axz <- list(
  nticks = 10,
  range = c(0,1)
)

fig <- fig %>% layout(scene = list(xaxis=axx,yaxis=axy,zaxis=axz))
fig

############################################
# FITTING THE MODELS FOR WOODLAND 
############################################




# fit Kernel Model
set.seed(12567826)
fitkm <- kmbayes(y = woodland.true, Z = cbind(priority.score,woodland.pred),
                 X = cbind(rep(1,length(woodland.true)), priority.score, woodland.pred),
                 verbose = FALSE, varsel = TRUE, iter = 1e4)

#Znew<- cbind(Input_Data$Proximal_Priority_Score,Input_Data$woodland)
#Xnew <- cbind(rep(1,nrow(Input_Data)),Input_Data$woodland)
#s<- SamplePred(fitkm, Znew = Znew, Xnew = Xnew,sel=NULL)


##############################################
# fitting data dumps
#############################################

fitting.data <- expand.grid(x= seq(0,7,by=0.5),
                            y= seq(0,1,by=0.05))
fitting.data[,3] <- rep(1,nrow(fitting.data))
fitting.data <- as.matrix(fitting.data)
length.dump<- rep(0,nrow(fitting.data))
pixel.refs.list <- list()

counter <- 1
for(i in 1:nrow(fitting.data)){
  
  cost.lower <- fitting.data[i,1] -0.25 
  cost.upper <- fitting.data[i,1] +0.25 
  
  pred.woodland.lower <-  fitting.data[i,2] -0.025
  pred.woodland.upper <- fitting.data[i,2] +0.025
  pixel.refs <- which(Input_Data$Proximal_Priority_Score > cost.lower & 
                        Input_Data$Proximal_Priority_Score <= cost.upper &
                        Input_Data$woodland >pred.woodland.lower &
                        Input_Data$woodland <= pred.woodland.upper
                      
  )
  
  length.dump[i] <- length(pixel.refs)
  if(length(pixel.refs)>0){
    pixel.refs.list[[counter]] <- pixel.refs
    counter<- counter +1
  }
}

fitting.data <- fitting.data[which(length.dump >0),]

###########################################################

fitting.data.means <- rep(0,nrow(fitting.data))
fitting.data.sds <- rep(0,nrow(fitting.data))
fitting.data.lower <- rep(0,nrow(fitting.data))
fitting.data.upper <- rep(0,nrow(fitting.data))

draw.refs <- seq(5e3,1e4,by=10)

start.time <- Sys.time()
s<- SamplePred(fitkm, Znew = fitting.data[,c(1,2)], Xnew = fitting.data[,c(3,1,2)],sel=draw.refs)
end.time <- Sys.time()
end.time- start.time

s.1 <- s
for(j in 1:ncol(s)){
  s.1[,j] <- s[,j] + rnorm(n=length(draw.refs), mean=0,sd= sqrt(fitkm$sigsq.eps[draw.refs]))
}

fitting.data.means <- apply(s.1,2,mean )
fitting.data.sds <- apply(s.1,2,sd )
fitting.data.lower <-  fitting.data.means - (fitting.data.sds*qnorm(0.975,0,1))
fitting.data.upper <-  fitting.data.means + (fitting.data.sds*qnorm(0.975,0,1))

woodland.pred.mean <- rep(0,length(Input_Data$woodland))
woodland.pred.ssd <- rep(0,length(Input_Data$woodland))
woodland.pred.s.1.sd <- rep(0,length(Input_Data$woodland))

for(i in 1:nrow(fitting.data)){
  
  cost.lower <- fitting.data[i,1] -0.25 
  cost.upper <- fitting.data[i,1] +0.25 
  
  pred.woodland.lower <-  fitting.data[i,2] -0.025
  pred.woodland.upper <- fitting.data[i,2] +0.025
  pixel.refs <- which(Input_Data$Proximal_Priority_Score > cost.lower & 
                        Input_Data$Proximal_Priority_Score <= cost.upper &
                        Input_Data$woodland >pred.woodland.lower &
                        Input_Data$woodland <= pred.woodland.upper
  )
  
  woodland.pred.mean[pixel.refs] <- mean(s[,i]) 
  woodland.pred.ssd[pixel.refs] <- sd(s[,i])
  woodland.pred.s.1.sd[pixel.refs] <- sd(s.1[,i])
}

Input_Data[,27]<- woodland.pred.mean
Input_Data[,28]<- woodland.pred.ssd
Input_Data[,29]<- woodland.pred.s.1.sd
Input_Data[,30]<- as.numeric(Input_Data$Proximal_Priority_Score> 4) +  
  3*as.numeric(Input_Data$Proximal_Priority_Score > 1.8 & Input_Data$Proximal_Priority_Score < 2.2)+
  7*as.numeric(Input_Data$woodland >=0.5 )

Input_Data[,31]<- as.numeric(Input_Data$Proximal_Priority_Score > 4)

Input_Data[,32]<- as.numeric(Input_Data$Proximal_Priority_Score > 1.8 & Input_Data$Proximal_Priority_Score < 2.2)

Input_Data[,33]<-as.numeric(Input_Data$Proximal_Priority_Score > 1.8 & Input_Data$Proximal_Priority_Score < 2.2 & 
                              Input_Data$woodland>=0.5)


colnames(Input_Data)[27:33] <- c("wood.Pred", "wood.SD.Ep","Wood.SD.Full","StrataRefs","Samp1","Samp2","Samp3")

d <- data.frame(Input_Data)
rownames(d) <-names(SpP)
SPDF <- SpatialPolygonsDataFrame(SpP, data=  d)
writeOGR(obj=SPDF, dsn="UKShapefiles", layer="GridLines_2", driver="ESRI Shapefile",overwrite_layer = TRUE)



###############################################
# 2nd wave fittings
###############################################

n.samples.UF <- 190
future.sample.sizes <- c(120,20,20)
###################################################
sample.wts <- list()
fig.list <- list()

sample.wts[[1]] <- as.numeric(Input_Data$Proximal_Priority_Score > 4)
sample.wts[[2]] <- as.numeric(Input_Data$Proximal_Priority_Score > 1.8 & Input_Data$Proximal_Priority_Score < 2.2)

# find ratio infaltion for woodland targeting
Sample.Ratio <- sum(as.numeric(Input_Data$Proximal_Priority_Score > 1.8 & Input_Data$Proximal_Priority_Score < 2.2))/
  sum(as.numeric(Input_Data$Proximal_Priority_Score > 1.8 & Input_Data$Proximal_Priority_Score < 2.2 & 
                   Input_Data$woodland>0.5))

#
sample.wts[[3]] <- sample.wts[[2]] + Sample.Ratio*as.numeric(Input_Data$Proximal_Priority_Score > 1.8 & Input_Data$Proximal_Priority_Score < 2.2 & 
                                                               Input_Data$woodland>=0.5)

set.seed(764333)
results.list<- list()
start.time.1 <- Sys.time()


for(i in 1:length(future.sample.sizes)){
  
  sample.selection.2 <- sample(x=nrow(Input_Data),size=future.sample.sizes[i], prob = sample.wts[[i]]/sum(sample.wts[[i]]),replace=FALSE)
  #########################################
  # future samples predictions
  ##########################################
  
  results.list.sub.matrix <- matrix(rep(0,n.samples.UF*nrow(fitting.data)),ncol=n.samples.UF)
  for(m in 1:n.samples.UF ){
    woodland.hyper.means <- s[m,]
    woodland.hyperthet <- rep(0,length(Input_Data$woodland))
    
    # create artiifcal samples using s (kmfit) 
    for(h in 1:nrow(fitting.data)){
      
      cost.lower <- fitting.data[h,1] -0.25 
      cost.upper <- fitting.data[h,1] +0.25 
      
      pred.woodland.lower <-  fitting.data[h,2] -0.025
      pred.woodland.upper <- fitting.data[h,2] +0.025
      pixel.refs <- which(Input_Data$Proximal_Priority_Score > cost.lower & 
                            Input_Data$Proximal_Priority_Score <= cost.upper &
                            Input_Data$woodland >pred.woodland.lower &
                            Input_Data$woodland <= pred.woodland.upper
      )
      
      woodland.hyperthet[pixel.refs] <- rnorm( n= length(pixel.refs) , mean= s[m,h],sd= sqrt(fitkm$sigsq.eps[draw.refs[m]])  ) 
      
    }
    
    # proportions shouldn't be allowed to be outsie 0,1
    woodland.hyperthet[which(woodland.hyperthet>1)] <-1
    woodland.hyperthet[which(woodland.hyperthet<0)] <-0
    
    ####
    
    fitkm.2 <- kmbayes(y = c(woodland.true,woodland.hyperthet[c(sample.selection.2)] ) ,
                       Z = cbind(c(priority.score,Input_Data$Proximal_Priority_Score[sample.selection.2]),
                                 c(woodland.pred,Input_Data$woodland[sample.selection.2])),
                       X = cbind(rep(1,length(c(woodland.pred,sample.selection.2))),
                                 c(priority.score,Input_Data$Proximal_Priority_Score[sample.selection.2]),
                                 c(woodland.pred,Input_Data$woodland[sample.selection.2])),
                       verbose = FALSE, varsel = TRUE, iter = 1e4)
    
    
    start.time <- Sys.time()
    s.hyp<- SamplePred(fitkm.2, Znew = fitting.data[,c(1,2)], Xnew = fitting.data[,c(3,1,2)],sel=draw.refs)
    end.time <- Sys.time()
    
    print(end.time- start.time)
    
    
    s.1.hyp <- s.hyp
    for(j in 1:ncol(s.hyp)){
      s.1.hyp[,j] <- s.hyp[,j] + rnorm(n=length(draw.refs), mean=0,sd= sqrt(fitkm.2$sigsq.eps[draw.refs]))
    }
    
    results.list.sub.matrix[,m] <- as.vector(apply(s.1.hyp,2,sd))
  }
  
  results.list[[i]] <- results.list.sub.matrix
}

end.time <- Sys.time()
end.time-start.time.1
################################################################################################
for(k in 1:length(results.list)){
  path= file.path(filename=paste("Results_Sim_",k,".csv", sep=""))
  write.csv(results.list[[k]], file=path) 
  
}



