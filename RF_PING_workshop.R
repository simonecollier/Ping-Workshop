

library(randomForest)

#IMPORTS THE DATA
ping_data <- read.csv("allFishDat.csv")
#ping_data=ping_data[,2:ncol(ping_data)]


# PROCESS AND NORMALIZE DATA
#ping_data=ping_data[ping_data$fishNum!="SMB013"&ping_data$fishNum!="LT013"&ping_data$fishNum!="LT019"&ping_data$fishNum!="LWF007"&ping_data$fishNum!="BUR001"&ping_data$fishNum!="BUR002",]
F45index <- which(names(ping_data) == "F45")

f1=seq(from=45,to=89.5,by=0.5) #first group of frequencies
f2=seq(from=90,to=170,by=0.5) #second group of frequencies
f3=seq(from=170.5,to=260,by=0.5) #third group of frequencies
listf1=seq(from=F45index,to=(F45index-1)+length(f1),by=1) #columns identifying the first group of frequencies
listf2=seq(from=F45index+length(f1),to=(F45index-1)+length(f1)+length(f2),by=1) #columns identifying the second group of frequencies
listf3=seq(from=F45index+length(f1)+length(f2),to=(F45index-1)+length(f1)+length(f2)+length(f3),by=1) #columns identifying the third group of frequencies
f1mar1=50
f1mar2=85.5
f2mar1=95
f2mar2=165
f3mar1=178
f3mar2=255
f1inc=f1>=(f1mar1)&f1<=(f1mar2) #list of frequencies to keep in the first group
f2inc=f2>=(f2mar1)&f2<=(f2mar2) #list of frequencies to keep in the second group
f3inc=f3>=(f3mar1)&f3<=(f3mar2) #list of frequencies to keep in the third group
freqs=c(f1[f1inc],f2[f2inc],f3[f3inc]) #kept frequencies
# X1=exp(ping_data[,listf1[f1inc]]/10) #first step in normalization (Gugele et al. 2021, Sci. Rep.)
# X2=exp(ping_data[,listf2[f2inc]]/10)
# X3=exp(ping_data[,listf3[f3inc]]/10)
# X1=(X1-apply(X1,1,min))/(apply(X1,1,max)-apply(X1,1,min)) #second step in normalization
# X2=(X2-apply(X2,1,min))/(apply(X2,1,max)-apply(X2,1,min))
# X3=(X3-apply(X3,1,min))/(apply(X3,1,max)-apply(X3,1,min))
#X4=ping_data[,c("Angle_minor_axis", "Angle_major_axis", "aspectAngle")] #auxiliary variables (ranges and depths)
#X=cbind(X1,X2,X3,X4) #predictor matrix

X1=ping_data[,listf1[f1inc]] #first step in normalization (Gugele et al. 2021, Sci. Rep.)
X2=ping_data[,listf2[f2inc]]
X3=ping_data[,listf3[f3inc]]
XF <- cbind(X1,X2,X3)
XF <- exp(XF/10)
XF=(XF-apply(XF,1,min))/(apply(XF,1,max)-apply(XF,1,min))
#X4=ping_data[,c("Angle_minor_axis", "Angle_major_axis", "aspectAngle")] #auxiliary variables (ranges and depths)
#X=cbind(XF,X4) #predictor matrix
X = XF

Y=as.factor(ping_data$species) #response variable: species ID
fish <- ping_data$fishNum[!is.na(rowSums(X))] #fish ID column without missing pings
ping_sub <- ping_data[!is.na(rowSums(X)),]
Y=Y[!is.na(rowSums(X))]
X=X[!is.na(rowSums(X)),]

# RESAMPLE DATA
# cap the number of pings from one individual at nping
resampled_fishIndex <- c()
nping <- 500
for (ID in unique(fish)) {
  if (sum(fish == ID) < nping) {
    resampled_fishIndex <- c(resampled_fishIndex, which(fish == ID)) 
    print(ID)
  } else {
    resampled_fishIndex <- c(resampled_fishIndex, sample(which(fish == ID), nping))
  }}


lentrain <- round(length(resampled_fishIndex)*0.70)
trainInd <- sample(resampled_fishIndex, lentrain)
xtrain <- X[trainInd,]
ytrain <- Y[trainInd]
xtest <- X[-trainInd,]
ytest <- Y[-trainInd]

print(paste("Number of pings used to train model:", lentrain))

#Y=Y[resampled_fishIndex]
#X=X[resampled_fishIndex,]

# TUNE PARAMETER MTRY
# NX=1:50 #vector with values of mtry
# NXERR=array(NX*0) #array that will store error statistic
# for (nx in 1:length(NX)){ #loops over NX
#   print(nx)
# rftry = randomForest(X,Y,ntree=500,mtry=NX[nx]) #runs random forest
# NXERR[nx]=rftry$err.rate[nrow(rftry$err.rate),1] #stores error rate
# }
# mbest=min(NX[NXERR==min(NXERR)]) #selects best mtry parameter (with lowest error)
# RUNS MODEL WITH BEST MTRY

rf = randomForest(x=xtrain, y=ytrain, xtest=xtest, ytest=ytest, ntree=500, mtry = 30, importance=TRUE) #runs random forest

#SUMMARY
print(rf)
rf$confusion #confusion matrix
accu=1-rf$err.rate[nrow(rf$err.rate),1] #accuracy
ivars=importance(rf) #relative importance of variables
igini=ivars[,4] #relative importance, based on mean decrease in Gini distances
plot(freqs,igini[1:length(freqs)]) #plot relative importance as a function of frequency
lines(freqs,igini[1:length(freqs)])


boxplot(Angle_minor_axis~species ,data = ping_sub)
boxplot(Angle_major_axis~species ,data = ping_sub)
boxplot(aspectAngle~species, data = ping_sub)
boxplot(totalLength~species, data = ping_sub)

median(ping_sub$aspectAngle[which(ping_sub$species == "lakeTrout")])
median(ping_sub$aspectAngle[which(ping_sub$species == "lakeWhitefish")])
median(ping_sub$aspectAngle[which(ping_sub$species == "smallmouthBass")])

