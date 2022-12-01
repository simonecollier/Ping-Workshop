#install.packages("neuralnet")
library(neuralnet)
library(dplyr)

#IMPORTS THE DATA
ping_data <- read.csv("allFishDat.csv")
ping_data=ping_data[,3:ncol(ping_data)]


# PROCESS AND NORMALIZE DATA

# keep two fish of LT and LW
#ping_data=ping_data[ping_data$fishNum=="LT014"|ping_data$fishNum=="LT016"|ping_data$fishNum=="LWF009"|ping_data$fishNum=="LWF005",]

#ping_data2<-ping_data%>%filter(fishNum=="LT014"|fishNum=="LT016"|fishNum=="LWF005"|fishNum=="LWF009")
ping_data2=ping_data[ping_data$fishNum!="SMB013"&ping_data$fishNum!="LT013"&ping_data$fishNum!="LWF007"&ping_data$fishNum!="BUR001"&ping_data$fishNum!="BUR002",]

F45index <- which(names(ping_data2) == "F45")

f1=seq(from=45,to=89.5,by=0.5) #first group of frequencies
f2=seq(from=90,to=170,by=0.5) #second group of frequencies
f3=seq(from=170.5,to=260,by=0.5) #third group of frequencies
listf1=seq(from=F45index,to=(F45index-1)+length(f1),by=1) #columns identifying the first group of frequencies
listf2=seq(from=F45index+length(f1),to=(F45index-1)+length(f1)+length(f2),by=1) #columns identifying the second group of frequencies
listf3=seq(from=F45index+length(f1)+length(f2),to=(F45index-1)+length(f1)+length(f2)+length(f3),by=1) #columns identifying the third group of frequencies
f1mar1=50
f1mar2=84.5
f2mar1=95
f2mar2=165
f3mar1=178
f3mar2=255
f1inc=f1>=(f1mar1)&f1<=(f1mar2) #list of frequencies to keep in the first group
f2inc=f2>=(f2mar1)&f2<=(f2mar2) #list of frequencies to keep in the second group
f3inc=f3>=(f3mar1)&f3<=(f3mar2) #list of frequencies to keep in the third group
freqs=c(f1[f1inc],f2[f2inc],f3[f3inc]) #kept frequencies
X1=exp(ping_data2[,listf1[f1inc]]/10) #first step in normalization (Gugele et al. 2021, Sci. Rep.)
X2=exp(ping_data2[,listf2[f2inc]]/10)
X3=exp(ping_data2[,listf3[f3inc]]/10)
X1=(X1-apply(X1,1,min))/(apply(X1,1,max)-apply(X1,1,min)) #second step in normalization
X2=(X2-apply(X2,1,min))/(apply(X2,1,max)-apply(X2,1,min))
X3=(X3-apply(X3,1,min))/(apply(X3,1,max)-apply(X3,1,min))
#X4=ping_data2[,c("Angle_minor_axis", "Angle_major_axis", "aspectAngle")] #auxiliary variables (ranges and depths)
X=cbind(X1,X2,X3)#,X4) #predictor matrix
Y=as.factor(ping_data2$species) #response variable: species ID
fish <- ping_data2$fishNum[!is.na(rowSums(X))]
Y=Y[!is.na(rowSums(X))]
X=X[!is.na(rowSums(X)),]


resampled_fishIndex <- c()
nping <- 300
for (ID in unique(fish)) {
  if (sum(fish == ID) < nping) {
    resampled_fishIndex <- c(resampled_fishIndex, which(fish == ID)) 
  } else {
    resampled_fishIndex <- c(resampled_fishIndex, sample(which(fish == ID), nping))
  }}

print(length(resampled_fishIndex))

Y=Y[resampled_fishIndex]
X=X[resampled_fishIndex,]

d<-cbind.data.frame(X,Y)

col_list <- paste(c(colnames(d[,1:366])),collapse="+")
col_list <- paste(c("Y~",col_list),collapse="")
f <- formula(col_list)

## For breaking into test and training
inp <- sample(2, nrow(d), replace = TRUE, prob = c(0.7, 0.3))
training_data <- d[inp==1, ]
test_data <- d[inp==2, ]

n <- neuralnet(f,
               data = training_data,
               hidden = 246, # hidden layers should be 2/3 of the input layer
               err.fct = "ce",
               linear.output = FALSE,
               lifesign = 'full',
               rep = 5,
               algorithm = "rprop+",
               stepmax = 100000)

#plot(n)

# Prediction - in sample
#Test the resulting output
training_data.2 <- training_data[,-367]
nn.results <- predict(n, training_data.2)
head(nn.results)
# nn.results gives the probabilities of each ping to be laketrout[,1] or lakewhitefish[,2]

results <- data.frame(actual = training_data$Y, prediction = nn.results$net.result)


# confusion Matrix  -Training data
# if probability is over 0.5 then it is classified as a lake trout
pred1 <- ifelse(nn.results[,1] > 0.5, "LT", "LW") # species 1 = laketrout
tab1 <- table(pred1, training_data[,367])
tab1

# misclassification error
1 - sum(diag(tab1)) / sum(tab1)

# Prediction - out sample
#Test the resulting output
test_data.2 <- test_data[,-367]
nn.results <-predict(n, test_data.2)

# confusion Matrix $-Test data
# if probability is over 0.5 then it is classified as a lake trout
pred1 <- ifelse(nn.results[,1] > 0.5, "LT", "LW") # species 1 = laketrout
tab1 <- table(pred1, test_data[,367])
tab1

# misclassification error
1 - sum(diag(tab1)) / sum(tab1)

# accuracy
sum(diag(tab1)) / sum(tab1)
