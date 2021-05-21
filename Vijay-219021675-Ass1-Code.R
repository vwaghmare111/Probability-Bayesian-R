library("stats")
library("normtest")
library("lmtest")

setwd("D:/01 MS Data Science/04 Academics/03 Sem/Bayesian Learning and Graphical models/Assignments/01 Due date 26-Apr/Submission")

############ QUESTION 1 ############

#Importing LZI dataset
the.data <- as.matrix(read.csv("LZIsData.csv", header = FALSE, sep = ",")) 

#Adding header to the.data
colnames(the.data)<-c("Air_Temperature","Humidity","Wind_Speed", "Air_Pressure" )

#Generating a sample of 1500 data using the following
set.seed(1)
my.data <- the.data [sample(1: 4464,1500),c(1:4)]
summary(my.data)

#Saving sample data
write.table(my.data,"Vijay-219021675-LzMyData.txt")

#1.1.a Histogram of Air Temperature 
hist(my.data[,"Air_Temperature"], col = "blue", xlab = "Air Temperature",
     main = "Histogram: Air Temperature")


#1.1.b. Histogram of Air Pressure
hist(my.data[,"Air_Pressure"], col = "blue", xlab = "Air Pressure",
     main = "Histogram: Air Pressure")


#1.2.a Box plot of Air Temperature and Wind Speed

# Plotting both the variables together gives unclear picture as range of Wind speed is greater
boxplot(my.data[,"Air_Temperature"], my.data[,"Wind_Speed"],
        main = "Air Temperature Boxplot", names = c("Air Temperature","Wind Speed"))

#Plotting both variables in one frame
par(mfrow = c(1,2))
boxplot(my.data[,"Air_Temperature"], main = "Air Temperature Boxplot", ylab = "Air Temperature")
boxplot(my.data[,"Wind_Speed"], main = "Wind Speed Boxplot", ylab = "Wind Speed")

#1.2.b Five number summary of Air Temperature and Wind Speed
summary(my.data[,"Air_Temperature"]) # Air Temperature
summary(my.data[,"Wind_Speed"])     # Wind Speed


#1.3 Summary statistics and plot
humidity_plot<-density(my.data[,"Humidity"])
plot(humidity_plot, main = "Humidity distribution", xlab = "Humidity") # Density plot
summary(my.data[,"Humidity"]) ## Mean and median are similar

shapiro.test(my.data[,"Humidity"]) #Shapiro-Wilk Normality Test, Reject null hypothesis of normality
jb.norm.test(my.data[,"Humidity"]) # JV Test, Reject null hypothesis of normality


#1.4 Scatterplot and Regression
first1000<-my.data[1:1000,]# Creating dataframe of first 1000 rows
first1000
plot(first1000[,"Air_Temperature"],first1000[,"Humidity"], 
     xlab = "Air Temperature", ylab = "Humidity", main = "Regression for Air Temperature on Humidity")
abline(lm(Humidity~Air_Temperature, data = as.data.frame(first1000)),col = 'red')

lm_model<-lm(Humidity~Air_Temperature, data = as.data.frame(first1000))#Fitting model for equation
summary(lm_model) #Summary of simple linear regression model
correlation<-cor(first1000[,"Air_Temperature"],first1000[,"Humidity"])# Correlation coefficient
correlation
coeff_det<-correlation^2  ## Coefficient of determination which is approximately
coeff_det                 ## equal to R squqred in lm model above. 
summary(lm_model)$r.squared ## Coefficient of determination as per model


############ QUESTION 4.c #################

x <- seq(-10,15,length=200)
colors <- c("black", "blue", "red")
#labels <- c("prior (a=0.1, b=0.1)", "likelihood (mean=0.167)", "posterior (a=6.1, b=36.1)")
labels<- c("prior", "likelihood", "posterior")

#Prior###
#prior is a Gamma with a = 0.1 and b = 0.1
a = 0.1; b = 0.1
priorTheta <- dgamma(x, shape = a, rate = b)
# Normalize so that values sum to 1
priorTheta<-priorTheta/sum(priorTheta)
plot(x, priorTheta, type="n", xlab="", ylab="", ylim=c(0, 0.05),
     main="Bayesian estimation", axes=TRUE)
lines(x, priorTheta, lwd=2, col=colors[1])


##Likelihood##
likelihood_exp<-1/mean(x)
pDataGivenTheta <- dexp(x, rate = likelihood_exp , log = FALSE)
# Normalize so that values sum to 1
pDataGivenTheta<-pDataGivenTheta/sum(pDataGivenTheta)
lines(x, pDataGivenTheta,lwd=2, col=colors[2])
legend("topleft", inset=.001,
       labels, lwd=2, col=colors)

#posterior
## Bayes theorem: likelihood * prior / pData
pData <- sum(pDataGivenTheta*priorTheta) # marginal probability of the data
pThetaGivenData <- pDataGivenTheta*priorTheta / pData #posterior distribution
lines(x, pThetaGivenData,lwd=2, col=colors[3])
legend("topleft", inset=.001,
       labels, lwd=2, col=colors)

#posterior mean: Note that mean=E(x) --- expected value of x.
a1 = length(x)+a; b1 = sum(x)+b
posteriorMean<- dgamma(x, a1, rate = b1)
lines(x,posteriorMean,lwd=2, col=colors[3])


############ QUESTION 5.D #################
library(Bolstad)
theta = seq(0,15,by=0.1)

#define the trapenzoidal prior
theta.prior = rep(0,length(theta))
theta.prior[theta >=4 & theta <=4.75] = -16/3 + theta[theta >=4 & theta<=4.75]*4/3
theta.prior[theta>4.75 & theta<=6] = 24/5 - theta[theta>4.75 & theta<=6]*4/5

#Find posterior mean and standard deviation
results = normgcp(c(5),0.25,density ='user', mu = theta, mu.prior = theta.prior)
mean(results)
var(results)
sd(results)

#plot prior, liklihood and posterior on a single plot
plot(results, overlay = TRUE, which = 1:3)

#plot the above results (prior, liklihood. posterior) in different axes
decomp(results)


############ QUESTION 6 #################

zz<-read.table("IOCdata2020.txt") 
zz<-as.matrix(zz)

#6.1.a Scatter plot
plot(zz[,'V1'],zz[,'V2'])
# 3 clusters can be found

#6.1.c fitting kmeans clustering model with k = 3
model_k3 = kmeans(zz,3, nstart=50)
model_k3$cluster
model_k3$tot.withinss
col = c("blue","red","green")
plot(zz, col=(model_k3$cluster), main="K-Means Clustering
+ Results with K=3", xlab ="", ylab="", pch =20, cex =2)

plot(model_k3$cluster, col=model_k3$cluster, main = "Data points in Clusters",
     xlab = "Data", ylab = "Clusters")


#6.1.d Number of clusters varying from 2 to 20

# Function to run kmeans 20 times
totwss<-c()
for (k in 1:20){
  model_km = kmeans(zz,k,nstart = 50)
  d = model_km$tot.withinss
  totwss[k] = d
}

#plotting total within SS w.r.t. iterations
totwss = as.data.frame(totwss)
plot(totwss$totwss, main="total within sum of squres (totWSS) with
diiferent K value", xlab = "Clusters", ylab = "TOTWSS")
totwss$totwss

#Creating difference series of total within SS
for(i in 1:nrow(totwss)){
  totwss1 = totwss[i+1,] - totwss[i,]
  diff_totwss[i] = totwss1
}

#Plotting difference in subsequent clusters totwss 
plot(diff_totwss, main = "Difference in subsequent clusters TOTWSS",
     xlab = "Difference", ylab = "Clusters")


# 6.2 Spectral clustering

#Normalizing dataset
maxMinScale <- function(x){(x-min(x))/(max(x)-min(x))}
xx<-maxMinScale(zz)
plot(xx)

#Compute similarity matrix
dXX<-as.matrix(dist(xx)) # compute Euclidean distance between data points
cParam = 1 # parameter of similarity function
S<-exp(-dXX/cParam) #compute similarity matrix
S
plot(dXX,S)

#Compute affinity matrix
#S-distance matrix and k-no of neighbours
AffMat<-function(S,k) #S-distance matrix and k-no of neighbours
{
  AM <- matrix(0,nrow=nrow(S),ncol=ncol(S))
  for(i in 1:nrow(S)){
    d <- sort(S[i,],decreasing=TRUE)
    for (t in 1:ncol(S))
    {
      if (S[i,t] < d[k])
      {
        AM[i,t]<-0
        AM[t,i]<-0
      }
      else
      {
        AM[i,t] <- S[i,t]
        AM[t,i] <- AM[i,t]
      }
    }
  }
  AM
}
A<-AffMat(S,3)
A


#Compute degree of Affinity matrix
D <- diag(apply(A, 1, sum)) # sum rows
D

#compute graph laplasian matrix (un-normalised)
L <- D - A
L

#find eigenvalues and eigenvectors
eigL<-eigen(L)
eigL
plot(eigL$values)

#smallest eigenvalues of L forming 3 clusters 
k<-3
Z<- eigL$vectors[,(ncol(eigL$vectors)-k+1):ncol(eigL$vectors)]
#plot data using the two eigenvectors
plot(Z)

# Perform k means clustering
library(stats)
km <- kmeans(Z, centers=k, nstart=50)
plot(Z, col=km$cluster, main = "Eigen values clusters")
plot(km$cluster, col=km$cluster, main = "Data points in Clusters",
     xlab = "Data", ylab = "Clusters")
plot(xx, col=km$cluster, main = "Original data in clusters")
km$cluster
km$tot.withinss


############ QUESTION 7 #################

#Importing data
WTempdata <- as.matrix(read.csv("HeronIslandWaterTemp.csv", header=TRUE, sep = ","))

#7.1 Time series plot
plot.ts(WTempdata, main = "Water Temp Data")
summary(WTempdata)
sd(WTempdata)

#7.2 Histogram
hist(WTempdata) #Bimodal

#7.3 Fit a Gaussian model
library(fitdistrplus)

#Fitting gaussian model on data
gauss_model<-fitdistr(WTempdata,"normal")

#Mean and standard deviation
para<-gauss_model1$estimate
para

# MLE of parameters of Gaussian model
mean(WTempdata) #Empirircal mean
sd(WTempdata)   #Empirircal standard deviation


#Plotting histogram of data with Gaussian density distribution
hist(WTempdata, prob = TRUE)
curve(dnorm(x, para[1], para[2]), col = 2, add = TRUE, lwd=2)


#7.4 Mixture of Gaussians
library("mixtools")
set.seed(2)
mixmdl = normalmixEM(WTempdata)# Default is 2 mixes and we have two modes
mixmdl
summary(mixmdl)
mixmdl$lambda #Mixing coefficients pi
mixmdl$mu #Mean of each Gaussian mix
mixmdl$sigma #Standard deviation of each Gaussian mix


#7.5 Ploting density of mix Gaussian and original data
plot(mixmdl,which=2)
lines(density(WTempdata), lty=2, lwd=2)

#7.6 Ploting log likelihood values over each iterations
plot(mixmdl$all.loglik, main = "Log-Likelihood values", xlab = "Iterations",
     ylab = "Log-Likelihood") #Log likelihood value plot with each iteration
