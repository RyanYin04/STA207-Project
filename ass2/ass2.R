library(MASS)
library(ggplot2)
library(onewaytests)
# ======================================================================
# 17.27
# Get the meta data
data0 = read.table('CH16PR09.txt')

# Initialize the data
names(data0) = c('days','level','observation_numer')
head(data0)
sapply(data0, typeof)
data0$level = as.factor(data0$level)
qt
#a
# Write a function to get the precision conveniently
my_precision = function(a,m,n,t){
    # t is the multiplier of the n in the sample size of average group
    s0 = 4.5
    b = qt(1-a/2*m,(2+t)*n-3)
    s = sqrt(((t+1)/(t*n))*s0^2)
    M = b*s
    M
}
my_precision(0.1,2,8,t=2)
my_precision(0.1,2,9,t=2)
my_precision(0.1,2,10,t=2)
# n should be between 8 and 9, closer to 9, since n should be an integer, so n=8

#b
# when t==1
my_precision(0.1,2,8,t=1)
my_precision(0.1,2,9,t=1)
my_precision(0.1,2,10,t=1)
my_precision(0.1,2,11,t=1)
# n should be 11

# when t == 3
my_precision(0.1,2,11,t=3)
my_precision(0.1,2,8,t=3)
my_precision(0.1,2,7,t=3)
my_precision(0.1,2,6,t=3)
# n should be 7
# ============================================================================

# 18.6
# Get all the estimate and write them in the data frame:
data1 = data0
data1$fit = 0
c = tapply(data1$days,data1$level,mean)
data1$fit[which(data1$level==1)] = c[1]
data1$fit[which(data1$level==2)] = c[2]
data1$fit[which(data1$level==3)] = c[3]
data1$res = data1$days -data1$fit
data1$level = as.numeric(data1$level)
plot(data1$level,data1$res, col = data1$level)

mse = sum(data1$res^2)/(length(data1$days)-3)
mse

pnorm(-2.111,mean =0, sd=1)

sorted = sort(data1$res/sqrt(mse))
my_qnorm = function(x){
    temp = c()
    for (i in 1:length(x)){
        temp[i] = pnorm(x[i], mean = 0, sd=1)
    }
    return(temp)
}

temp1 = my_qnorm(sorted)
cor(temp1,sorted)
plot(data1$observation_numer, data1$res, col = data1$level)
ggplot(data = data1, aes(x = observation_numer, y = res))+geom_point()+facet_grid(level~.)

#==============================================================================


# 18.17
# Load the data
wind = read.csv('Data18.17.csv')
head(wind)


#a
class(wind$Speed)
mu = tapply(wind$Breaks, wind$Speed, mean)
mu
wind$fit = 0

wind$fit[which(wind$Speed == 1)] = mu[1]
wind$fit[which(wind$Speed == 2)] = mu[2]
wind$fit[which(wind$Speed == 3)] = mu[3]
wind$fit[which(wind$Speed == 4)] = mu[4]

wind$res = wind$Breaks - wind$fit


#b
plot(wind$Observation.number,wind$res, col = wind$Speed, 
     xlab = 'Observation number', ylab = 'Residual')
ggplot(data = wind, aes(x = Observation.number, y = res))+
    geom_point()+facet_grid(Speed~.)
ggplot(data = wind, aes(x = Speed, y = res))+geom_point()
ggplot(data = wind, aes(x = fit, y = res))+geom_point()

#c
bf.test(Breaks~Speed, data = wind, alpha = 0.05, na.rm = TRUE, verbose = TRUE)



#d
S = tapply(wind$Breaks, wind$Speed, sd)
S
r = rank(wind$Breaks,ties.method = 'min')
wind$rank = r
wind$Speed = factor(wind$Speed)
rr = tapply(wind$rank, wind$Speed, mean)
rr
(S^2)/mu
 S/mu
 S/mu^2
 # Logrithmic tran is reasonable


aov.wind = aov(wind$Breaks~wind$Speed)
summary(aov.wind)
?plot
plot(aov.wind,which = 2)
a = boxcox(aov.wind)
# ===================================================================================
# 18.18
# a

data2 = wind
d
data2$log.b = log10(data2$Breaks)
mub = tapply(data2$log.b, data2$Speed, mean)
data2 = data2[,-1]
data2$fit[which(wind$Speed == 1)] = mub[1]
data2$fit[which(wind$Speed == 2)] = mub[2]
data2$fit[which(wind$Speed == 3)] = mub[3]
data2$fit[which(wind$Speed == 4)] = mub[4]

data2$res = data2$log.b-data2$fit

#b
# Residual plots
ggplot(data = data2, aes(x = Observation.number, y = res))+
    geom_point()+facet_grid(Speed~.)
ggplot(data = data2, aes(x = Speed, y = res))+geom_point()
ggplot(data = data2, aes(x = fit, y = res))+geom_point()

# Corelation 
mse2 = sqrt(sum(data2$res^2)/(length(data2$Speed)-4))
mse2
sorted2 = sort(data2$res/mse2)
my_qnorm = function(x){
    temp = c()
    for (i in 1:length(x)){
        temp[i] = pnorm(x[i], mean = 0, sd=1)
    }
    return(temp)
}
temp2 = my_qnorm(sorted2)
cor(temp2,sorted2)


#c
#BF test
bf.test(log.b~Speed, data = data2, alpha = 0.5, na.rm = TRUE, verbose = TRUE)
med = tapply(data2$log.b, data2$Speed, median)
med
data2$d = 0


# =============================
# 19.5
# a
mu = mean(c(250,265,268,269,288,273,270,269))
mu
a1 = mean(c(250,265,268,269) - mu)
a2 = mean(c(288,273,270,269)) - mu
b1 = mean(c(250,288)-mu)
b2 = mean(c(265,273)) - mu
b3 = mean(c(268,270)) - mu
b4 = mean(c(269,269)) - mu
a = c(a1,a2)
b = c(b1,b2,b3,b4)
a
b

#b
alevel=1:2
blevel = 1:4
mu0 = c(250,265,268,269,288,273,270,269)
blevs = 1:4
data3 = data.frame(mu = mu0, b = blevs, a = alevs)
data3$a[1:4] = 1
data3$a[5:8] = 2
data3
ggplot(data = data3)+ geom_line(aes(x = b,y = mu,group = a))

#c
data3$log_ = log(data3$mu)
ggplot(data = data3)+ geom_line(aes(x = b,y = log_,group = a))
