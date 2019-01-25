library(MASS)
library(ggplot2)
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

