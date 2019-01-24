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
my_precision(0.1,2,8,t=1)
my_precision(0.1,2,9,t=1)
my_precision(0.1,2,10,t=1)
my_precision(0.1,2,11,t=1)
# n should be 11

#c
my_precision(0.1,2,11,t=3)
my_precision(0.1,2,8,t=3)
my_precision(0.1,2,7,t=3)
my_precision(0.1,2,6,t=3)
# n should be 7

