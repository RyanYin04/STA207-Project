#====================================
#--------------16.5------------------

mu = c(5.1,6.3,7.9,9.5)
sigma = 2.8
r = 4
#-------------------a---------------------------------
curve(dnorm(x, 5.1, sigma), from = -10, to = 20,ylab = ' ',col = 'red')
curve(dnorm(x, 6.3, sigma), from = -10, to = 20,add = T, col = 'blue')
curve(dnorm(x, 7.9, sigma), from = -10, to = 20,add = T,col = 'green')
curve(dnorm(x, 9.5, sigma), from = -10, to = 20,add = T,col = 'purple')
#------------------ b---------------------------------
# It is a balanced model, so the wieght shoulde be 1/4
Mu = (1/4)*(sum(mu))
n = 100
E_MSTR = sigma^2 + sum(n*(mu - Mu)^2) / (r-1)
E_MSTR

E_MSE = sigma^2

E_MSTR/E_MSE # 47.76871
# F* is really large which can indicate that we should reject the null hypothesis. So the 
# mean should not be equal.

#------------------c----------------------------------
mu2 = c(5.1,5.6,9.0,9.5)
Mu2 = 1/4*(sum(mu2))
E_MSTR2 = sigma^2 + sum(n*(mu2 - Mu2)^2) / (r-1)
E_MSTR2
# Even the range stays the same, the average changes a lot which means the estimation
# will change a lot based on the level of the factor. 


#============================================================
#--------------------16.9-----------------------------------
# Set up the data frame
data2 = read.table("CH16PR09.txt")
names(data2) = c('days','fitness_level','sample_size')

nt = nrow(data2)
r = 3

#----------------------a--------------------------------------  
data2
plot(data2$fitness_level,data2$days )
# From the plot, the means tend to be different




# The varability are 2.4,2.7,2.3 respectively. Close to each other.


#-------------------------b-------------------------------
data2_low = data2[which(data2$fitness_level==1),]
data2_moderate = data2[which(data2$fitness_level==2),]
data2_high = data2[which(data2$fitness_level==3),]
fit1 = mean(data2_low$days)
fit2 = mean(data2_moderate$days)
fit3 = mean(data2_high$days)

#---------------------c-----------------------------
data2$fit = 0
data2$fit[which(data2$fitness_level==1)] = fit1
data2$fit[which(data2$fitness_level==2)] = fit2
data2$fit[which(data2$fitness_level==3)] = fit3

data2$residual = data2$days - data2$fit
sum(data2$residual)
# Equals to zero

#-----------------------e--------------------------------
Mu = sum(data2$days)/nt
sstr = sum((data2$fit-Mu)^2)
sse = sum(data2$residual^2)
mstr = sstr / (r-1)
mse = sse/(nt -r)
f = mstr/mse
f
# F = 12
qf(0.01, r-1, nt-r)
# Should reject the null

#---------------------f----------------------------------

pf(f,r-1,nt-r)

# ================================================================
# 17.10
#-----------------------a-----------------------------
head(data2)
mu1 = mean(data2_low$days)
mu2 = mean(data2_moderate$days)
mu3 = mean(data2_high$days)
plot(c(1,2,3),c(mu1,mu2,mu3),xlab = 'level - i', ylab = 'days',type = 'o')


#------------------------b-----------------------------
n1 = nrow(data2_low)
n2 = nrow(data2_moderate)
n3 = nrow(data2_high)
mse2 = sum((data2_moderate$days - data2_moderate$fit)^2)/(nt - r)
s2 = sqrt(mse1)
qt2 = qt(0.995,nt-r)
lb2 = -qt1*s1+data2$fit[1]
ub2 =  qt1*s1+data2$fit[1]
s2





#------------------------c-----------------------------
mse = sum(data2$residual^2)/(nt-r)
sl1 = sqrt((1/n2 + 1/n3)*mse)
sl2 = sqrt((1/n1 + 1/n2)*mse)
b = qt(p = 1-0.05/4, df = nt-r)
b
b*sl1
df1 = mu2-mu3
df2 = mu1-mu2
c(df1-b*sl1,df1+b*sl1)
c(df2-b*sl2,df2+b*sl2)

#------------------------d-----------------------------
?TukeyHSD
data2$fitness_level = factor(data2$fitness_level)
fit1 = aov(days~fitness_level, data = data2)
fit1
TukeyHSD(fit1,conf.level = 0.95)
pairwise.t.test(x = data2$days,g = data2$fitness_level, p.adjust.method = 'bonferroni')


#=================================================
#17.15
sl3 = sqrt(mse*(1/n1+4/n2+1/n3))
qt2 = qt(0.995,21)
l3 = mu1-2*mu2+mu3
c(l3-qt2*sl3,l3+qt2*sl3)


s1d1 = sqrt(mse*(1/n1+1/n2))
dff1 = mu1-mu2
s1d2 = sqrt(mse*(1/n1+1/n3))
dff2 = mu1-mu3
s1d3 = sqrt(mse*(1/n2+1/n3))
dff3 = mu2-mu3
s1d4 = sl3
dff4 = mu1-2*mu2+mu3
B=qt(1-0.05/8, nt-r)
B




#=================================================
#16.26
sigma = 4.5
sigma^2
mu1 = 37
mu2 = 35
mu3 = 28
mu = n1/nt*mu1+n2/nt*mu2+n3/nt*mu3
mu
mstr = (n1*(mu1-mu)^2+n2*(mu2-mu)^2+n3*(mu3-mu)^2)/(r-1)
mstr
data2$newfit = 0
data2$newfit[which(data2$fitness_level==1)]=37
data2$newfit[which(data2$fitness_level==2)]=35
data2$newfit[which(data2$fitness_level==3)]=28
data2$newres = data2$days-data2$newfit
sse = sum(data2$newres^2)
mse = sse/(nt-r)
mse
mstr/mse
