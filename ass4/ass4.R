library(MASS)


kidney = read.csv('Data19.18.csv')
kidney
diet = read.csv('Data21,7.csv')
diet


#23.13
kid_incomp = kidney[-which(kidney$Duration == 2& kidney$Weight.Gain == 1),]
kid_incomp$log_y = log10(kid_incomp$ï..Y+1)
kid_incomp$Weight.Gain = factor(kid_incomp$Weight.Gain)
kid_incomp$Duration = factor(kid_incomp$Duration)
fit1 = aov(log_y~Duration+Weight.Gain,data = kid_incomp)
summary(fit1)
fit2 = aov(log_y~Weight.Gain,data = kid_incomp)
summary(fit2)
qf(0.95,2,46)

# 23.19
diet = diet[-3,]
diet = diet[-13,]
diet
diet$Block = factor(diet$Block)
diet$Fat = factor(diet$Fat)
fit3 = aov(Lipid~Block+Fat,data = diet)
anova(fit3)

diet$fit = fit3$fitted.values
mu = tapply(diet$fit,diet$Fat, mean)
mu
?TukeyHSD
a = TukeyHSD(fit3,conf.level = 0.98)
plot(a)





#24.12
data3 = read.csv('Data24.12.csv')
data3$A = factor(data3$A)
data3$B = factor(data3$B)
data3$C = factor(data3$C)
fit4  = aov(Y~A+B+C, data = data3)
data3$fit = fit4$fitted.values
data3$res = fit4$residuals
anova(fit4)
mse = 777
fit4  = aov(Y~A+C+B, data = data3)


res_sd = (fit4$residuals-mean(fit4$residuals))/sd(fit4$residuals)
res_sort = sort(res_sd)

t = c()
n = length(res_sd)
for (i in seq(1,n-1)) {
    t[i] =  qnorm(i/n, mean = 0,sd = 1)
}
seq(1,n-1)
t
cor(res_sort[1:59],t)
plot(fit4,which=2)
boxcox(fit4)
?qnorm


fit5 = aov(Y~A*B*C, data = data3)
summary(fit5)
