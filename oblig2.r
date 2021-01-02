data = read.table("https://www.uio.no/studier/emner/matnat/math/STK1110/data/temp.txt", header=T)
menn = data$Menn
kvinner = data$Kvinner
boxplot(menn,kvinner, names = c("Menn","Kvinner"))

#qqplot
qqnorm(menn, ylab = "Menns kropptemperatur")
qqline(menn)
qqnorm(kvinner, ylab = "Kvinners kropptemperatur")
qqline(kvinner)


#1c
m = length(menn)
n = length(kvinner)
sp2 = (m-1)/(m+n-2)*sd(menn)^2 + (n-1)/(m+n-2)*sd(kvinner)^2 
t = (mean(menn)-mean(kvinner))/sqrt(sp2 *(1/length(menn) + 1/length(kvinner)) )
print(t)

a = (sd(menn)^2 /m)^2 /(m-1)
b = (sd(kvinner)^2 /n)^2 /(n-1)
v = ((sd(menn)^2 / m) + (sd(kvinner)^2 / n))^2 / (a+b)
print(v)

pvalue = 2*(1-pt(abs(t),18))
print(pvalue)

ovre = mean(menn) - mean(kvinner) + 2.101*sqrt(sp2 *(1/n + 1/m))
nedre = mean(menn) - mean(kvinner) - 2.101*sqrt(sp2 *(1/n + 1/m))
c(nedre,ovre)

t.test(menn,kvinner, var.equal = TRUE)

#1d
t = (mean(menn) - mean(kvinner))/sqrt(sd(menn)^2/m + sd(kvinner)^2/n)
print(t)

#1e
f = sd(menn)^2/sd(kvinner)^2
print(f)


#2b)
T = -3.26/1.58
p_verdi = 2*(1-pt(abs(T),31))
print(p_verdi)

#2c)

nedre = -3.26 - qt(0.025,df=30,lower.tail = F)*1.58
ovre = -3.26 + qt(0.025,df=30,lower.tail = F)*1.58
c(nedre,ovre)

#3a)

p_verdi = 2*(1-pnorm(1.61))
print(p_verdi)

#3b)

prop.test(c(486,441),c(3000,3000) , correct=F)



rm(list=ls(all=TRUE))

data = read.table("https://www.uio.no/studier/emner/matnat/math/STK1110/data/snoe_vann.txt", header=T)
sno =  c(23.1,data$X23.1)
vann = c(10.5,data$X10.5)
lm(vann~sno)

plot(sno,vann,xlab="Snoemengede",ylab = "Vannstand")
abline(lm(vann~sno))

#4b)


vann_res = resid(lm(vann~sno))
plot(sno,vann_res, ylab="Residualer", xlab="Sno")

#Standadiserer residualene 
vann_standard = rstandard(lm(vann~sno))
qqnorm(vann_standard)
qqline(vann_standard)

B0 = 0.2800       
B1 = 0.5056
n = length(sno)
SSE = sum((vann - (B0+B1*sno))^2)
s2 = SSE/(n-2)
print(s2)



S_B = n*s2 / ( n*sum(sno^2) - ((sum(sno))^2) )
ovre = B1 + S_B * 2.120
nedre = B1 - S_B * 2.120
c(nedre,ovre)














