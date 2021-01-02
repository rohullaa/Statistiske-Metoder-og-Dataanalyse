rm(list=ls(all=TRUE))

#moment_estimator
data = read.table("https://www.uio.no/studier/emner/matnat/math/STK1110/data/forsikringskrav.txt", header=T)
tall = data$X7.708

sigma2 = log(1+(var(tall)/mean(tall)^2))
mu = log(mean(tall)) - 1/2 * sigma2^2

#MLE_estomator
m = 1/length(tall) * sum(log(tall))
s2 = 1/length(tall) * sum((log(tall) - m)^2)
c("Moment","Forventingsverdi:",mu,"Variansen:",sigma2)
c("MLE", "Forventingsverdi:",m,"Variansen:",s2)

phi = exp(m +1/2*s2)

#beregning_konfidens_intervall:

x_strek = mean(tall)
sigma = sd(tall)
z = 1.96
n = length(tall)
low = x_strek - (z*sigma /sqrt(n))
high = x_strek + (z*sigma /sqrt(n))
c("Lav: ",low,"Høy:",high)

#opp_1g_konfidens_varians:
nedre = (n-1)*var(tall)/qchisq(0.025, df=n-1)
ovre = (n-1)*var(tall)/qchisq(0.975, df=n-1)
c("Nedre: ",low,"Ovre:",high)


##OPPGAVE2:
#oppb:
prosent_b = vector()
for(j in 1:10){
  matrise_str = 10000
  M = matrix(data = NA, nrow = matrise_str, ncol = 15, byrow = FALSE, dimnames = NULL)
  ovre_vektor = vector()
  nedre_vektor = vector()
  num = 0
  for(i in 1:matrise_str){
    M[i,] = rnorm(15,558,30)
    #intervallet_fra_opp_a
    nedre_vektor[i] = mean(M[i,])-qt(0.975,14)*sd(M[i,])/sqrt(15) 
    ovre_vektor[i] = mean(M[i,])+qt(0.975,14)*sd(M[i,])/sqrt(15)
    if(nedre_vektor[i] < 558 && ovre_vektor[i] > 558){
      num = num +1 }
  }
  prosent = num/matrise_str * 100
  prosent_b[j] = prosent
  print(c("Kjoring: ", j, "Prosent: ",prosent, "Antall 558: ", num))
}
#opp_c
for(j in 1:10){
  matrise_str = 10000
  M = matrix(data = NA, nrow = matrise_str, ncol = 15, byrow = FALSE, dimnames = NULL)
  ovre_vektor = vector()
  nedre_vektor = vector()
  num = 0
  for(i in 1:matrise_str){
    M[i,] = rnorm(15,558,30)
    nedre_vektor[i] = mean(M[i,])-1.96*sd(M[i,])/sqrt(15) 
    ovre_vektor[i] = mean(M[i,])+1.96*sd(M[i,])/sqrt(15)
    if(nedre_vektor[i] < 558 && ovre_vektor[i] > 558){
      num = num +1 }
  }
  print(c("Antall 558: ", num,"Prosent",num/matrise_str * 100))
  
  prosent = num/matrise_str * 100
  print(c("Kjoring: ", j, "Prosent: ",prosent, "Antall 558: ", num))
}

#opp_d
for(j in 1:10){
  matrise_str = 10000
  M = matrix(data = NA, nrow = matrise_str, ncol = 15, byrow = FALSE, dimnames = NULL)
  ovre_vektor = vector()
  nedre_vektor = vector()
  num = 0
  n = 15
  for(i in 1:matrise_str){
    M[i,] = rnorm(15,558,30)
    nedre_vektor[i] = sqrt((n-1)*sd(M[i,])^2 /(26.119 ))
    ovre_vektor[i] =sqrt( (n-1)*sd(M[i,])^2 /(5.629) )
    if(nedre_vektor[i] < 30 && ovre_vektor[i] > 30){
      num = num +1 }
  }
  print(c("Antall 30: ", num,"Prosent",num/matrise_str * 100))
  
  prosent = num/matrise_str * 100
  
  print(c("Kjoring: ", j, "Prosent: ",prosent, "Antall 30: ", num))
}

#opp_e:
prosent_e = vector()
for(j in 1:10){
  matrise_str = 10000
  M = matrix(data = NA, nrow = matrise_str, ncol = 15, byrow = FALSE, dimnames = NULL)
  Z = matrix(data = NA, nrow = matrise_str, ncol = 15, byrow = FALSE, dimnames = NULL)
  ovre_vektor = vector()
  nedre_vektor = vector()
  num = 0
  sigma = 30
  mu = 558
  
  for(i in 1:matrise_str){
    #trekker data t7 ved rt()
    Z[i,] = rt(n = 15, df = 7)
    #lager nye xi ved formelen i oppgaven
    M[i,] = mu + sigma*Z[i,]
    
    #intervallet_fra_opp_a
    nedre_vektor[i] = mean(M[i,])-qt(0.975,14)*sd(M[i,])/sqrt(15) 
    ovre_vektor[i] = mean(M[i,])+qt(0.975,14)*sd(M[i,])/sqrt(15)
    if(nedre_vektor[i] < mu && ovre_vektor[i] > mu){
      num = num +1 }
  }
  print(c("Antall 558: ", num,"Prosent",num/matrise_str * 100))
  prosent= num/matrise_str * 100
  prosent_e[j] = prosent
  
  print(c("Kjoring: ", j, "Prosent: ",num/matrise_str * 100, "Antall 558: ", num))
  
}


for(i in 1:10){
  print(c("Kjoring:",i,"Opp_b:", prosent_b[i],"Opp_e:", prosent_e[i] ))
}

for(j in 1:10){
  matrise_str = 10000
  M = matrix(data = NA, nrow = matrise_str, ncol = 15, byrow = FALSE, dimnames = NULL)
  Z = matrix(data = NA, nrow = matrise_str, ncol = 15, byrow = FALSE, dimnames = NULL)
  ovre_vektor = vector()
  nedre_vektor = vector()
  num = 0
  n = 15
  sigma_tilde = 30*1.4
  for(i in 1:matrise_str){
    #trekker data t7 ved rt()
    Z[i,] = rt(n = 15, df = 7)
    
    #lager nye xi ved formelen i oppgaven
    M[i,] = mu + sigma*Z[i,]
    
    #variansen til Xi er 1.4*Sigma^2
    nedre_vektor[i] = sqrt((n-1)*(sd(M[i,]*1.4))^2 /(26.119 ))
    ovre_vektor[i] = sqrt((n-1)*(sd(M[i,]*1.4))^2 /(5.629) )
    if(nedre_vektor[i] < sigma_tilde && ovre_vektor[i] > sigma_tilde){
      num = num +1 }
  }
  print(c("Antall 1.4*30: ", num,"Prosent",num/matrise_str * 100))
  
  print(c("Kjoring: ", j, "Prosent: ",prosent, "Antall 42: ", num))
}








