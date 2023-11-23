install.packages("cubature")
library(cubature)
library("data.table")

h=function(a,b,c,d,e,f){exp(a*b*c*d*e*f)}

adaptIntegrate(h,rep(0,1),rep(0,1),rep(0,1),rep(0,1),rep(0,1),rep(0,1),rep(0,1))

n=0
MCsample=0
expected=1

for (i in 1:10^6) {
n[i]=i
u=runif(6)
MCsample[i]={exp(prod(u))}
expected[i]=(sqrt(n[i]))/sqrt(n[i+1])}

sol_data = data.table(n,MCsample)
View(sol_data)

estint=cumsum(MCsample)/(1:10^6)
esterr=sqrt(cumsum((MCsample-estint)^2))/(1:10^6)
plot(estint, xlab="Mean and error range",type="l",lwd=
       + 2,ylim=mean(MCsample)+20*c(-esterr[10^6],esterr[10^6]),ylab="", main="Graph of Estimated Integral with Confidence Interval", col="blue")
lines(estint+2*esterr,col="red",lwd=2)
lines(estint-2*esterr,col="red",lwd=2)

expected=1/(sqrt(1:10^6))

dfexpected=data.frame(expected)
dferr=data.frame(esterr)
dfMC=data.frame(estint)

dfMC[10^6,]
