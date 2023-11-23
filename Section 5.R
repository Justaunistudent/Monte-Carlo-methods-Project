install.packages("rmutil")

library(rmutil)

n=0
f=0
expected=0

h=function(x,y){(x^3+ 2*sin(y)/(1 + x))}

expectedf=int2(h, a=c(0,0), b=c(1,1))

for (i in 1:10^5){
  n[i]=i
  x1=runif(n)
  x2=runif(n)
  f[i]=(x1^3+ 2*sin(x2)/(1 + x1))}

df=data.frame(n,f)

estint=cumsum(f)/(1:10^5)
esterr=sqrt(cumsum((f-estint)^2))/(1:10^5)
plot(estint, xlab="Mean and error range",type="l",lwd=
       + 2,ylim=mean(f)+20*c(-esterr[10^5],esterr[10^5]),ylab="", main="Graph of Estimated Integral with Confidence Interval", col="blue")
lines(estint+2*esterr,col="red",lwd=2)
lines(estint-2*esterr,col="red",lwd=2)

expected=1/(sqrt(1:10^5))

dfMC=data.frame(estint)
dferr=data.frame(esterr)

dfMC[c(10^1,10^2,10^3,10^4,10^5),]
dferr[c(10^1,10^2,10^3,10^4,10^5),]
