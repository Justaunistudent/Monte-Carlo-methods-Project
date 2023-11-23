 h=function(x){(cos(10*x)+sin(70*x))^2}
 par(mar=c(2,2,2,1),mfrow=c(2,1))
 curve(h,xlab="Function",ylab="",lwd=2)
 integrate(h,0,1)
#0.965201 with absolute error < 1.9e-10
 x=h(runif(10^4))
 estint=cumsum(x)/(1:10^4)
 esterr=sqrt(cumsum((x-estint)^2))/(1:10^4)
 plot(estint, xlab="Mean and error range",type="l",lwd=
         + 2,ylim=mean(x)+20*c(-esterr[10^4],esterr[10^4]),ylab="", col="blue")
 lines(estint+2*esterr,col="red",lwd=2)
 lines(estint-2*esterr,col="red",lwd=2)
 
 expected=1/(sqrt(1:10^4))
 
View(table(expected))
View(table(esterr)) 
