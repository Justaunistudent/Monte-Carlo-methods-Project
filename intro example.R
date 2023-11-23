n=0
h=0
m=0
a=0

for (i in 1:10){
  n[i]=10^i
  
  x=runif(n[i])
  
  h[i]=(1/n[i])*sum(x)
  m[i]=sum(x)
  b=1/n
  a[i]=(1/n[i])*sum((x-0.5)^2)}

h_df=data.frame(h,m)

n_df=data.frame(a)

