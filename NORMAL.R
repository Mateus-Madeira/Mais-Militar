plot(1,1,ylim=c(0,1),
     xlim=c(0,1),pch=NA)
polygon(x=c(0,0.6,1),
        col='blue')

#x=sample(seq(-3,3,0.01)) #amaranhado
x=seq(-3,3,0.01)
y=dnorm(x)
plot(x,y,type="l")
x0=1
yo=dnorm(x0)
polygon(x=c(x[1],x[x<x0],x0),
        y=c(0,y[x<x0],0),
        col = "gray",
        border=NA)

lines(x,y,type="l")
text(2,0.335,expression(P(Z<z[0])))
text(2,0.3,round(pnorm(x0),4))