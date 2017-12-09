devtools::install_github('jucdiaz/ZOIP', force=TRUE)
library(ZOIP)
# R-S, simplex y F-C 0.6, Original 14.4
# R-S 0.2, simplex 20, F-C 24, Original 9.6
# 0.03 igual para todas
# 0.05 igual para todas
y<-seq(0,1,0.0001)

?pZOIP

dZOIP(x=0.5, mu = 0.2, sigma = 0.5, p0 = 0.2, p1 =0.2,family='R-S',log = FALSE)
pZOIP(q=0.5, mu = 0.2, sigma = 3, p0 = 0, p1 = 0.2,family='F-C',log = FALSE)
qZOIP(p=0.7, mu = 0.6, sigma = 2.4, p0 = 0.2, p1 = 0,family='Original',log = FALSE)
set.seed(12345)
rZOIP(n=8, mu = 0.2, sigma = 3, p0 = 0.2, p1 = 0.2,family='Simplex')
#####Distribucion ZOIP###############
par(mfrow=c(2,2))

x<-dZOIP(y, mu = 0.6, sigma = 0.2, p0 = 0.3, p1 = 0.2,family='R-S')
plot(y,x,type='l',main=expression(paste('Distribución ZOIP \nbeta R-S ',mu,'=0.6 ',sigma,'=0.2 ',p[0],'=0.3 ',p[1],'=0.2 '))
     ,xlab='y',ylab='Densidad',las=1)

x<-dZOIP(y, mu = 0.6, sigma = 24, p0 = 0.3, p1 = 0.2,family='F-C')
plot(y,x,type='l',main=expression(paste('Distribución ZOIP \nbeta F-C ',mu,'=0.6 ',sigma,'=24 ',p[0],'=0.3 ',p[1],'=0.2 '))
     ,xlab='y',ylab='Densidad',las=1)

x<-dZOIP(y, mu = 14.4, sigma = 9.6, p0 = 0.3, p1 = 0.2,family='Original')
plot(y,x,type='l',main=expression(paste('Distribución ZOIP \nbeta Original ',mu,'=14.4 ',sigma,'=9.6 ',p[0],'=0.3 ',p[1],'=0.2 '))
     ,xlab='y',ylab='Densidad',las=1)

x<-dZOIP(y, mu = 0.4, sigma = 0.2, p0 = 0.3, p1 = 0.2,family='Simplex')
plot(y,x,type='l',main=expression(paste('Distribución ZOIP \nSimplex ',mu,'=0.6 ',sigma,'=24 ',p[0],'=0.3 ',p[1],'=0.2'))
     ,xlab='y',ylab='Densidad',las=1,ylim=c(0,4))

