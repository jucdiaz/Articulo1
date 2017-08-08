devtools::install_github('jucdiaz/ZOIP', force=TRUE)
library(ZOIP)
#Test 1--------------------------------------------------
ns<-c(100,500,1000,10000)
family='R-S'
i=3
mu=NULL
sigma=NULL
p0=NULL
p1=NULL

while(i<=4){
n<-ns[i]
a<-0.6 # R-S, simplex y F-C 0.6, Original 14.4
b<-0.2 # R-S 0.2, simplex 20, F-C 24, Original 9.6
c<-0.03 # 0.03 igual para todas
d<-0.05 # 0.05 igual para todas
y_i<-rZOIP(n, mu = a, sigma = b, p0 = c, p1 = d,family=family)

plot(density(y_i))
data<-as.data.frame(y_i)
min(y_i)
max(y_i)
formula.mu=y_i~1
formula.sigma=~1
formula.p0=~1
formula.p1=~1
link=c('identity','identity','identity','identity')
family=family
mod<-RM.ZOIP(formula.mu=formula.mu,formula.sigma=formula.sigma,formula.p0=formula.p0,formula.p1=formula.p1,data=data,link=link,family=family)
summary(mod)
mu[i]<-mod[[1]][1]
sigma[i]<-mod[[1]][2]
p0[i]<-mod[[1]][3]
p1[i]<-mod[[1]][4]
i=i+1
}

#help(plotmath)
ns.factor<-as.factor(ns)
par(mfrow=c(2,2))
barplot(abs(mu-a),xlab='n',ylab=expression(paste('|',hat(mu)-mu,'|')),names.arg=ns.factor,col='azure3',main=expression(paste('Valor de ','|',hat(mu)-mu,'|',' variando n')))
barplot(abs(sigma-b),xlab='n',ylab=expression(paste('|',hat(sigma)-sigma,'|')),names.arg=ns.factor,col='azure3',main=expression(paste('Valor de ','|',hat(sigma)-sigma,'|',' variando n')))
barplot(abs(p0-c),xlab='n',ylab=expression(paste('|',hat(p0)-p0,'|')),names.arg=ns.factor,col='azure3',main=expression(paste('Valor de ','|',hat(p0)-p0,'|',' variando n')))
barplot(abs(p1-d),xlab='n',ylab=expression(paste('|',hat(p1)-p1,'|')),names.arg=ns.factor,col='azure3',main=expression(paste('Valor de ','|',hat(p1)-p1,'|',' variando n')))



# luego de correr los datos con p0!=0 y p=0


p0d0<-p0
p0.0<-c(0,0,0,0)

difp0<-rbind(p0d0,p0.0)
par(mfrow=c(1,1))
barplot(difp0,xlab='n',ylab=expression(paste('|',hat(p0)-p0,'|')),ylim=c(0,0.15),names.arg=ns.factor,col=c('azure3','bisque4')
        ,main=expression(paste('Diferencia entre ',p0==0,' y ', p0!=0,' variando n')),args.legend = list(x = "topright"),legend.text = c(expression(p0!=0),expression(p0==0)))

p1d0<-p1
p1.0<-p1

difp1<-rbind(p1d0,p1.0)
par(mfrow=c(1,1))
barplot(difp1,xlab='n',ylab=expression(paste('|',hat(p1)-p1,'|')),ylim=c(0,0.15),names.arg=ns.factor,col=c('azure3','bisque4')
        ,main=expression(paste('Diferencia entre ',p1==0,' y ', p1!=0,' variando n')),args.legend = list(x = "topright"),legend.text = c(expression(p1!=0),expression(p1==0)))


#-------------------------------------------------------
# --------------------Con covariables ----------------
?RM.ZOIP

#Test 1--------------------------------------------------
ns<-c(100,500,1000,10000)
family='Simplex'
i=1
mu=as.data.frame(matrix(ncol=2))
sigma=as.data.frame(matrix(ncol=3))
p0=as.data.frame(matrix(ncol=1))
p1=as.data.frame(matrix(ncol=2))

while(i<=4){
n<-ns[i]
x1<-runif(n)
x2<-runif(n)

c1<-0.2
c2<--1
mu_i<-inv.logit(c1+c2*x1)

b1<-0.3
b2<-3
b3<-0.9
sigma_i<-exp(b1+b2*x1+b3*x2)


d1<-0.07
p0_i<-rep(d1,1)

e1<-0.02
e2<--4
p1_i<-inv.logit(e1+e2*x2)

param<-cbind(mu_i,sigma_i,p0_i,p1_i)

system.time(y_i<-apply(param,1,function(x){rZOIP(1,mu=x[1],sigma=x[2],p0=x[3],p1=x[4],family=family)}))
data<-as.data.frame(cbind(y_i,x1,x2))

formula.mu=y_i~x1
formula.sigma=~x1+x2
formula.p0=~1
formula.p1=~x2
link=c('logit','exp','identity','logit')
system.time(mod<-RM.ZOIP(formula.mu=formula.mu,formula.sigma=formula.sigma,formula.p0=formula.p0,formula.p1=formula.p1,data=data,link=link,family=family))
summary(mod)
mu[i,]<-mod[[1]][c(1,2)]
sigma[i,]<-mod[[1]][c(3,4,5)]
p0[i,]<-mod[[1]][6]
p1[i,]<-mod[[1]][c(7,8)]
i=i+1
}

mu<-cbind(abs(mu[,1]-c1),abs(mu[,2]-c2))
sigma<-cbind(abs(sigma[,1]-b1),abs(sigma[,2]-b2),abs(sigma[,3]-b3))
p0<-cbind(abs(p0[,1]-d1))
p1<-cbind(abs(p1[,1]-e1),abs(p1[,2]-e2))

colnames(mu)<-names(mod[[1]])[c(1,2)]
colnames(sigma)<-names(mod[[1]])[c(3,4,5)]
colnames(p0)<-names(mod[[1]])[6]
colnames(p1)<-names(mod[[1]])[c(7,8)]
par(mfrow=c(1,1))

barplot(as.matrix(mu), beside=TRUE, legend.text = c('n=100','n=500','n=1000','n=10000')
        ,args.legend = list(x = "topleft"),ylim=c(0,2),ylab='Error absoluto', xlab='Parámetros',
        main=expression(paste('Error absoluto de ',beta,"'s para ", mu)),col=c('skyblue4','skyblue3','skyblue2','skyblue1'))

barplot(as.matrix(sigma), beside=TRUE, legend.text = c('n=100','n=500','n=1000','n=10000')
        ,args.legend = list(x = "topleft"),ylim=c(0,1.4),ylab='Error absoluto', xlab='Parámetros',
        main=expression(paste('Error absoluto de ',beta,"'s para ", sigma)),col=c('skyblue4','skyblue3','skyblue2','skyblue1'))

barplot(as.matrix(p0), beside=TRUE, legend.text = c('n=100','n=500','n=1000','n=10000')
        ,args.legend = list(x = "topleft"),ylim=c(0,0.1),ylab='Error absoluto', xlab='Parámetros',
        main=expression(paste('Error absoluto de ',beta,"'s para ", p0)),col=c('skyblue4','skyblue3','skyblue2','skyblue1'))

barplot(as.matrix(p1), beside=TRUE, legend.text = c('n=100','n=500','n=1000','n=10000')
        ,args.legend = list(x = "topleft"),ylim=c(0,4),ylab='Error absoluto', xlab='Parámetros',
        main=expression(paste('Error absoluto de ',beta,"'s para ", p1)),col=c('skyblue4','skyblue3','skyblue2','skyblue1'))

#-----------------------------------------------------------------------

library(ZOIP)
a<-0.6 # R-S, simplex y F-C 0.6, Original 14.4
b<-0.2 # R-S 0.2, simplex 20, F-C 24, Original 9.6
c<-0.03 # igual para todas
d<-0.05 # igual para todas


par(mfrow=c(2,2))
y_i<-rZOIP(10000, mu = 14.4, sigma = 9.6, p0 = 0.03, p1 = 0.05,family='Original')
plot(density(y_i),main='Distribución Beta Original'
     ,sub=expression(paste(p,'=14.4 ',q,'=9.6 ',p0,'=0.03 ',p1,'=0.05 ')),xlab='X',ylab='P(X=x)')

y_i<-rZOIP(10000, mu = 0.6, sigma = 0.2, p0 = 0.03, p1 = 0.05,family='R-S')
plot(density(y_i),main='Distribución Beta R-S'
,sub=expression(paste(mu,'=0.6 ',sigma,'=0.2 ',p0,'=0.03 ',p1,'=0.05 ')),xlab='X',ylab='P(X=x)')

y_i<-rZOIP(10000, mu = 0.6, sigma = 24, p0 = 0.03, p1 = 0.05,family='F-C')
plot(density(y_i),main='Distribución Beta F-C'
     ,sub=expression(paste(mu,'=0.6 ',phi,'=24 ',p0,'=0.03 ',p1,'=0.05 ')),xlab='X',ylab='P(X=x)')

y_i<-rZOIP(10000, mu = 0.6, sigma = 20, p0 = 0.03, p1 = 0.05,family='Simplex')
plot(density(y_i),main='Distribución Simplex'
     ,sub=expression(paste(mu,'=0.6 ',sigma,'=20 ',p0,'=0.03 ',p1,'=0.05 ')),xlab='X',ylab='P(X=x)')

