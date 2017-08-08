devtools::install_github('jucdiaz/ZOIP', force=TRUE)
library(ZOIP)
#Test 1--------------------------------------------------
data_bank<-read.csv(file.choose(),header=T)
dim(data_bank)
head(data_bank)
y_i<-data_bank[,1]

summary(y_i)
length(y_i)
par(mfrow=c(1,1))
hist(y_i, ylab='Frecuencia',xlab='Porcentaje de utilización de una tdc'
     ,main='Histograma del Porcentaje \n de utilización tdc',col='gray')
plot(density(y_i))
data<-as.data.frame(y_i)
min(y_i)
max(y_i)


family='Simplex'
formula.mu=y_i~1
formula.sigma=~1
formula.p0=~1
formula.p1=~1
link=c('identity','identity','identity','identity')
family=family
mod<-RM.ZOIP(formula.mu=formula.mu,formula.sigma=formula.sigma,formula.p0=formula.p0,formula.p1=formula.p1,data=data,link=link,family=family)
summary(mod)
mod
coef(mod)
mu<-mod[[1]][1]
sigma<-mod[[1]][2]
p0<-mod[[1]][3]
p1<-mod[[1]][4]
#prueba<-c(5.676749e-01,5.615646e+04,2.219202e-01,6.951985e-02)
#y_sim<-rZOIP(1000,mu=prueba[1],sigma=prueba[2],p0=prueba[3],p1=prueba[4],family=family)

y_sim<-rZOIP(10000,mu=mu,sigma=sigma,p0=p0,p1=p0,family=family)
min(y_sim)
max(y_sim)


par(mfrow=c(2,2))
plot(density(y_i),xlab='Porcentaje de utilizaci?n tdc', main='Ajuste distribuci?n \n ZOIP-Simplex',ylim=c(0,4.5),las=2,ylab='Probabilidad')
lines(density(y_sim),col='blue',lty=2)
legend("topright", legend = c("Densidad Emp?rica", "Desidad Ajustada"),
       lty=1:2,col = c('black', 'blue'))



temp <- legend("topright", legend = c(" ", " "),
               text.width = strwidth("Ajuste"),
               lty = 1:2, xjust = 1, yjust = 1,
               col=c(1,'blue'),bty='n')


text(temp$rect$left + temp$rect$w, temp$text$y,
     c("Densidad Emp?rica", "Desidad Ajustada"), pos = 2)



x <- 1:5; y1 <- 1/x; y2 <- 2/x
plot(rep(x, 2), c(y1, y2), type = "n", xlab = "x", ylab = "y")
lines(x, y1); lines(x, y2, lty = 2)
temp <- legend("topright", legend = c(" ", " "),
               text.width = strwidth("1,000,000"),
               lty = 1:2, xjust = 1, yjust = 1,
               title = "Line Types")
text(temp$rect$left + temp$rect$w, temp$text$y,
     c("1,000", "1,000,000"), pos = 2)


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


#Test 2--------------------------------------------------



head(data_bank)

y_i<-data_bank[,1]
data_bank$CUPO_TDC_ENTIDAD
data_bank$TOTAL_CARTERA
data_bank$SCORE

data<-as.data.frame(cbind(y_i,CUPO_TDC_ENTIDAD=data_bank$CUPO_TDC_ENTIDAD,
                          TOTAL_CARTERA=data_bank$TOTAL_CARTERA,PROM_Cuotas=data_bank$PROM_Cuotas))
data<-as.data.frame(cbind(y_i,SCORE=data_bank$SCORE/1000,PROM_Cuotas=data_bank$PROM_Cuotas
                          ,CUPO_TDC_ENTIDAD=log(data_bank$CUPO_TDC_ENTIDAD+1)))


head(data)
dim(data)
formula.mu=y_i~SCORE+PROM_Cuotas+CUPO_TDC_ENTIDAD # en simplex CUPO_TDC_ENTIDAD no va
formula.sigma=~SCORE+PROM_Cuotas+CUPO_TDC_ENTIDAD
formula.p0=~SCORE+CUPO_TDC_ENTIDAD
formula.p1=~1
link=c('exp','exp','logit','identity')
family='Original'
system.time(mod<-RM.ZOIP(formula.mu=formula.mu,formula.sigma=formula.sigma,formula.p0=formula.p0,formula.p1=formula.p1,data=data,link=link,family=family))
summary(mod)
mod

