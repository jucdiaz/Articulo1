devtools::install_github('jucdiaz/ZOIP', force=TRUE)
library(ZOIP)
dZOIP
#Caso 1--------------------------------------------------
n<-seq(5,500,by=5)
nrep=1000
family='Simplex'
a<-0.6 # R-S y F-C 0.6, Original 14.4, simplex 0.4
b<-20 # R-S 0.2, simplex 0.2, F-C 24, Original 9.6
c<-0.03 # 0.03 igual para todas
d<-0.05 # 0.05 igual para todas


j=1
mu=NULL
sigma=NULL
p0=NULL
p1=NULL


while(j<=length(n)){
  i=1
  while(i<=nrep){
    try({
    y_i<-rZOIP(n[j], mu = a, sigma = b, p0 = c, p1 = d,family=family)
    
    data<-as.data.frame(y_i)
    link=c('identity','identity','identity','identity')
    
    mod<-RM.ZOIP(formula.mu=y_i~1,formula.sigma=~1,formula.p0=~1,formula.p1=~1,data=data,link=link,family=family)
    mu[i]<-coef(mod)[,1]
    sigma[i]<-coef(mod)[,2]
    p0[i]<-coef(mod)[,3]
    p1[i]<-coef(mod)[,4]
    i=i+1
     },silent=TRUE)
  }

data_i<-as.data.frame(cbind(tamaño_muestra=n[j],mu_est=mu,sigma_est=sigma,p0_est=p0,p1_est=p1,mu_verd=a,sigma_verd=b,p0_verd=c,p1_verd=d))
write.table(data_i,file='D:\\datos_sim_simplex.csv',append=TRUE,sep = ",",col.names = FALSE, row.names = FALSE)
j=j+1
}
i
j


#-----Caso1 R-S #---------------------------------------------------------
datos_RS<-read.table(file.choose(),header=F,sep=',')
colnames(datos_RS)<-c('Tamanio_m','mu_est','sigma_est','p0_est','p1_est'
                      ,'mu','sigma','p0','p1')
head(datos_RS)

datos_RS$Error_mu<-datos_RS$mu_est-datos_RS$mu
datos_RS$Error_abs_mu<-abs(datos_RS$mu_est-datos_RS$mu)
datos_RS$Error_cua_mu<-(datos_RS$mu_est-datos_RS$mu)^2

datos_RS$Error_sigma<-datos_RS$sigma_est-datos_RS$sigma
datos_RS$Error_abs_sigma<-abs(datos_RS$sigma_est-datos_RS$sigma)
datos_RS$Error_cua_sigma<-(datos_RS$sigma_est-datos_RS$sigma)^2

datos_RS$Error_p0<-datos_RS$p0_est-datos_RS$p0
datos_RS$Error_abs_p0<-abs(datos_RS$p0_est-datos_RS$p0)
datos_RS$Error_cua_p0<-(datos_RS$p0_est-datos_RS$p0)^2

datos_RS$Error_p1<-datos_RS$p1_est-datos_RS$p1
datos_RS$Error_abs_p1<-abs(datos_RS$p1_est-datos_RS$p1)
datos_RS$Error_cua_p1<-(datos_RS$p1_est-datos_RS$p1)^2

datos_RS$Error_porc_mu<-abs(datos_RS$mu_est-datos_RS$mu)/datos_RS$mu
datos_RS$Error_porc_sigma<-abs(datos_RS$sigma_est-datos_RS$sigma)/datos_RS$sigma
datos_RS$Error_porc_p0<-abs(datos_RS$p0_est-datos_RS$p0)/datos_RS$p0
datos_RS$Error_porc_p1<-abs(datos_RS$p1_est-datos_RS$p1)/datos_RS$p1

head(datos_RS)

median_mu<-tapply(datos_RS$mu_est,datos_RS$Tamanio_m,median)
median_sigma<-tapply(datos_RS$sigma_est,datos_RS$Tamanio_m,median)
median_p0<-tapply(datos_RS$p0_est,datos_RS$Tamanio_m,median)
median_p1<-tapply(datos_RS$p1_est,datos_RS$Tamanio_m,median)

#quan_0.1_mu<-tapply(datos_RS$mu_est,datos_RS$Tamanio_m,function(x)quantile(x,probs=0.1))
#quan_0.9_mu<-tapply(datos_RS$mu_est,datos_RS$Tamanio_m,function(x)quantile(x,probs=0.9))


par(mfrow=c(2,4),mar=c(5, 5, 4, 2))
title('Tiulo')
plot(names(median_mu),median_mu,type='l'
     ,xlab='Tamaño muestra', ylab=expression(paste('mediana ',hat(mu))),main='Distribución \n ZOIP beta R-S')
#lines(names(quan_0.1_mu),quan_0.1_mu,lty=3,col='blue')
#lines(names(quan_0.1_mu),quan_0.9_mu,lty=3,col='blue')
abline(h=datos_RS$mu[1],col='red',lty=2)
#par(new=T)
plot(names(median_sigma),median_sigma,type='l'
     ,xlab='Tamaño muestra', ylab=expression(paste('mediana ',hat(sigma))),main='Distribución \n ZOIP beta R-S')
abline(h=datos_RS$sigma[1],col='red',lty=2)
#par(new=T)
plot(names(median_p0),median_p0,type='l'
     ,xlab='Tamaño muestra', ylab=expression(paste('mediana ',hat(p0))),main='Distribución \n ZOIP beta R-S')
abline(h=datos_RS$p0[1],col='red',lty=2)
#par(new=T)
plot(names(median_p1),median_p1,type='l'
     ,xlab='Tamaño muestra', ylab=expression(paste('mediana ',hat(p1))),main='Distribución \n ZOIP beta R-S',ylim=c(0.185,0.215))
abline(h=datos_RS$p1[1],col='red',lty=2)


error_mu<-tapply(datos_RS$Error_mu,datos_RS$Tamanio_m,median)
error_sigma<-tapply(datos_RS$Error_sigma,datos_RS$Tamanio_m,median)
error_p0<-tapply(datos_RS$Error_p0,datos_RS$Tamanio_m,median)
error_p1<-tapply(datos_RS$Error_p1,datos_RS$Tamanio_m,median)

plot(names(error_mu),error_mu,type='l'
     ,xlab='Tamaño muestra', ylab=expression(paste('median ',hat(tetha_i))),ylim=c(-0.01,0.025))
lines(names(error_sigma),error_sigma,col='chartreuse4')
lines(names(error_p0),error_p0,col='dodgerblue2')
lines(names(error_p1),error_p1,col='darkorange3')
abline(h=0,col='red',lty=2)
legend("topright", legend = c("error mu", "error sigma", "error p0","error p1" ),
       lty=1,col = c('black', 'chartreuse4', 'dodgerblue2', 'darkorange3'))

#-----Caso2 F-C #---------------------------------------------------------
datos_FC<-read.table(file.choose(),header=F,sep=',')
colnames(datos_FC)<-c('Tamanio_m','mu_est','sigma_est','p0_est','p1_est'
                      ,'mu','sigma','p0','p1')
head(datos_FC)
dim(datos_FC)

datos_FC$Error_mu<-datos_FC$mu_est-datos_FC$mu
datos_FC$Error_abs_mu<-abs(datos_FC$mu_est-datos_FC$mu)
datos_FC$Error_cua_mu<-(datos_FC$mu_est-datos_FC$mu)^2

datos_FC$Error_sigma<-datos_FC$sigma_est-datos_FC$sigma
datos_FC$Error_abs_sigma<-abs(datos_FC$sigma_est-datos_FC$sigma)
datos_FC$Error_cua_sigma<-(datos_FC$sigma_est-datos_FC$sigma)^2

datos_FC$Error_p0<-datos_FC$p0_est-datos_FC$p0
datos_FC$Error_abs_p0<-abs(datos_FC$p0_est-datos_FC$p0)
datos_FC$Error_cua_p0<-(datos_FC$p0_est-datos_FC$p0)^2

datos_FC$Error_p1<-datos_FC$p1_est-datos_FC$p1
datos_FC$Error_abs_p1<-abs(datos_FC$p1_est-datos_FC$p1)
datos_FC$Error_cua_p1<-(datos_FC$p1_est-datos_FC$p1)^2

datos_FC$Error_porc_mu<-abs(datos_FC$mu_est-datos_FC$mu)/datos_FC$mu
datos_FC$Error_porc_sigma<-abs(datos_FC$sigma_est-datos_FC$sigma)/datos_FC$sigma
datos_FC$Error_porc_p0<-abs(datos_FC$p0_est-datos_FC$p0)/datos_FC$p0
datos_FC$Error_porc_p1<-abs(datos_FC$p1_est-datos_FC$p1)/datos_FC$p1

head(datos_FC)

median_mu<-tapply(datos_FC$mu_est,datos_FC$Tamanio_m,median)
median_sigma<-tapply(datos_FC$sigma_est,datos_FC$Tamanio_m,median)
median_p0<-tapply(datos_FC$p0_est,datos_FC$Tamanio_m,median)
median_p1<-tapply(datos_FC$p1_est,datos_FC$Tamanio_m,median)

par(mfrow=c(2,2))

plot(names(median_mu),median_mu,type='l'
     ,xlab='Tamaño muestra', ylab=expression(paste('mediana ',hat(mu))),main='Distribución \n ZOIP beta F-C')
abline(h=datos_FC$mu[1],col='red',lty=2)
#par(new=T)
plot(names(median_sigma),median_sigma,type='l'
     ,xlab='Tamaño muestra', ylab=expression(paste('mediana ',hat(sigma))),main='Distribución \n ZOIP beta F-C')
abline(h=datos_FC$sigma[1],col='red',lty=2)
#par(new=T)
plot(names(median_p0),median_p0,type='l'
     ,xlab='Tamaño muestra', ylab=expression(paste('mediana ',hat(p0))),main='Distribución \n ZOIP beta F-C')
abline(h=datos_FC$p0[1],col='red',lty=2)
#par(new=T)
plot(names(median_p1),median_p1,type='l'
     ,xlab='Tamaño muestra', ylab=expression(paste('mediana ',hat(p1))),main='Distribución \n ZOIP beta F-C',ylim=c(0.185,0.215))
abline(h=datos_FC$p1[1],col='red',lty=2)

error_mu<-tapply(datos_FC$Error_mu,datos_FC$Tamanio_m,median)
error_sigma<-tapply(datos_FC$Error_sigma,datos_FC$Tamanio_m,median)
error_p0<-tapply(datos_FC$Error_p0,datos_FC$Tamanio_m,median)
error_p1<-tapply(datos_FC$Error_p1,datos_FC$Tamanio_m,median)

plot(names(error_mu),error_mu,type='l'
     ,xlab='Tamaño muestra', ylab=expression(paste('median ',hat(tetha_i))),ylim=c(-0.01,0.025))
lines(names(error_sigma),error_sigma,col='chartreuse4')
lines(names(error_p0),error_p0,col='dodgerblue2')
lines(names(error_p1),error_p1,col='darkorange3')
abline(h=0,col='red',lty=2)
legend("topright", legend = c("error mu", "error sigma", "error p0","error p1" ),
       lty=1,col = c('black', 'chartreuse4', 'dodgerblue2', 'darkorange3'))

#-----Caso3 Ori #---------------------------------------------------------
datos_Ori<-read.table(file.choose(),header=F,sep=',')
colnames(datos_Ori)<-c('Tamanio_m','mu_est','sigma_est','p0_est','p1_est'
                       ,'mu','sigma','p0','p1')
head(datos_Ori)
dim(datos_Ori)

datos_Ori$Error_mu<-datos_Ori$mu_est-datos_Ori$mu
datos_Ori$Error_abs_mu<-abs(datos_Ori$mu_est-datos_Ori$mu)
datos_Ori$Error_cua_mu<-(datos_Ori$mu_est-datos_Ori$mu)^2

datos_Ori$Error_sigma<-datos_Ori$sigma_est-datos_Ori$sigma
datos_Ori$Error_abs_sigma<-abs(datos_Ori$sigma_est-datos_Ori$sigma)
datos_Ori$Error_cua_sigma<-(datos_Ori$sigma_est-datos_Ori$sigma)^2

datos_Ori$Error_p0<-datos_Ori$p0_est-datos_Ori$p0
datos_Ori$Error_abs_p0<-abs(datos_Ori$p0_est-datos_Ori$p0)
datos_Ori$Error_cua_p0<-(datos_Ori$p0_est-datos_Ori$p0)^2

datos_Ori$Error_p1<-datos_Ori$p1_est-datos_Ori$p1
datos_Ori$Error_abs_p1<-abs(datos_Ori$p1_est-datos_Ori$p1)
datos_Ori$Error_cua_p1<-(datos_Ori$p1_est-datos_Ori$p1)^2

datos_Ori$Error_porc_mu<-abs(datos_Ori$mu_est-datos_Ori$mu)/datos_Ori$mu
datos_Ori$Error_porc_sigma<-abs(datos_Ori$sigma_est-datos_Ori$sigma)/datos_Ori$sigma
datos_Ori$Error_porc_p0<-abs(datos_Ori$p0_est-datos_Ori$p0)/datos_Ori$p0
datos_Ori$Error_porc_p1<-abs(datos_Ori$p1_est-datos_Ori$p1)/datos_Ori$p1

head(datos_Ori)

median_mu<-tapply(datos_Ori$mu_est,datos_Ori$Tamanio_m,median)
median_sigma<-tapply(datos_Ori$sigma_est,datos_Ori$Tamanio_m,median)
median_p0<-tapply(datos_Ori$p0_est,datos_Ori$Tamanio_m,median)
median_p1<-tapply(datos_Ori$p1_est,datos_Ori$Tamanio_m,median)

par(mfrow=c(2,4),mar=c(5, 5, 4, 2))

plot(names(median_mu),median_mu,type='l'
     ,xlab='Tamaño muestra', ylab=expression(paste('mediana ',hat(mu))),main='Distribución \n ZOIP beta Original')
abline(h=datos_Ori$mu[1],col='red',lty=2)
#par(new=T)
plot(names(median_sigma),median_sigma,type='l'
     ,xlab='Tamaño muestra', ylab=expression(paste('mediana ',hat(sigma))),main='Distribución \n ZOIP beta Original')
abline(h=datos_Ori$sigma[1],col='red',lty=2)
#par(new=T)
plot(names(median_p0),median_p0,type='l'
     ,xlab='Tamaño muestra', ylab=expression(paste('mediana ',hat(p0))),main='Distribución \n ZOIP beta Original')
abline(h=datos_Ori$p0[1],col='red',lty=2)
#par(new=T)
plot(names(median_p1),median_p1,type='l'
     ,xlab='Tamaño muestra', ylab=expression(paste('mediana ',hat(p1))),main='Distribución \n ZOIP beta Original',ylim=c(0.185,0.215))
abline(h=datos_Ori$p1[1],col='red',lty=2)

error_mu<-tapply(datos_Ori$Error_mu,datos_Ori$Tamanio_m,median)
error_sigma<-tapply(datos_Ori$Error_sigma,datos_Ori$Tamanio_m,median)
error_p0<-tapply(datos_Ori$Error_p0,datos_Ori$Tamanio_m,median)
error_p1<-tapply(datos_Ori$Error_p1,datos_Ori$Tamanio_m,median)

plot(names(error_mu),error_mu,type='l'
     ,xlab='Tamaño muestra', ylab=expression(paste('median ',hat(tetha_i))),ylim=c(-0.01,0.025))
lines(names(error_sigma),error_sigma,col='chartreuse4')
lines(names(error_p0),error_p0,col='dodgerblue2')
lines(names(error_p1),error_p1,col='darkorange3')
abline(h=0,col='red',lty=2)
legend("topright", legend = c("error mu", "error sigma", "error p0","error p1" ),
       lty=1,col = c('black', 'chartreuse4', 'dodgerblue2', 'darkorange3'))

#-----Caso4 sim #---------------------------------------------------------
datos_Sim<-read.table(file.choose(),header=F,sep=',')
colnames(datos_Sim)<-c('Tamanio_m','mu_est','sigma_est','p0_est','p1_est'
                       ,'mu','sigma','p0','p1')
head(datos_Sim)
dim(datos_Sim)

datos_Sim$Error_mu<-datos_Sim$mu_est-datos_Sim$mu
datos_Sim$Error_abs_mu<-abs(datos_Sim$mu_est-datos_Sim$mu)
datos_Sim$Error_cua_mu<-(datos_Sim$mu_est-datos_Sim$mu)^2

datos_Sim$Error_sigma<-datos_Sim$sigma_est-datos_Sim$sigma
datos_Sim$Error_abs_sigma<-abs(datos_Sim$sigma_est-datos_Sim$sigma)
datos_Sim$Error_cua_sigma<-(datos_Sim$sigma_est-datos_Sim$sigma)^2

datos_Sim$Error_p0<-datos_Sim$p0_est-datos_Sim$p0
datos_Sim$Error_abs_p0<-abs(datos_Sim$p0_est-datos_Sim$p0)
datos_Sim$Error_cua_p0<-(datos_Sim$p0_est-datos_Sim$p0)^2

datos_Sim$Error_p1<-datos_Sim$p1_est-datos_Sim$p1
datos_Sim$Error_abs_p1<-abs(datos_Sim$p1_est-datos_Sim$p1)
datos_Sim$Error_cua_p1<-(datos_Sim$p1_est-datos_Sim$p1)^2

datos_Sim$Error_porc_mu<-abs(datos_Sim$mu_est-datos_Sim$mu)/datos_Sim$mu
datos_Sim$Error_porc_sigma<-abs(datos_Sim$sigma_est-datos_Sim$sigma)/datos_Sim$sigma
datos_Sim$Error_porc_p0<-abs(datos_Sim$p0_est-datos_Sim$p0)/datos_Sim$p0
datos_Sim$Error_porc_p1<-abs(datos_Sim$p1_est-datos_Sim$p1)/datos_Sim$p1


head(datos_Sim)

median_mu<-tapply(datos_Sim$mu_est,datos_Sim$Tamanio_m,mean)
median_sigma<-tapply(datos_Sim$sigma_est,datos_Sim$Tamanio_m,median)
median_p0<-tapply(datos_Sim$p0_est,datos_Sim$Tamanio_m,median)
median_p1<-tapply(datos_Sim$p1_est,datos_Sim$Tamanio_m,median)



par(mfrow=c(1,1))

plot(names(median_mu),median_mu,type='l'
     ,xlab='Tamaño muestra', ylab=expression(paste('mediana ',hat(mu))),main='Distribución \n ZOIP Simplex')
abline(h=datos_Sim$mu[1],col='red',lty=2)
#par(new=T)

plot(names(median_sigma),median_sigma,type='l'
     ,xlab='Tamaño muestra', ylab=expression(paste('mediana ',hat(sigma))),main='Distribución \n ZOIP Simplex')
abline(h=datos_Sim$sigma[1],col='red',lty=2)
#par(new=T)


plot(names(median_p0),median_p0,type='l'
     ,xlab='Tamaño muestra', ylab=expression(paste('mediana ',hat(p0))),main='Distribución \n ZOIP Simplex')
abline(h=datos_Sim$p0[1],col='red',lty=2)
#par(new=T)

plot(names(median_p1),median_p1,type='l'
     ,xlab='Tamaño muestra', ylab=expression(paste('mediana ',hat(p1))),main='Distribución \n ZOIP Simplex',ylim=c(0.185,0.215))
abline(h=datos_Sim$p1[1],col='red',lty=2)

error_mu<-tapply(datos_Sim$Error_mu,datos_Sim$Tamanio_m,median)
error_sigma<-tapply(datos_Sim$Error_sigma,datos_Sim$Tamanio_m,median)
error_p0<-tapply(datos_Sim$Error_p0,datos_Sim$Tamanio_m,median)
error_p1<-tapply(datos_Sim$Error_p1,datos_Sim$Tamanio_m,median)

plot(names(error_mu),error_mu,type='l'
     ,xlab='Tamaño muestra', ylab=expression(paste('median ',hat(tetha_i))),ylim=c(-0.01,0.01))
lines(names(error_sigma),error_sigma,col='chartreuse4')
lines(names(error_p0),error_p0,col='dodgerblue2')
lines(names(error_p1),error_p1,col='darkorange3')
abline(h=0,col='red',lty=2)
legend("topright", legend = c("error mu", "error sigma", "error p0","error p1" ),
       lty=1,col = c('black', 'chartreuse4', 'dodgerblue2', 'darkorange3'))

##################ERORES EN TODOS LOS CASOS MAPE########################################
par(mfrow=c(1,1))

error_mu_C1<-tapply(datos_RS$Error_porc_mu,datos_RS$Tamanio_m,mean)
error_mu_C2<-tapply(datos_FC$Error_porc_mu,datos_FC$Tamanio_m,mean)
error_mu_C3<-tapply(datos_Ori$Error_porc_mu,datos_Ori$Tamanio_m,mean)
error_mu_C4<-tapply(datos_Sim$Error_porc_mu,datos_Sim$Tamanio_m,mean)

error_mu_C1[100]
error_mu_C2[100]
error_mu_C3[100]
error_mu_C4[100]

error_sigma_C1<-tapply(datos_RS$Error_porc_sigma,datos_RS$Tamanio_m,mean)
error_sigma_C2<-tapply(datos_FC$Error_porc_sigma,datos_FC$Tamanio_m,mean)
error_sigma_C3<-tapply(datos_Ori$Error_porc_sigma,datos_Ori$Tamanio_m,mean)
error_sigma_C4<-tapply(datos_Sim$Error_porc_sigma,datos_Sim$Tamanio_m,mean)

error_sigma_C1[100]
error_sigma_C2[100]
error_sigma_C3[100]
error_sigma_C4[100]

error_p0_C1<-tapply(datos_RS$Error_porc_p0,datos_RS$Tamanio_m,mean)
error_p0_C2<-tapply(datos_FC$Error_porc_p0,datos_FC$Tamanio_m,mean)
error_p0_C3<-tapply(datos_Ori$Error_porc_p0,datos_Ori$Tamanio_m,mean)
error_p0_C4<-tapply(datos_Sim$Error_porc_p0,datos_Sim$Tamanio_m,mean)

error_p0_C1[100]
error_p0_C2[100]
error_p0_C3[100]
error_p0_C4[100]

error_p1_C1<-tapply(datos_RS$Error_porc_p1,datos_RS$Tamanio_m,mean)
error_p1_C2<-tapply(datos_FC$Error_porc_p1,datos_FC$Tamanio_m,mean)
error_p1_C3<-tapply(datos_Ori$Error_porc_p1,datos_Ori$Tamanio_m,mean)
error_p1_C4<-tapply(datos_Sim$Error_porc_p1,datos_Sim$Tamanio_m,mean)

error_p1_C1[100]
error_p1_C2[100]
error_p1_C3[100]
error_p1_C4[100]


plot(names(error_mu_C1),error_mu_C1,type='l'
     ,xlab='Tamaño muestra', ylab='Mape',ylim=c(0,0.3))
lines(names(error_mu_C2),error_mu_C2,col='chartreuse4')
lines(names(error_mu_C3),error_mu_C3,col='dodgerblue2')
lines(names(error_mu_C4),error_mu_C4,col='darkorange3')
abline(h=0,col='red',lty=2)
legend("topright", legend = c(expression(paste('MAPE ',mu,' Caso1')), expression(paste('MAPE ',mu,' Caso2')), expression(paste('MAPE ',mu,' Caso3')),expression(paste('MAPE ',mu,' Caso4'))),
       lty=1,col = c('black', 'chartreuse4', 'dodgerblue2', 'darkorange3'))

plot(names(error_sigma_C1),error_sigma_C1,type='l'
     ,xlab='Tamaño muestra', ylab='MAPE',ylim=c(0,0.4))
lines(names(error_sigma_C2),error_sigma_C2,col='chartreuse4')
lines(names(error_sigma_C3),error_sigma_C3,col='dodgerblue2')
lines(names(error_sigma_C4),error_sigma_C4,col='darkorange3')
abline(h=0,col='red',lty=2)
legend("topright", legend = c(expression(paste('MAPE ',sigma,' Caso1')), expression(paste('MAPE ',sigma,' Caso2')), expression(paste('MAPE ',sigma,' Caso3')),expression(paste('MAPE ',sigma,' Caso4'))),
       lty=1,col = c('black', 'chartreuse4', 'dodgerblue2', 'darkorange3'))

plot(names(error_p0_C1),error_p0_C1,type='l'
     ,xlab='Tamaño muestra', ylab='MAPE',ylim=c(0,1))
lines(names(error_p0_C2),error_p0_C2,col='chartreuse4')
lines(names(error_p0_C3),error_p0_C3,col='dodgerblue2')
lines(names(error_p0_C4),error_p0_C4,col='darkorange3')
abline(h=0,col='red',lty=2)
legend("topright", legend = c("MAPE p0 Caso1", "MAPE p0 Caso2", "MAPE p0 Caso3","MAPE p0 Caso4" ),
       lty=1,col = c('black', 'chartreuse4', 'dodgerblue2', 'darkorange3'))

plot(names(error_p1_C1),error_p1_C1,type='l'
     ,xlab='Tamaño muestra', ylab='MAPE',ylim=c(0,1))
lines(names(error_p1_C2),error_p1_C2,col='chartreuse4')
lines(names(error_p1_C3),error_p1_C3,col='dodgerblue2')
lines(names(error_p1_C4),error_p1_C4,col='darkorange3')
abline(h=0,col='red',lty=2)
legend("topright", legend = c("MAPE p1 Caso1", "MAPE p1 Caso2", "MAPE p1 Caso3","MAPE p1 Caso4" ),
       lty=1,col = c('black', 'chartreuse4', 'dodgerblue2', 'darkorange3'))

error_C1<-apply(cbind(error_mu_C1,error_sigma_C1,error_p0_C1,error_p1_C1),1,mean)
error_C2<-apply(cbind(error_mu_C2,error_sigma_C2,error_p0_C2,error_p1_C2),1,mean)
error_C3<-apply(cbind(error_mu_C3,error_sigma_C3,error_p0_C3,error_p1_C3),1,mean)
error_C4<-apply(cbind(error_mu_C4,error_sigma_C4,error_p0_C4,error_p1_C4),1,mean)

mean(c(error_C1[100],error_C2[100],error_C3[100],error_C4[100]))


plot(names(error_C1),error_C1,type='l'
     ,xlab='Tamaño muestra', ylab='MAPE',ylim=c(0,1.1),las=1,main='Estudio de simulación del escenario 2')
lines(names(error_C2),error_C2,col='chartreuse4')
lines(names(error_C3),error_C3,col='dodgerblue2')
lines(names(error_C4),error_C4,col='darkorange3')
#abline(h=0,col='red',lty=2)
legend("topright", legend = c("MAPE Caso R-S", "MAPE Caso F-C", "MAPE Caso original","MAPE Caso simplex" ),
       lty=1,col = c('black', 'chartreuse4', 'dodgerblue2', 'darkorange3'),bty='n')

#########################RECM########################################################
error_mu_C1<-sqrt(tapply(datos_RS$Error_cua_mu,datos_RS$Tamanio_m,mean))
error_mu_C2<-sqrt(tapply(datos_FC$Error_cua_mu,datos_FC$Tamanio_m,mean))
error_mu_C3<-sqrt(tapply(datos_Ori$Error_cua_mu,datos_Ori$Tamanio_m,mean))
error_mu_C4<-sqrt(tapply(datos_Sim$Error_cua_mu,datos_Sim$Tamanio_m,mean))

error_sigma_C1<-sqrt(tapply(datos_RS$Error_cua_sigma,datos_RS$Tamanio_m,mean))
error_sigma_C2<-sqrt(tapply(datos_FC$Error_cua_sigma,datos_FC$Tamanio_m,mean))
error_sigma_C3<-sqrt(tapply(datos_Ori$Error_cua_sigma,datos_Ori$Tamanio_m,mean))
error_sigma_C4<-sqrt(tapply(datos_Sim$Error_cua_sigma,datos_Sim$Tamanio_m,mean))

error_p0_C1<-sqrt(tapply(datos_RS$Error_cua_p0,datos_RS$Tamanio_m,mean))
error_p0_C2<-sqrt(tapply(datos_FC$Error_cua_p0,datos_FC$Tamanio_m,mean))
error_p0_C3<-sqrt(tapply(datos_Ori$Error_cua_p0,datos_Ori$Tamanio_m,mean))
error_p0_C4<-sqrt(tapply(datos_Sim$Error_cua_p0,datos_Sim$Tamanio_m,mean))

error_p1_C1<-sqrt(tapply(datos_RS$Error_cua_p1,datos_RS$Tamanio_m,mean))
error_p1_C2<-sqrt(tapply(datos_FC$Error_cua_p1,datos_FC$Tamanio_m,mean))
error_p1_C3<-sqrt(tapply(datos_Ori$Error_cua_p1,datos_Ori$Tamanio_m,mean))
error_p1_C4<-sqrt(tapply(datos_Sim$Error_cua_p1,datos_Sim$Tamanio_m,mean))


error_C1<-apply(cbind(error_mu_C1,error_sigma_C1,error_p0_C1,error_p1_C1),1,mean)
error_C2<-apply(cbind(error_mu_C2,error_sigma_C2,error_p0_C2,error_p1_C2),1,mean)
error_C3<-apply(cbind(error_mu_C3,error_sigma_C3,error_p0_C3,error_p1_C3),1,mean)
error_C4<-apply(cbind(error_mu_C4,error_sigma_C4,error_p0_C4,error_p1_C4),1,mean)

plot(names(error_C1),error_C1,type='l'
     ,xlab='Tamaño p1estra', ylab='RECM',ylim=c(0,2.5))
lines(names(error_C2),error_C2,col='chartreuse4')
lines(names(error_C3),error_C3,col='dodgerblue2')
lines(names(error_C4),error_C4,col='darkorange3')
abline(h=0,col='red',lty=2)
legend("topright", legend = c("RECM Caso1", "RECM Caso2", "RECM Caso3","RECM Caso4" ),
       lty=1,col = c('black', 'chartreuse4', 'dodgerblue2', 'darkorange3'))

