devtools::install_github('jucdiaz/ZOIP', force=TRUE)
library(ZOIP)
#Test 1--------------------------------------------------
data_bank<-read.csv(file.choose(),header=T)
dim(data_bank)
head(data_bank)
y_i<-data_bank[,1]

par(mfrow=c(1,2))

mu=0.4040 # Simplex: 0.5741
sigma=0.6601 # Sim: 4885.4370
p0=0.2219 # Sim: 0.1497
p1=0.0695 # Sim: 0.0090
family='R-S' # Simplex

y_sim<-rZOIP(10000,mu=mu,sigma=sigma,p0=p0,p1=p0,family=family)
min(y_sim)
max(y_sim)

plot(density(y_i,cut=0),xlab='Porcentaje de utilización tdc', main='Ajuste distribución \n ZOIP-Beta',ylim=c(0,4.5),las=2,ylab='Densidad')
lines(density(y_sim,cut=0),col='blue',lty=2)
legend("topright",bty='n', legend = c("Densidad Empírica", "Desidad Ajustada"),lty=1:2,col = c('black', 'blue'))

mu=0.5741
sigma=4885.4370
p0=0.1497
p1=0.0090
family='Simplex'

y_sim<-rZOIP(10000,mu=mu,sigma=sigma,p0=p0,p1=p0,family=family)
y_sim_simp<-y_sim
y_sim<-y_sim_simp
min(y_sim)
max(y_sim)
aux<-density(y_sim)
plot(density(y_i,cut=0),xlab='Porcentaje de utilización tdc', main='Ajuste distribución \n ZOIP-Simplex',ylim=c(0,4.5),las=2,ylab='Densidad')
lines(density(y_sim,cut=0),col='blue',lty=2)
legend("topright",bty='n', legend = c("Densidad Empírica", "Desidad Ajustada"),lty=1:2,col = c('black', 'blue'))

#################
