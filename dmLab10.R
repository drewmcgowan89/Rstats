mat<-read.csv("trout.maturity.csv")
head(mat)       #Response is maturity
#Length is continuous predictor (in mm)
attach(mat)
levels(Sex:Species)  #Sex and Species are categorical predictors that define four groups

lenmod<-glm(Maturity~Length*Sex*Species, family=binomial(link="logit")) 
summary(lenmod)   

B0<-lenmod$coef[[1]]
B1<-lenmod$coef[[2]]
B2<-lenmod$coef[[3]]
B3<-lenmod$coef[[4]]
B4<-lenmod$coef[[5]]
B5<-lenmod$coef[[6]]
B6<-lenmod$coef[[7]]
B7<-lenmod$coef[[8]]
exp(B1)  

L<-100:600 #Dummy variable for plotting
plot(L,1/(1+exp(-(B0+B1*L))),type="l",ylab="Probability of maturity",xlab="Length (mm)")

lenLL<-function(t,dat){
  F<-subset(dat,Sex=="F")
  M<-subset(dat,Sex=="M")
  FYCT<-subset(F, Species=="YCT")
  MYCT<-subset(M, Species=="YCT")
  FRHT<-subset(F, Species=="RHT")
  MRHT<-subset(M, Species=="RHT")
  B0<-t[1]
  B1<-t[2]
  B2<-t[3]
  B3<-t[4]
  B4<-t[5]
  B5<-t[6]
  B6<-t[7]
  B7<-t[8]
  pi1<-1/(1+exp(-(B0+B1*FRHT$Length)))
  pi2<-1/(1+exp(-(B0+B2+(B1+B4)*MRHT$Length)))
  pi3<-1/(1+exp(-(B0+B3+(B1+B5)*FYCT$Length)))
  pi4<-1/(1+exp(-(B0+B6+(B1+B7)*MYCT$Length)))
  FRHTLL<-sum(dbinom(FRHT$Maturity,1,pi1,log=T))  #negative log(L) of observed maturity (0 or 1)
  MRHTLL<-sum(dbinom(MRHT$Maturity,1,pi2,log=T))
  FYCTLL<-sum(dbinom(FYCT$Maturity,1,pi3,log=T))  #negative log(L) of observed maturity (0 or 1)
  MYCTLL<-sum(dbinom(MYCT$Maturity,1,pi4,log=T))
  -(FYCTLL+MYCTLL+FRHTLL+MRHTLL)   #negative log(L) of observed maturity (0 or 1)
}

t0<-c(-13,0.03,4.71,-.9,-.01,.009,2.6,-.009)
lenmle<-optim(t0,lenLL,dat=mat,hessian=T)
lenmle$converge
sqrt(diag(solve(lenmle$hessian)))
lenmle$par

L<-100:600 #Dummy variable for plotting
L<-100:600 #Dummy variable for plotting
plot(L,1/(1+exp(-(B0+B1*L))),type="l",ylab="Probability of maturity",xlab="Length (mm)",lty=1)
  lines(L,1/(1+exp(-(B0+B2+(B1+B4)*L))),lty=2)
  lines(L,1/(1+exp(-(B0+B3+(B1+B5)*L))),lty=3)
  lines(L,1/(1+exp(-(B0+B6+(B1+B7)*L))),lty=4)
legend("topleft",c("RHT Females","RHT Males", "YCT Females","YCT Males"),lty=1:4)


