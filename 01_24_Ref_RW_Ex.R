N<-10
p<-1/2
reps<-100
start<-0
X<-numeric(reps+1)
X[1] = 0
for(i in 1:reps){
  if(X[i]==0){
    X[i+1] =1
  }else if(X[i]==N){
    X[i+1] = N-1
  }else{
    X[i+1] = X[i] + sample(c(-1,1),size=1,prob=c(p,1-p))
  }
}
est_pi<-summary(as.factor(X))/(reps+1)
barplot(est_pi)
