#Example 1
maxcap = 100*10^3;
out_data = read.csv("out_youtube.csv",header=TRUE)
plot(out_data$t,out_data$y_t,ylim=c(0,maxcap),xlim=c(0,1600),
     ylab="Youtube Traffic (Kb)",xlab="Time (sec)",main="Youtube Traffic Data")
abline(h = maxcap,col="red",lwd=2)

y_t=out_data$y_t
t=out_data$t

#Example 2
maxcap=100
box_data = read.csv("out_box.csv",header=TRUE)
plot(box_data$t,box_data$y_t,ylim=c(-10,maxcap),xlim=c(0,400),
     ylab="Box Traffic (kb) ",xlab="Time (sec)",main="Box Traffic Data")
abline(h = maxcap,col="red",lwd=2)

y_t=box_data$y_t
t=box_data$t

#Example 3
y_t=c(1,2,3,4,5,'a',NaN,NA,'apple',1e3,'1e3',-3,-3,6,NA,NaN)
t=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)


#Example 4
testdata=read.csv("testdata.csv",header=TRUE)
y_t1=testdata$y_t1
y_t2=testdata$y_t2
y_t3=testdata$y_t3


t=testdata$t
y_t=y_t1



#Any clean part of function must pass the following example:
#y_t=c('1',1,'-1',-1,Inf,'Inf','c','你好',NA,NULL,NaN,'NA','NULL','NaN')

#Clean part:

y_t=as.numeric(y_t)
t=as.numeric(t)

clean_up=toupper(y_t)
clean_down=tolower(y_t)

clean_index=which(clean_up==clean_down & y_t>=0 & !is.na(y_t) & y_t<Inf)

y_t=y_t[clean_index]
t=t[clean_index]

y_t=as.numeric(y_t)
t=as.numeric(t)

#Plot the y_t if you want
#plot(y_t~t)

#Two special situations:

n=length(y_t)

#If the valid data length is 0, just give an alarm at the end of t, we regard the system is broken and nothing is recorded.
if(n==0){
  return(max(t))
}

#The following loop will do two things, find the maintenance time and find the cleanpoint. 

#Find the maintenance time, we regard maintenance time as an outliers of this problem. If y_t drop to a piece of low level and back to the level again. we regard it as maintenance time. Maintenance time is difficult for our prediction.

#If our program find maintenance time, it will remove it. They will be outliers in regression.

#Find the cleanpoint, cleanpoint is the point where y_t drop to a piece of low level from high_level and start to increase again, which is different from maintenance time. 

#The cleanpoint is very important for subsection, we regard subsection divided by cleanpoint as different process. 

#Our program will record every cleanpoint for further work.

#Every clean point will be saved in the following list
Cleanpoint=c()

#Every index of maintenance will be saved in the following list
MaintenanceIndex=c()

#Diffy_t calculate the difference from y_t[n] to y_t[n-1]
Diffy_t=diff(y_t)

#DiffPoint
DiffPoint=c()
m=length(Diffy_t)
for(i in 11:m-1){
  if(Diffy_t[i]<0 & abs(Diffy_t[i])>0.6*mean(y_t[max(i-8,1):max(i-1,1)]) & 0.8*abs(Diffy_t[i])<mean(y_t[max(i-8,1):max(i-1,1)])-mean(y_t[min(n,i+1):min(n,i+8)])){
    DiffPoint=c(DiffPoint,c(i,'Down'))
  }
  else if(Diffy_t[i]>0 & abs(Diffy_t[i])>0.6*mean(y_t[max(i-8,1):max(i-1,1)]) & 0.8*abs(Diffy_t[i])<mean(y_t[min(n,i+1):min(n,i+8)])-mean(y_t[max(i-8,1):max(i-1,1)])){
    DiffPoint=c(DiffPoint,c(i,'Up'))
  }
}
plot(Diffy_t)
DiffPoint

LR<-function(X,Y){
  clean = !is.na(Y) 
  Y = as.numeric(Y[clean]);
  X = as.numeric(X[clean])
  n=length(Y)
  Xmean=mean(X)
  Ymean=mean(Y)
  B=sum((X-Xmean)*(Y-Ymean))/sum((X-Xmean)^2)
  A=Ymean-B*Xmean
  FitY=A+B*X
  SST=sum((Y-Ymean)^2)
  SSR=sum((Y-FitY)^2)
  rsquare=1-SSR/SST
  return(c(B,A,rsquare))
}

#For the final piece of section, do the following calculation:
#piece=0

Halfy_t=y_t[round((n+piece)/2):n]
Halft=t[round((n+piece)/2):n]

HalfOUTPUT=LR(Halft,Halfy_t)

Wholey_t=y_t[piece:n]
Wholet=t[piece:n]

WholeOUTPUT=LR(Wholet,Wholey_t)

if(WholeOUTPUT[3]>HalfOUTPUT[3]){
  OUTPUT=WholeOUTPUT
}else{OUTPUT=HalfOUTPUT}

FinalSlope=OUTPUT[1]
FinalIntercept=OUTPUT[2]

deadtime=(maxcap-FinalIntercept)/FinalSlope-2
deadtime


