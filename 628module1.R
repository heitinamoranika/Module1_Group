Capacity_Deadtime<-function(y_t,t,maxcap){
  
  #Clean part:
  #Any clean part of function must pass the following example:
  #y_t=c('1',1,'-1',-1,Inf,'Inf','c','你好',NA,NULL,NaN,'NA','NULL','NaN')
  
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
  Maxy_t=max(y_t)
  EndTime=max(t)
  n=length(y_t)
  
  #Two special situations:
  #If the valid data length is less than 1, just give an alarm at the end of t, we regard the system is broken and nothing is recorded.
  #If the y_t in the end of time is larger than 98% of maxcap, just give an alarm at the end of t, we regard the system will broken soon.
  
  if(n<=1){
    return(EndTime)
  }
  if(n>12){
    if(mean(y_t[min((n-20),n):n])>0.95*maxcap){
      return(EndTime)
    }
  }
  
  #Our linear regression function:
  
  LR<-function(X,Y){
    clean = !is.na(Y) 
    Y = as.numeric(Y[clean]);
    X = as.numeric(X[clean])
    n=length(Y)
    Xmean=mean(X)
    Ymean=mean(Y)
    B=sum((X-Xmean)*(Y-Ymean))/sum((X-Xmean)^2)
    A=Ymean-B*Xmean
    return(c(B,A))
  }
  
  if(n<80){
    Result=LR(t,y_t)
    FinalSlope=Result[1]
    FinalIntercept=Result[2]
    if(FinalSlope>0){
      deadtime=(maxcap-FinalIntercept)/FinalSlope-2
      OUTPUT=max(deadtime,EndTime)
    }
  }else{
    
    #The following loop is to find great jump of data, great jump is t when y_t drop from high level to low level.
    #There is no perfect rule to find great jump, any rule will meet exceptions, therefore great jump is an assist in our algorithm
    
    GreatJump=c()
    Diffy_t=diff(y_t)
    m=length(Diffy_t)
    
    DetectJumpDown<-function(ind){
      ForEight<-y_t[max(ind-8,1):ind]
      BackEight<-y_t[(ind+1):min(m,ind+8)]
      if(Diffy_t[ind]<0 & min(ForEight)>max(BackEight) & max(0.8*abs(Diffy_t[ind]),0.3*Maxy_t)<(mean(ForEight)-mean(BackEight))){
        return(ind)
      }
      return(NA)
    }
    DetectJumpUp<-function(ind){
      ForEight<-y_t[max(ind-8,1):ind]
      BackEight<-y_t[(ind+1):min(m,ind+8)]
      if(Diffy_t[ind]>0 & max(ForEight)<min(BackEight) & max(0.8*abs(Diffy_t[ind]),0.3*Maxy_t)<(-mean(ForEight)+mean(BackEight))){
        return(ind)
      }
      return(NA)
    }
    
    
    detectret<-sapply(11:(m-11), FUN = DetectJumpDown)
    GreatJump<-detectret[!is.na(detectret)]
    
    #plot(Diffy_t)
    #GreatJump
    #plot(y_t)
    #As we said before, subsection cut by great jump is only an assist for our prediction. In case that cutting by great jump will make our data piece too small. We run the following code:
    #We will check the length of every piece of data cutted by great jump point from the end, if the length is less than 30, we will ignore this cut. 
    
    n=length(y_t)
    FinalCut=c(n)
    GreatJump=c(1,GreatJump)
    
    while(length(GreatJump)>0){
      lenJump=length(GreatJump)
      if(FinalCut[1]-GreatJump[lenJump]>=80){
        FinalCut=c(GreatJump[lenJump],FinalCut)
        GreatJump=GreatJump[-lenJump]
        n=FinalCut[1]
     }else{
        GreatJump=GreatJump[-lenJump]
     }
    } 
    #For any section cutted by FinalCut, do the linear regression 1/3 part from the end, 2/3 part from the end and the whole data, this is for exp and log shape data. 
    #We will consider R square from the three linear regression and choose the best one. 
    #We regard the slope must be positive.
    
    right=length(FinalCut)
    Index=FinalCut[right-1]:FinalCut[right]
    
    Y=y_t[Index]
    X=t[Index]
    n=length(Y)
    n1=round(1*n/4)
    n2=round(n/2)
    n3=round(3*n/4)
    Result3=LR(X[n3:n],Y[n3:n])
    Result2=LR(X[n2:n],Y[n2:n])
    Slope=c(Result3[1],Result2[1])
    Intercept=c(Result3[2],Result2[2])
    if(max(Slope)>0){
      index=which(Slope>0 & Slope<Inf)
      Intercept=Intercept[index]
      Slope=Slope[index]
      deadtime=(maxcap-Intercept)/Slope-2
      Finalindex=which.min(deadtime)
      FinalSlope=Slope[Finalindex]
      FinalIntercept=Intercept[Finalindex]
      OUTPUT=max(min(deadtime),EndTime)
    }else{
      Result1=LR(X[n1:n],Y[n1:n])
      Result0=LR(X,Y)
      Slope=c(Result1[1],Result0[1])
      Intercept=c(Result1[2],Result0[2])
      if(max(Slope)>0){
        index=which(Slope>0 & Slope<Inf)
        Intercept=Intercept[index]
        Slope=Slope[index]
        deadtime=(maxcap-Intercept)/Slope-2
        Finalindex=which.min(deadtime)
        FinalSlope=Slope[Finalindex]
        FinalIntercept=Intercept[Finalindex]
        OUTPUT=max(min(deadtime),EndTime)
      }else{OUTPUT=EndTime}
    }
  }  
  #If you want to see how our function fit the data. Run the following code:
  plot(y_t~t)
  abline(FinalIntercept, FinalSlope,col="red")
  return(OUTPUT)
}




#Example 1
maxcap = 100*10^3;
out_data = read.csv("out_youtube.csv",header=TRUE)
plot(out_data$t,out_data$y_t,ylim=c(0,maxcap),xlim=c(0,1600),
     ylab="Youtube Traffic (Kb)",xlab="Time (sec)",main="Youtube Traffic Data")
abline(h = maxcap,col="red",lwd=2)

out_data_y_t=out_data$y_t
out_data_t=out_data$t


#Example 2
maxcap=100
box_data = read.csv("out_box.csv",header=TRUE)
plot(box_data$t,box_data$y_t,ylim=c(-10,maxcap),xlim=c(0,400),
     ylab="Box Traffic (kb) ",xlab="Time (sec)",main="Box Traffic Data")
abline(h = maxcap,col="red",lwd=2)

box_data_y_t=box_data$y_t
box_data_t=box_data$t

y_t=box_data_y_t
t=box_data_t

#Test

n=500

Error=c('1',1,'-1',-1,Inf,'Inf','c','你好',NA,NaN,'NA','NULL','NaN')

#Example 3
test_t=1:n
test_y_t1=3*test_t-50+rnorm(length(test_t), mean=0, sd=70)
test_y_t1[round(0.5*n):(round(0.5*n)+30)]=0
test_y_t1[round(0.7*n):(round(0.7*n)+5)]=100
test_y_t1[sample(1:n, 13)]=Error
plot(test_y_t1~test_t)


#Example 4
test_y_t2=rep(0,n)
m=round(n/3)
test_y_t2[1:m]=3*test_t[1:m]-50+rnorm(m, mean=0, sd=70)
test_y_t2[(m+1):(2*m+1)]=4*test_t[1:m]-50+rnorm(m, mean=0, sd=70)
test_y_t2[(2*m+2):n]=10*test_t[1:m]-50+rnorm(m, mean=0, sd=70)
test_y_t2[round(0.5*n):(round(0.5*n)+30)]=0
test_y_t2[round(0.7*n):(round(0.7*n)+5)]=100
test_y_t2[sample(1:n, 13)]=Error
plot(test_y_t2~test_t)

#Example 5
test_y_t3=rep(0,n)
m=round(n/3)
test_y_t3[1:m]=10*test_t[1:m]-50+rnorm(m, mean=0, sd=70)
test_y_t3[(m+1):(2*m+1)]=5*test_t[1:m]-50+rnorm(m, mean=0, sd=70)
test_y_t3[(2*m+2):n]=1*test_t[1:m]-50+rnorm(m, mean=0, sd=70)
test_y_t3[sample(1:n, 13)]=Error
test_y_t3[round(0.5*n):(round(0.5*n)+30)]=0
test_y_t3[round(0.7*n):(round(0.7*n)+5)]=100
plot(test_y_t3~test_t)

#Example 6
m=round(4*n/5)
test_y_t4=rep(0,n)
test_y_t4[1:m]=rnorm(m, mean=100, sd=70)
test_y_t4[(m+1):n]=10*test_t[1:(n-m)]+rnorm(n-m, mean=0, sd=70)
test_y_t4[sample(1:n, 13)]=Error
test_y_t4[round(0.5*n):(round(0.5*n)+30)]=0
test_y_t4[round(0.7*n):(round(0.7*n)+5)]=100
plot(test_y_t4~test_t)

#Example 7
test_y_t5=100*log(test_t)+rnorm(n, mean=100, sd=70)
test_y_t5[sample(1:n, 13)]=Error
test_y_t5[round(0.5*n):(round(0.5*n)+30)]=0
test_y_t5[round(0.7*n):(round(0.7*n)+5)]=3
plot(test_y_t5~test_t)


#Example 8
test_y_t6=rep(0,n)
m=round(n/3)
test_y_t6[1:m]=3*test_t[1:m]-50+rnorm(m, mean=0, sd=70)
test_y_t6[(m+1):(2*m+1)]=4*test_t[1:m]-50+rnorm(m, mean=0, sd=70)
test_y_t6[(2*m+2):n]=-2*test_t[1:m]+600+rnorm(m, mean=0, sd=70)
test_y_t6[round(0.5*n):(round(0.5*n)+30)]=0
test_y_t6[round(0.7*n):(round(0.7*n)+5)]=100
test_y_t6[sample(1:n, 13)]=Error
plot(test_y_t6~test_t)


par(mfrow = c(4, 2))

Capacity_Deadtime(out_data_y_t,out_data_t,100*10^3)
Capacity_Deadtime(box_data_y_t,box_data_t,100)
Capacity_Deadtime(test_y_t1,test_t,2000)
Capacity_Deadtime(test_y_t2,test_t,2000)
Capacity_Deadtime(test_y_t3,test_t,2000)
Capacity_Deadtime(test_y_t4,test_t,2000)
Capacity_Deadtime(test_y_t5,test_t,2000)
Capacity_Deadtime(test_y_t6,test_t,2000)

##### What We'll Run for Grading #####
# The example data for y_t and t will change.

### Robustness/Error Tolerance ###
maxcap = 10; y_t = c(1:3,rep(NA,7)); t = 1:10; 
tryCatch(Capacity_Deadtime(y_t,t,maxcap),error=function(e) "error")
maxcap = 10; y_t = c(1:3,sample(c(rep("a",5),rep(NA,5)),7)); t = 1:10; 
tryCatch(Capacity_Deadtime(y_t,t,maxcap),error=function(e) "error")
# and other examples.

### Accuracy ###
maxcap = 15; y_t = 1:10; t = 1:10; trueTime = 14
# some loss function l(your_function(y_t,t,maxcap),trueTime)
# square-error loss:
(Capacity_Deadtime(y_t,t,maxcap) - trueTime)^2
# non-symmetric square-error loss:
if(trueTime >= Capacity_Deadtime(y_t,t,maxcap)) {
  (trueTime - Capacity_Deadtime(y_t,t,maxcap))^2
} else {
  Inf
}  

### Speed ###
maxcap=2*10^5
y_t = 1:(10^5);  y_t[sample(1:10^5,10)] = NA; y_t[sample(1:10^5,10)] = 0; 
t = 1:(10^5);

codetime = rep(0,10)
for(i in 1:10) {
  start=Sys.time()
  output = Capacity_Deadtime(y_t,t,maxcap) 
  end=Sys.time()
  codetime[i] = as.numeric(end-start)
}
mean(codetime)

### Scalability  ###
codetime = 1:6; maxcap = 10^7
for(i in 1:6) {
  start=Sys.time()
  y_t = 1:(10^i); t = 1:(10^i);
  output = Capacity_Deadtime(y_t,t,maxcap) 
  end=Sys.time()
  codetime[i] = as.numeric(end-start)
}
plot(1:6,log(codetime,10))


