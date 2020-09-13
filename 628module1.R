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


#Test

n=500

Error=c('1',1,'-1',-1,Inf,'Inf','c','你好',NA,NaN,'NA','NULL','NaN')

#Example 1
test_t=1:n
test_y_t1=3*test_t-50+rnorm(length(test_t), mean=0, sd=70)
test_y_t1[round(0.5*n):(round(0.5*n)+30)]=0
test_y_t1[round(0.7*n):(round(0.7*n)+5)]=100
test_y_t1[sample(1:n, 13)]=Error
plot(test_y_t1~test_t)


#Example 2
test_y_t2=rep(0,n)
m=round(n/3)
test_y_t2[1:m]=3*test_t[1:m]-50+rnorm(m, mean=0, sd=70)
test_y_t2[(m+1):(2*m+1)]=4*test_t[1:m]-50+rnorm(m, mean=0, sd=70)
test_y_t2[(2*m+2):n]=10*test_t[1:m]-50+rnorm(m, mean=0, sd=70)
test_y_t2[round(0.5*n):(round(0.5*n)+30)]=0
test_y_t2[round(0.7*n):(round(0.7*n)+5)]=100
test_y_t2[sample(1:n, 13)]=Error
plot(test_y_t2~test_t)

#Example 3
test_y_t3=rep(0,n)
m=round(n/3)
test_y_t3[1:m]=10*test_t[1:m]-50+rnorm(m, mean=0, sd=70)
test_y_t3[(m+1):(2*m+1)]=5*test_t[1:m]-50+rnorm(m, mean=0, sd=70)
test_y_t3[(2*m+2):n]=1*test_t[1:m]-50+rnorm(m, mean=0, sd=70)
test_y_t3[sample(1:n, 13)]=Error
test_y_t3[round(0.5*n):(round(0.5*n)+30)]=0
test_y_t3[round(0.7*n):(round(0.7*n)+5)]=100
plot(test_y_t3~test_t)

#Example 4
m=round(4*n/5)
test_y_t4=rep(0,n)
test_y_t4[1:m]=rnorm(m, mean=100, sd=70)
test_y_t4[(m+1):n]=10*test_t[1:(n-m)]+rnorm(n-m, mean=0, sd=70)
test_y_t4[sample(1:n, 13)]=Error
test_y_t4[round(0.5*n):(round(0.5*n)+30)]=0
test_y_t4[round(0.7*n):(round(0.7*n)+5)]=100
plot(test_y_t4~test_t)

#Example 5
test_y_t5=100*log(test_t)+rnorm(n, mean=100, sd=70)
test_y_t5[sample(1:n, 13)]=Error
test_y_t5[round(0.5*n):(round(0.5*n)+30)]=0
test_y_t5[round(0.7*n):(round(0.7*n)+5)]=3
plot(test_y_t5~test_t)


#Example 6
test_y_t6=rep(0,n)
m=round(n/3)
test_y_t6[1:m]=3*test_t[1:m]-50+rnorm(m, mean=0, sd=70)
test_y_t6[(m+1):(2*m+1)]=4*test_t[1:m]-50+rnorm(m, mean=0, sd=70)
test_y_t6[(2*m+2):n]=-2*test_t[1:m]+600+rnorm(m, mean=0, sd=70)
test_y_t6[round(0.5*n):(round(0.5*n)+30)]=0
test_y_t6[round(0.7*n):(round(0.7*n)+5)]=100
test_y_t6[sample(1:n, 13)]=Error
plot(test_y_t6~test_t)



Capacity_Deadtime<-function(y_t,t,maxcap){
  
  #Clean part:
  #Any clean part of function must pass the following example:
  #y_t=c('1',1,'-1',-1,Inf,'Inf','c','你好',NA,NULL,NaN,'NA','NULL','NaN')
  
  y_t=as.numeric(y_t)
  t=as.numeric(t)
  EndTime=max(t)
    
  clean_up=toupper(y_t)
  clean_down=tolower(y_t)
  
  clean_index=which(clean_up==clean_down & y_t>=0 & !is.na(y_t) & y_t<Inf)
  
  y_t=y_t[clean_index]
  t=t[clean_index]
  
  y_t=as.numeric(y_t)
  t=as.numeric(t)
  
  #Plot the y_t if you want
  #plot(y_t~t)
  
  n=length(y_t)
  
  #Two special situations:
  #If the valid data length is less than 1, just give an alarm at the end of t, we regard the system is broken and nothing is recorded.
  #If the y_t in the end of time is larger than 95% of maxcap, just give an alarm at the end of t, we regard the system will broken soon.
  
  if(n<=1 | mean(y_t[min((n-10),n):n])>0.95*maxcap){
    OUTPUT=EndTime
  }else{
  
  #The following loop is to find great jump of data, great jump is t when y_t drop from high level to low level.
  #There is no perfect rule to find great jump, any rule will meet exceptions, therefore great jump is an assist in our algorithm
  
  GreatJump=c()
  Diffy_t=diff(y_t)
  m=length(Diffy_t)
  for(i in 11:(m-11)){
    ForEight=y_t[max(i-8,1):i]
    BackEight=y_t[(i+1):min(m,i+8)]
    if(Diffy_t[i]<0 & min(ForEight)>max(BackEight) & 0.8*abs(Diffy_t[i])<(mean(ForEight)-mean(BackEight))){
      GreatJump=c(GreatJump,i)
    }
  }
  
  #plot(Diffy_t)
  #GreatJump
  #plot(y_t)
  
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
    FitY=A+B*X
    SST=sum((Y-Ymean)^2)
    SSR=sum((Y-FitY)^2)
    rsquare=1-SSR/SST
    return(c(B,A,rsquare))
  }
  
  #As we said before, subsection cut by great jump is only an assist for our prediction. In case that cutting by great jump will make our data piece too small. We run the following code:
  #We will check the length of every piece of data cutted by great jump point from the end, if the length is less than 30, we will ignore this cut. 
  
  n=length(y_t)
  FinalCut=c(n)
  GreatJump=c(1,GreatJump)
  
  while(length(GreatJump)>0){
    lenJump=length(GreatJump)
    if(FinalCut[1]-GreatJump[lenJump]>=100){
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
  n2=round(2*n/3)
  n1=round(n/3)
  Result1=LR(X[n2:n],Y[n2:n])
  Result2=LR(X[n1:n],Y[n1:n])
  Result3=LR(X,Y)
  AllSlope=c(Result1[1],Result2[1],Result3[1])
  AllIntercept=c(Result1[2],Result2[2],Result3[2])
  AllRsquare=c(Result1[3],Result2[3],Result3[3])
  GoodOutput=AllSlope>0
  GoodSlope=AllSlope[GoodOutput]
  GoodIntercept=AllIntercept[GoodOutput]
  GoodRsquare=AllRsquare[GoodOutput]
  BestOutput=which.max(GoodRsquare)
  FinalIntercept=GoodIntercept[BestOutput]
  FinalSlope=GoodSlope[BestOutput]
  deadtime=(maxcap-FinalIntercept)/FinalSlope-10 
  OUTPUT=deadtime
  
  #If you want to see how our function fit the data. Run the following code:
  plot(y_t~t)
  abline(FinalIntercept, FinalSlope,col="red")
  }
  return(OUTPUT)
  #If you want to check any detail in our function, use the following return:
  #return(c(OUTPUT,FinalCut,GreatJump))
}

par(mfrow = c(4, 2))

Capacity_Deadtime(out_data_y_t,out_data_t,100*10^3)
Capacity_Deadtime(box_data_y_t,box_data_t,100)
Capacity_Deadtime(test_y_t1,test_t,2000)
Capacity_Deadtime(test_y_t2,test_t,2000)
Capacity_Deadtime(test_y_t3,test_t,2000)
Capacity_Deadtime(test_y_t4,test_t,2000)
Capacity_Deadtime(test_y_t5,test_t,2000)
Capacity_Deadtime(test_y_t6,test_t,2000)
