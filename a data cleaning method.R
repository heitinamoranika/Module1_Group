#Any clean part of function must pass the following example:
#y_t=c('1',1,'-1',-1,Inf,'Inf','c','你好',NA,NULL,NaN,'NA','NULL','NaN')#

y_t=c('1',1,'-1',-1,Inf,'Inf','c','你好',NA,NULL,NaN,'NA','NULL','NaN')

y_t<-as.numeric(y_t)
clean_index=which(is.infinite(y_t)|y_t<0|is.na(y_t))
y_t<-y_t[-clean_index]



