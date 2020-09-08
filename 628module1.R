#Example 1
maxcap = 100*10^3;
out_data = read.csv("out_youtube.csv",header=TRUE)
plot(out_data$t,out_data$y_t,ylim=c(0,maxcap),xlim=c(0,1600),
     ylab="Youtube Traffic (Kb)",xlab="Time (sec)",main="Youtube Traffic Data")
abline(h = maxcap,col="red",lwd=2)

#Example 2
maxcap=100
box_data = read.csv("out_box.csv",header=TRUE)
plot(box_data$t,box_data$y_t,ylim=c(-10,maxcap),xlim=c(0,400),
     ylab="Box Traffic (kb) ",xlab="Time (sec)",main="Box Traffic Data")
abline(h = maxcap,col="red",lwd=2)

#Example 3
y_t=c(1,2,3,4,5,'a','apple',1e3,'1e3',-3,-3,6,NA,NaN)
t=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14)

#If you use Example 1 and 2, please run the following code:
#y_t=out_data$y_t
#t=out_data$t

#length of row y_t
n=length(y_t)

#clean our data
check_up=toupper(y_t)
check_low=tolower(y_t)
clean_index=which(check_up==check_low & y_t>=0)

clean_index
y_t[clean_index]
