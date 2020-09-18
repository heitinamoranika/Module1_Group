##### Sample Code for Module 1#####

# your_function: the function you'll turn in as part of the module.
#                a. The function must take in the arguments 
#                   listed below and must output a single scalar number.
#                b. We'll not install any additional R packages when
#                   running your code. 
#                c. We'll test your code on R version 4.0.2.
#               
# Input: (1) y_t: a vector value representing the outcome from timepoint in t
#                 Value should be between 0 to maxcap, but could include NA or other values
#        (2) t: a vector value representing timepoints.
#               This value will always be 0,1,2,...,M where M is some integer.
#        (3) maxcap: a scalar value representing the maximum capacity of hard_drive
#                    This value will always be a positive integer.
# Output: A scalar value representing the time at which the server will reach close to maximum capacity.
#         This scalar value should be higher than M in (2).
your_function <- function(y_t,t,maxcap) {
  # Some computation here #
  return(max(t))
}

##### What We'll Run for Grading #####
# The example data for y_t and t will change.

### Robustness/Error Tolerance ###
maxcap = 10; y_t = c(1:3,rep(NA,7)); t = 1:10; 
tryCatch(your_function(y_t,t,maxcap),error=function(e) "error")
maxcap = 10; y_t = c(1:3,sample(c(rep("a",5),rep(NA,5)),7)); t = 1:10; 
tryCatch(your_function(y_t,t,maxcap),error=function(e) "error")
# and other examples.

### Accuracy ###
maxcap = 15; y_t = 1:10; t = 1:10; trueTime = 14
# some loss function l(your_function(y_t,t,maxcap),trueTime)
# square-error loss:
(your_function(y_t,t,maxcap) - trueTime)^2
# non-symmetric square-error loss:
if(trueTime >= your_function(y_t,t,maxcap)) {
  (trueTime - your_function(y_t,t,maxcap))^2
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
  output = your_function(y_t,t,maxcap) 
  end=Sys.time()
  codetime[i] = as.numeric(end-start)
}
mean(codetime)

### Scalability  ###
codetime = 1:6; maxcap = 10^7
for(i in 1:6) {
  start=Sys.time()
  y_t = 1:(10^i); t = 1:(10^i);
  output = your_function(y_t,t,maxcap) 
  end=Sys.time()
  codetime[i] = as.numeric(end-start)
}
plot(1:6,log(codetime,10))
