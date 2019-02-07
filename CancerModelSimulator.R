library(deSolve)


#####################################################################################################
#
#                                 Growth Models
#     V = Volume of Tumor
#     a,b,c = Are parameters that can be ajusted to describe a particular data set.
#
#
#
#####################################################################################################
#
#                             Exponential Model
#                                   V = aV
#

f = function( time, v , parameter){
  with(as.list(c(v,parameter)),{ 
    dp = (parameter[1]*v)
    list(dp)
  })
  
}
parameter = c(0.1,0.2)
time = seq(0,100,1)
v=c(P=0.1)
out=ode(v,time,f,parameter)
plot(out)

f = function(time, a, v){
  P = a*v
}
a = 0.1
v = 1
time = seq(1,100,1)
plot(f(time,a,v))
lines(out,col="green")

####################################################################################################
#
#                               Mendelsohn Model
#                                   V = aV^b
#


f = function(time, v , parameter){
  with(as.list(c(v,parameter)),{ 
    dp = (parameter[1]*(v^parameter[2]))
    list(dp)
  })
  
}

#parameter(a,b)
parameter = c(0.1,0,2)
time = seq(0,100,1)
v=c(P=0.1)
out=ode(v, time, f, parameter)
plot(out)


f = function(time, a, v, b){
  P = a*(v^b)
}
a = 0.1
b = 0.1 
v = 1
time = seq(1,100,1)
plot(f(time,a,v,b))
lines(out,col="blue")

#####################################################################################################
#
#                                   Logistic Model
#                                   V = aV*(1-v/b)
#

f = function( time, v , parameter){
  with(as.list(c(v,parameter)),{ 
    dp = (parameter[1]*v)*(1-v/parameter[2])
    list(dp)
  })
  
}
parameter = c(0.1,0.2)
time = seq(0,100,1)
v=c(P=0.1)
out=ode(v, time, f, parameter)
plot(out)


f = function(time, a, v, b){
  P = a*v*(1-v/b)
}
a = 0.1
b = 0.1 
v = 1
time = seq(1,100,1)
plot(f(time,a,v,b))
lines(out,col="blue")

#####################################################################################################
#
#                                   Lineal Model
#                                   V = aV/(v+b)
#

f = function( time, v , parameter){
  with(as.list(c(v,parameter)),{ 
    dp = (parameter[1]*v)/(v+parameter[2])
    list(dp)
  })
  
}
parameter = c(0.1,0.2)
time = seq(0,100,1)
v=c(P=0.1)
out=ode(v, time, f, parameter)
plot(out)


f = function(time, a, v, b){
  P = (a*v)/(v+b)
}
a = 0.1
b = 0.1 
v = 1
time = seq(1,100,1)
plot(f(time,a,v,b))
lines(out,col="blue")

#####################################################################################################
#
#                                   Surface Model
#                                   V = aV*(v+b)^3
#

f = function( time, v , parameter){
  with(as.list(c(v,parameter)),{ 
    dp = (parameter[1]*v)/((v+parameter[2])^3)
    list(dp)
  })
  
}
parameter = c(0.1,0.2)
time = seq(0,100,1)
v=c(P=0.1)
out=ode(v, time, f, parameter)
plot(out)


f = function(time, a, v, b){
  P = (a*v)/((v+b)^3)
}
a = 0.1
b = 0.1 
v = 1
time = seq(1,100,1)
plot(f(time,a,v,b))
lines(out,col="blue")

#####################################################################################################
#
#                                    Gumpertz Model
#                                   V = aVln*(b/v+c)
#

f = function( time, v , parameter){
  with(as.list(c(v,parameter)),{ 
    dp = (parameter[1]*v)*log(parameter[2]/v+parameter[3])
    list(dp)
  })
  
}
# Parameter (a,b,c)
parameter = c(0.1,0.2,0.3)
time = seq(0,100,1)
v=c(P=0.1)
out=ode(v, time, f, parameter)
plot(out)


f = function(time, a, v, b, c){
  P = (a*v)*log(b/v+c)
}
a = 0.1
b = 0.1 
c = 0.1
v = 1
time = seq(1,100,1)
plot(f(time,a,v,b,c))
lines(out,col="blue")


#####################################################################################################
#
#                                   Bertanlaffy Model
#                                    V = aV^2/3 - bV
#

f = function( time, v , parameter){
  with(as.list(c(v,parameter)),{ 
    dp = (parameter[1]*v)^(2/3) - parameter[2]*v
    list(dp)
  })
  
}
# Parameter (a,b)
parameter = c(0.1,0.2)
time = seq(0,100,1)
v=c(P=0.1)
out=ode(v, time, f, parameter)
plot(out)


f = function(time, a, v, b){
  P = (a*(v^(2/3))) - (b*v)
}
a = 0.1
b = 0.1 
v = 1
time = seq(1,100,1)
plot(f(time,a,v,b))
lines(out,col="blue")

