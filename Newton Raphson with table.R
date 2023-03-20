
# Newton-Raphson Method
#______________________

# First, we define the function we want to use.
fn <- function(x){x^3-2*x^2+x-3} # other function: exp(-x)-x
fn(4)
#_____________________

#Second, get the derivative.
fne<- expression(x^3-2*x^2+x-3)
#fn1<- deriv(fne, "x")
fn1<- D(fne, 'x')
# this will print the derivative 
fn1 #3 * x^2 - 2 * (2 * x) + 1
#_____________________
#Third, define new function of the derivative
fn_1<- function(x){3 * x^2 - 2 * (2 * x) + 1}
fn_1(4)
#_____________________

#To graph the function  we have to see the intial point
curve(fn, from=-5, to=5, xlab="x", ylab="y")
# NOTE:  you can change the range of x (from, to) 
#_____________________


#Eventually we define the function of Newton-Raphson 
#___________________________________________________

#NewRaph(x="number",es="error",j="number of iteration")



NewRaph <- function(x, es = 0.05, j = 10) {
  i = 0 #intial value for i
  ea <- 1.1 * es
  x0 = x
  newlist <- list()
  
  while (ea > es & i < j) {
    if (fn_1(x0) != 0) {
      #to check that the derivative is not = 0
      x1 <- x0 - (fn(x0) / fn_1(x0))
      ea <-
        abs((x1 - x0)) #/x1 -> divide by x1 if you want relative error
      
      looplist <- list(i,x0, fn(x0), fn_1(x0), x1, ea)
      newlist <- list.append(newlist, looplist)
      x0 = x1
      i = i + 1
    } else{
      i = 1 + j
      ea <- 0.9 * es
      print("Division by 0")
    }
  }
  df <-  as.data.frame(do.call(rbind, newlist))
  list_name <- list("iteration",
                    "x(i)",
                    "f(xi)",
                    "f'(xi)",
                    "x(i+1)",
                    "Error")
  names(df) <- c(list_name)
  print(df)
  #View(df)
  cat("The root is: x=", x0 , "\n") #
}

NewRaph(4,0.005,5)

#let's graph the function again
x_1= 2.17456 
curve(fn, from=1, to=2.5, xlab="x", ylab="y")
abline(h=0,col="red",lwd=1.5)
abline(v=x_1,col="red" ,lwd=1.5)
text(x=x_1-0.3,y=1,labels="(2.17456,0)")

