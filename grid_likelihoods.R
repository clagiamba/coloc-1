# Use this to solve quadratic equations and find p1,p2,p12 for each value of C and X
qs<-function(a,b,c){
    a<-as.complex(a); b<-as.complex(b); c<-as.complex(c)
    i2<-(a!=0); i1<-((a==0)&(b!=0));
    solns<-as.complex(rep(NA,length(a)))
      solns<-cbind(solns,solns); colnames(solns)<-c("soln 1","soln 2")
    a2<-a[i2]; b2<-b[i2]; c2<-c[i2]
      solns[i2,1]<-(-b2 + sqrt(b2^2 - 4*a2*c2))/(2*a2)
      solns[i2,2]<-(-b2 - sqrt(b2^2 - 4*a2*c2))/(2*a2)
    b1<-b[i1]; c1<-c[i1]
      solns[i1,1]<-(-c1)/b1
    solns
  }

logsum <- function(x) {
  my.max <- max(x)                              ##take out the maximum value in log form
  my.res <- my.max + log(sum(exp(x - my.max )))
  return(my.res)
}


##############################
lkl.frame = read.table("likelihoods.tab", header=T)
# Create grid of priors for 
# X = p1/p2 ; C = p12/(p1*p2)
X = 10^(seq(-2, 2, 0.2))
C = seq(10, 10000, length.out = 20)
grid = expand.grid(X=X,C=C)
# Compute values of p1, p2, p12 for each row of the grid
# qs(C*X, X+1, -2e-04)
p2 = as.numeric(apply(grid[,c('X','C')], 1, function(y) qs(y['C']*y['X'], y['X']+1, -2e-04)[1]))
p1 = grid$X*p2
p12 = grid$C*p1*p2
p0=0.9998   # 1-(p1+p2+p12) 

grid= cbind(grid, p0,p1,p2,p12)

# Loop through each row of the grid
grid$sum.lkl <- NA
for (i in 1:nrow(grid)) {
  message("Looping through prior ", i)
  lkl.prior.i <- vector()

  lkl.frame.temp <- as.matrix(lkl.frame[, c("X1", "X2", "X3", "X4", "X5") ])
  p1 <- grid$p1[i]
  p2 <- grid$p2[i]
  p12 <- grid$p12[ i ]

  p0 <- 1 - (p1 + p2 + p12 + p1*p2)
  lkl.frame.temp[,1] <- lkl.frame.temp[,1] + log(p0)
  lkl.frame.temp[,2] <- lkl.frame.temp[,2] + log(p1)
  lkl.frame.temp[,3] <- lkl.frame.temp[,3] + log(p2)
  lkl.frame.temp[,4] <- lkl.frame.temp[,4] + log(p1) + log(p2)
  lkl.frame.temp[,5] <- lkl.frame.temp[,5] + log(p12)

  grid$sum.lkl[ i ] <- sum(apply(lkl.frame.temp, MAR = 1, FUN = logsum))

}

print(grid[which.max(grid$sum.lkl),])


# These are the values I get at the maximum for the test data
# 0.1 1587.368 0.9998 1.772828e-05 0.0001772828 4.988968e-06 5424.663

##############################
# If I replace with 0s I get these:
lkl.frame = matrix(0,ncol=5, nrow=355)
colnames(lkl.frame)=c("X1", "X2", "X3", "X4", "X5")
X = 10^(seq(-2, 2, 0.2))
C = seq(10, 10000, length.out = 20)
grid = expand.grid(X=X,C=C)
# Compute values of p1, p2, p12 for each row of the grid
# qs(C*X, X+1, -2e-04)
p2 = as.numeric(apply(grid[,c('X','C')], 1, function(y) qs(y['C']*y['X'], y['X']+1, -2e-04)[1]))
p1 = grid$X*p2
p12 = grid$C*p1*p2
p0=0.9998   # 1-(p1+p2+p12) 

grid= cbind(grid, p0,p1,p2,p12)

# Loop through each row of the grid
grid$sum.lkl <- NA
for (i in 1:nrow(grid)) {
  message("Looping through prior ", i)
  lkl.prior.i <- vector()

  lkl.frame.temp <- as.matrix(lkl.frame[, c("X1", "X2", "X3", "X4", "X5") ])
  p1 <- grid$p1[i]
  p2 <- grid$p2[i]
  p12 <- grid$p12[ i ]

  p0 <- 1 - (p1 + p2 + p12 + p1*p2)
  lkl.frame.temp[,1] <- lkl.frame.temp[,1] + log(p0)
  lkl.frame.temp[,2] <- lkl.frame.temp[,2] + log(p1)
  lkl.frame.temp[,3] <- lkl.frame.temp[,3] + log(p2)
  lkl.frame.temp[,4] <- lkl.frame.temp[,4] + log(p1) + log(p2)
  lkl.frame.temp[,5] <- lkl.frame.temp[,5] + log(p12)

  grid$sum.lkl[ i ] <- sum(apply(lkl.frame.temp, MAR = 1, FUN = logsum))

}

print(grid[which.max(grid$sum.lkl),])

> print(grid[which.max(grid$sum.lkl),])

             X        C     p0           p1           p2          p12
150 0.02511886 3690.526 0.9998 4.817134e-06 0.0001917736 3.409304e-06
         sum.lkl
150 5.750283e-14



###################### DO IT WITH OPTIM
# Use this to solve quadratic equations and find p1,p2,p12 for each value of C and X
qs<-function(a,b,c){
    a<-as.complex(a); b<-as.complex(b); c<-as.complex(c)
    i2<-(a!=0); i1<-((a==0)&(b!=0));
    solns<-as.complex(rep(NA,length(a)))
      solns<-cbind(solns,solns); colnames(solns)<-c("soln 1","soln 2")
    a2<-a[i2]; b2<-b[i2]; c2<-c[i2]
      solns[i2,1]<-(-b2 + sqrt(b2^2 - 4*a2*c2))/(2*a2)
      solns[i2,2]<-(-b2 - sqrt(b2^2 - 4*a2*c2))/(2*a2)
    b1<-b[i1]; c1<-c[i1]
      solns[i1,1]<-(-c1)/b1
    solns
  }

logsum <- function(x) {
  my.max <- max(x)                              ##take out the maximum value in log form
  my.res <- my.max + log(sum(exp(x - my.max )))
  return(my.res)
}

logsum <- function(x) {
  my.mean <- mean(x)                              ##take out the maximum value in log form
  my.res <- my.mean + log(sum(exp(x - my.mean )))
  return(my.res)
}

fn = function(p, data) {
  p1 = p[1]
  p2 = p[2]
  p12 = p[3]

  p0 <- 1 - (p1 + p2 + p12 + p1*p2)

  lkl.frame.temp[,1] <- lkl.frame.temp[,1] + log(p0)
  lkl.frame.temp[,2] <- lkl.frame.temp[,2] + log(p1)
  lkl.frame.temp[,3] <- lkl.frame.temp[,3] + log(p2)
  lkl.frame.temp[,4] <- lkl.frame.temp[,4] + log(p1) + log(p2)
  lkl.frame.temp[,5] <- lkl.frame.temp[,5] + log(p12)

  sumlkl = grid$sum.lkl[ i ] <- sum(apply(lkl.frame.temp, MAR = 1, FUN = logsum))
  return(sumlkl)
}


lkl.frame = read.table("/Users/claudiagiambartolomei/Downloads/likelihoods.tab", header=T)
# optim(c(10^-4, 10^-4, 10^-6), fn, data=lkl.frame, method = "Nelder-Mead", lower = c(10^-10, 10^-10, 10^-10), upper=c(0.999, 0.999, 0.999))
optim(c(10^-4, 10^-4, 10^-6), fn, data=lkl.frame, method = "Nelder-Mead", control=list(fnscale=-1))
