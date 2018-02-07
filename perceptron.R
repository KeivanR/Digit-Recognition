w <- c(0.1,0.3,0.5)
no_error <- 0
tests <- 0
v <- 0.1
f <- function(x,w){
  if (sum(x%*%w)>0) return (1)
  return (0)
}
while (no_error<10&&tests<1000){
  plot(x=NA,y=NA,xlim = c(-0.5,1.5), ylim = c(-0.5,1.5))
  abline(h=0)
  abline(v=0)
  abline(h=1)
  abline(v=1)
  abline(a = -w[1]/w[3],b = -w[2]/w[3])
  x <- c(1,NA,NA)
  x[2] <- as.numeric(readline(prompt="Première entrée : "))
  x[3] <- as.numeric(readline(prompt="Deuxième entrée : "))
  pred <- f(x,w)
  print(paste("Je pense que c'est",pred))
  y <- as.numeric(readline(prompt="Il fallait trouver : "))
  error <- pred-y
  w <- w - v*error*x
  if (error==0) no_error <- no_error+1
  else no_error <- 0
  tests <- tests+1
}
