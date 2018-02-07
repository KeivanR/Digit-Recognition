library(hier.part)
library(progress)
seuil <- function(x,w,f=sigmoid){
  return(f(sum(x%*%w)))
  if (sum(x%*%w)>0) return (1)
  return (0)
}
sigmoid <- function(x){
  return (1/(1+exp(-x)))
}

propa <- function(x,w,couches){
  x <- as.numeric(as.character(x))
  nombre_couches <- length(couches)
  nombre_neurones <- unlist(couches)[length(unlist(couches))]
  a <- numeric(nombre_neurones)
  #entrées
  a[couches[[1]]] <- x
  #biais
  for (lay in 2:(nombre_couches-1)) {
    a[couches[[lay]][1]] <- 1
    a[couches[[lay]][-1]] <- apply(w[couches[[lay-1]],couches[[lay]][-1]],2,seuil,x = a[couches[[lay-1]]])
  }
  a[couches[[nombre_couches]]]<- apply(w[couches[[nombre_couches-1]],couches[[nombre_couches]]],2,seuil,x = a[couches[[nombre_couches-1]]])
  return(a)
}

retro_propa <- function(a,w,v,error,couches,convolution){
  nombre_couches <- length(couches)
  nombre_neurones <- unlist(couches)[length(unlist(couches))]
  d <- numeric(nombre_neurones)
  d[couches[[nombre_couches]]] <- error
  for (lay in (nombre_couches-1):2){
    diff1 <- a[couches[[lay]]]*(1-a[couches[[lay]]])
    d[couches[[lay]]] <- diff1*rowSums(t(t(w[couches[[lay]],couches[[lay+1]]])*d[couches[[lay+1]]]))
  } 
  for (lay in 3:nombre_couches){
    w[couches[[lay-1]],couches[[lay]]] <- w[couches[[lay-1]],couches[[lay]]] + v*t(sapply(a[couches[[lay-1]]],function(x){x*d[couches[[lay]]]}))
  }
  if (convolution) {
    for (i in couches[[2]])
    w[pave(i-length(couches[[1]])),i] <- w[pave(i-length(couches[[1]])),i] + v*t(sapply(a[pave(i)],function(x){x*d[i]}))
  }
  else  w[couches[[1]],couches[[2]]] <- w[couches[[1]],couches[[2]]] + v*t(sapply(a[couches[[1]]],function(x){x*d[couches[[2]]]}))
  return (w)
}

pave <- function(i,a=4,b=4,p=28,q=28){
  j <- i-1
  x1 <- a*j+1+(b-1)*p*trunc(a*j/p)
  x2 <- x1+a-1
  coef <- (x1:x2)+sapply(0:(b-1),function(x){rep(p,a)*x})
  return(as.vector(coef))
}

reseau_de_neurones <- function(hidden, v=0.3, nb_tests = 1000, precision = 100, 
                               entrees, sorties, winit=NULL, convolution = FALSE){
  tailles_couches <- c(dim(entrees)[2]+1,hidden,dim(sorties)[2])
  couches <- list()
  num <- 1
  for (lay in 1:length(tailles_couches)){
    couches[[lay]] <- num:(num+tailles_couches[lay]-1)
    num <- num+tailles_couches[lay]
  }
  nombre_couches <- length(couches)
  nombre_neurones <- sum(tailles_couches)
  a <- numeric(nombre_neurones)
  w <- matrix(0,nrow = nombre_neurones, ncol = nombre_neurones)
  for (lay in 2:nombre_couches){
    for (i in couches[[lay-1]]){
      for (j in couches[[lay]]){
        w[i,j] <- runif(1,-1,1)
      }
    }  
  }
  if (convolution) {
    for (i in couches[[2]]){
      w[!couches[[1]]%in%pave(i-length(couches[[1]])),i] <- 0
    }
  }
  if (!is.null(winit)) w <- winit
  tests <- 1
  no_error <- 0
  entrees <- cbind(1,entrees)
  pct <- progress_bar$new(total = nb_tests)
  while (tests<nb_tests){
    x <- entrees[tests,]
    x[is.na(x)] <- 0
    # print(paste('Entrées :',x[2],x[3]))
    a <- propa(x,w,couches)
    pred <- a[couches[[nombre_couches]]]
    # print(paste("Je pense que c'est",pred))
    y <- sorties[tests,]
    # print(paste('Il fallait trouver :',y))
    error <- y-pred
    w <- retro_propa(a,w,v,error,couches,convolution)
    tests <- tests+1
    pct$tick()
    
  }
  return (list(weights = w, couches = couches))
  
}
sign <- function(x){
  if (x<0) return (-1)
  else return (1)
}
estim <- function(net, y, n){
  if (length(which(net$weights[n,]!=0))==0) return (y[n+1-net$couches[[length(net$couches)]][1]])
  else{
    i <- which.max(abs(net$weights[n,]))
    return (0.5*(1+sign(net$weights[n,i]*(estim(net,y,i)-0.5))))
  }
}
estimation <- function(nb){
  y <- numeric(10)
  y[nb+1]<-1
  estim_nb<-sapply(1:dim(train)[2],estim,net=net,y=y)
  show_digit(estim_nb)
}
estim_nb <- estimation(0)
prediction <- function (net,x,prec=100){
  x <- c(1,x)
  return (1/prec*trunc(propa(x,net$weights,net$couches)[net$couches[[length(net$couches)]]]*prec))
}
chosen <- function(net,x,prec=100){
  return((0:9)[which.max(prediction(net,x,prec))])
}
synapses_entrants <- function(net,couche){
  return(net$weights[rowSums(net$weights[,net$couches[[couche]]])!=0,net$couches[[couche]]])
}



entrees <- train/255
reponses <- matrix(0,length(train_y),10)
for (i in 1:length(train_y)){
  reponses[i,train_y[i]] <- 1
}
net <- reseau_de_neurones(hidden = c(49), entrees = entrees, sorties = reponses, winit = NULL, nb_tests = 10000,convolution = TRUE)
try <- function(){
  k <- 1
  while (k<length(test_y)){
    pred <- data.frame(0:9,prediction(net = net,x = test[k,]/255,prec = 100000000))
    colnames(pred) <- c("num","proba")
    show_digit(test[k, ],xlab=paste("Actual :",as.numeric(as.character(test_y[k])),"\nPredicted :",pred$num[which.max(pred$proba)]))
    print(pred)
    print(pred$num[order(pred$proba,decreasing = T)][pred$proba[order(pred$proba,decreasing = T)]>0])
    print(paste("predicted :",pred$num[which.max(pred$proba)]))
    print(paste("actual :",as.numeric(as.character(test_y[k]))))
    k <- k+1
    readline(prompt="next")
  }
}
try()

predicted <- apply(test,1,chosen,net = net,prec = 100000000)
length(which(predicted==test_y))/10000

#############  APPLICATION AUX SITES TELECOM ##############
sites <- unique(unlist(cam2))
adjacence <- as.data.frame(matrix(0,length(cam2),length(sites))) 
colnames(adjacence) <- sites
rownames(adjacence) <- names(cam2)
for (i in 1:length(names(cam2))){
  adjacence[i,sites%in%cam2[[i]]]<-1
}
adjacence_pb <- adjacence[!rownames(adjacence)%in%c('cuves intermediaires ?'),]
#Oscillations
serveur <- TOM3
entrees <- t(serveur$fuel_cm_c[serveur$fuel_cm_c$Date>=as.POSIXct(d1)&serveur$fuel_cm_c$Date<=as.POSIXct(d2),])[-1,1:500]
sites_prob <- t(adja_regrouper(adjacence))
sites_prob <- sites_prob[match(row.names(entrees),sites_prob[,1]),]
reponses <- matrix(0,dim(entrees)[1],3)
for (i in 1:dim(entrees)[1]){
  if (grepl('oscillations',sites_prob[sites_prob[,1]==row.names(entrees)[i],2])) reponses[i,1] <- 1
  else reponses[i,2] <- 1
}
net_osc <- reseau_de_neurones(hidden = 502, entrees = entrees, sorties = reponses, winit = NULL, nb_tests = dim(entrees)[1])

#Phase test
dtest1 <- ymd(d1)%m+%months(-1)
dtest2 <- ymd(d2)%m+%months(-1)
x <- serveur$fuel_cm_c$`TBOE601`[serveur$fuel_cm_c$Date>=as.POSIXct(dtest1)&serveur$fuel_cm_c$Date<=as.POSIXct(dtest2)][1:500]
x <- x/max(x)
x1 <- data.frame(x,x)
afficher(x1,date = serveur$fuel_cm_c$Date[serveur$fuel_cm_c$Date>=as.POSIXct(dtest1)&serveur$fuel_cm_c$Date<=as.POSIXct(dtest2)][1:500])
prediction(net_osc,x,prec = 1000000)


