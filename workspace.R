install.packages(scales)    

remove(list = ls()[(grepl("", ls()))])
load('C:/Users/jaemini_man/Desktop/project/DACON_modified_dataset.RData')

dt <- function(data,id){
  a <- 375*as.integer(id)-374
  b <- 375*(as.integer(id))
  dd <- data$variable[a:b,]
  return(dd)
}

plot_data <- function(data,id){
  plot(dt(data,id)$Time,dt(data,id)$S1, type='l', ylim=c(-10**6,10**6))
  lines(dt(data,id)$Time,dt(data,id)$S2, col='red')
  lines(dt(data,id)$Time,dt(data,id)$S3, col='blue')
  lines(dt(data,id)$Time,dt(data,id)$S4, col='green')
}

plot_data(data,1)

data$label$V
plot_lb <- function(data,id){
  x<-data$label$X[id]
  y<-data$label$Y[id]
  m<-data$label$M[id]
  v<-data$label$V[id]
  plot(x,y, cex = m/40+2, xlim=c(-450,450), ylim=c(-450,450), pch = 19, col=alpha('red',v))
}

plot_data_lb <- function(data,id){
  par(mfrow=c(2,1))
  plot_data(data,id)
  plot_lb(data,id)
  text(-100,0, bquote(M==.(data$label$M[id])))
  text(100,0, bquote(V==.(data$label$V[id])))
}

plot_data_lb(data,1007)

a<-0
dd <- c()
for(i in 1:2521){
  if((data$label[i,2] == data$label[1,2])
     &(data$label[i,3] == data$label[1,3])
     &(data$label)[i,5] == data$label[1,5]){
    a <- 1 +a
    dd <- append(dd,i)
  }
}
dd

data$label
data$label$id[2521]

81*7*5

dt(data,1)
transition_time <- function(data, sensor, id){
  a <- 375*as.integer(id)-374 
  b <- 375*(as.integer(id))
  c <- as.integer(sensor+2)
  sensor_dt <- data$variable[a:b,c]
  for (i in 1:375){
    if (abs(sensor_dt[i])>0) {
      return(i)
    }
  }
}

t_time<-function(data,sensor_no){
  tt<-c()
  for (i in 1:2521){
    a <- transition_time(data,sensor_no,i)
    tt <- append(tt, a)
  }
  return (tt)
}

t_time(data,2)
par(mfrow=c(1,1))
plot_t_time<-function(data, axis, sensor_no){
  if (axis==1){
    ax<- axis+1
    plot(data$label[,ax], t_time(data,sensor_no), xlab = 'X')
  }
  else {ax<- axis+1
  plot(data$label[,ax], t_time(data,sensor_no), xlab = 'Y')
  }
}

plot_t_time(data,1,4)


