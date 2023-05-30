#lotka-volterra


dolotka <- function(t, alpha, beta, delta, gamma, x, y){
  out.df <- data.frame(t = seq(1:t), x = x, y = y )
  for(i in 2:t){
    out.df$x[i] = out.df$x[i-1] + (alpha*out.df$x[i-1] - beta*out.df$x[i-1]*out.df$y[i-1])
    
    out.df$y[i] = out.df$y[i-1] + (delta*out.df$x[i-1]*out.df$y[i-1] - gamma*out.df$y[i-1])
    
    if(out.df$x[i] < 0 | out.df$x[i-1] == 0){
      out.df$x[i] = 0
    }
    
    if(out.df$y[i] < 0 | out.df$y[i-1] == 0){
      out.df$y[i] = 0
    }
    
  }
  return(out.df)
}





dolotka_K.x <- function(t, alpha, beta, delta, gamma, x, y, x.K){
  out.df <- data.frame(t = seq(1:t), x = x, y = y )
  for(i in 2:t){
    out.df$x[i] = out.df$x[i-1] + (alpha*out.df$x[i-1]*(1-(out.df$x[i-1]/x.K)) - beta*out.df$x[i-1]*out.df$y[i-1])
    
    out.df$y[i] = out.df$y[i-1] + (delta*out.df$x[i-1]*out.df$y[i-1] - gamma*out.df$y[i-1])
    
    if(out.df$x[i] < 0 | out.df$x[i-1] == 0){
      out.df$x[i] = 0
    }
    
    if(out.df$y[i] < 0 | out.df$y[i-1] == 0){
      out.df$y[i] = 0
    }
    
  }
  return(out.df)
}




dolotka_K.x_Stochast <- function(t, alpha, beta, delta, gamma, x, y, x.K, x.p, y.p){
  out.df <- data.frame(t = seq(1:t), x = x, y = y )
  for(i in 2:t){
    tempxp <- runif(1,0,1)
    xd = 0
    if(tempxp < x.p){
      xd = -1
    }
    
    tempyp <- runif(1,0,1)
    yd = 0
    if(tempyp < y.p){
      yd = -1
    }
    
    
    out.df$x[i] = out.df$x[i-1] + (alpha*out.df$x[i-1]*(1-(out.df$x[i-1]/x.K)) - beta*out.df$x[i-1]*out.df$y[i-1])+(out.df$x[i-1]*xd*runif(1,0,0.1))
    
    out.df$y[i] = out.df$y[i-1] + (delta*out.df$x[i-1]*out.df$y[i-1] - gamma*out.df$y[i-1])+(out.df$y[i-1]*yd*runif(1,0,0.1))
    
    if(out.df$x[i] < 0 | out.df$x[i-1] == 0){
      out.df$x[i] = 0
    }
    
    if(out.df$y[i] < 0 | out.df$y[i-1] == 0){
      out.df$y[i] = 0
    }
    
  }
  return(out.df)
}


alpha = 0.1
beta = 0.01
delta = 0.001
gamma = 0.03
x = 100
y = 10


pureLotka <- dolotka(t = 3000,alpha =  alpha, beta = beta, delta = delta, gamma = gamma,x = x, y = y)
pureLotka$type = "simple"

plot(pureLotka$t, pureLotka$x, type = "l", col = "blue", ylim = c(0, max(pureLotka$x)+1))
lines(pureLotka$t, pureLotka$y, col = "red")

lotka.with.k <- dolotka_K.x(t = 3000,alpha =  alpha, beta = beta, delta = delta, gamma = gamma,x = x, y = y, x.K = 500)
lotka.with.k$type = "with.k"

plot(lotka.with.k$t, lotka.with.k$x, type = "l", col = "blue", ylim = c(0, max(lotka.with.k$x)+1))
lines(lotka.with.k$t, lotka.with.k$y, col = "red")


lotka.with.k.stochastic <- dolotka_K.x_Stochast(t = 3000,alpha =  alpha, beta = beta, delta = delta, gamma = gamma,x = x, y = y, x.K = 500, x.p = 0.1, y.p = 0.01)
lotka.with.k.stochastic$type = "with.k.stochastic"


plot(lotka.with.k.stochastic$t, lotka.with.k.stochastic$x, type = "l", col = "blue", ylim = c(0, max(lotka.with.k.stochastic$x)+1))
lines(lotka.with.k.stochastic$t, lotka.with.k.stochastic$y, col = "red")

pureLotka.g <- pureLotka%>%gather(key = "population", value = "N", -t, -type)

lotka.with.k.g <- lotka.with.k%>%gather(key = "population", value = "N", -t, -type)

lotka.with.k.stochastic.g <- lotka.with.k.stochastic%>%gather(key = "population", value = "N", -t, -type)


allLotkas <- rbind(pureLotka.g,lotka.with.k.g,lotka.with.k.stochastic.g)


ggplot(allLotkas, aes(t, N, colour = population))+geom_line()+facet_grid(vars(type), scales = "free")+theme_bw()+scale_colour_manual(values = c("blue","red"),labels = c("prey", "predator"))+scale_y_continuous(labels = NULL)+scale_x_continuous(labels = NULL)
