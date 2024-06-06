
library(reshape2)
library(ggplot2)
library(gridExtra)

######################### funkce   ###############################
######################### simulace ###############################

SIMUL = function (theta,y) {
  
  # pocet pozorovani
  T = length(y)
  
  # parametry
  omega = theta[1]
  alpha = theta[2]
  beta  =  theta[3]
  sig1  = abs(omega/(1-alpha-beta))       # ne vzdy kladne cislo, proto absolutni hodnota
  
  # inovace
  epsilon = rnorm(T)
  
  # volatilita
  sig = rep(0,T)
  x = rep(0,T)
  sig[1] = sig1
  
  # GARCH model
  for (t in 2:T) {
    x[t] = sqrt(sig[t]) * epsilon[t]
    sig[t+1] = omega + alpha * x[t]^2 + beta * sig[t]
  }
  
  # priprava pro graf
  df = data.frame(sig = sqrt(sig[1:T]), y = x)
  dfp = melt(df)
  dfp$x = rep(1:T,2)
  
  return(dfp)
}

# krypto - simulace
BTC_dfp <- SIMUL(BTC_opt.theta,BTC_y)
ETH_dfp <- SIMUL(ETH_opt.theta,ETH_y)
LTC_dfp <- SIMUL(LTC_opt.theta,LTC_y)
ZEC_dfp <- SIMUL(ZEC_opt.theta,ZEC_y)

# meny - simulace
EUR_dfp <- SIMUL(EUR_opt.theta,EUR_y)
USD_dfp <- SIMUL(USD_opt.theta,USD_y)
GBP_dfp <- SIMUL(GBP_opt.theta,GBP_y)
NOK_dfp <- SIMUL(NOK_opt.theta,NOK_y)
CHF_dfp <- SIMUL(CHF_opt.theta,CHF_y)
DKK_dfp <- SIMUL(DKK_opt.theta,DKK_y)


# krypto . simulace - grafy
BTC_g <- ggplot(BTC_dfp, aes(x = x, y = value)) + 
  geom_line(aes(linetype = variable, color = variable)) +
  theme(legend.position = "none", axis.title.x=element_blank(), axis.title.y=element_blank()) 

ETH_g <- ggplot(ETH_dfp, aes(x = x, y = value)) + 
  geom_line(aes(linetype = variable, color = variable)) +
  theme(legend.position = "none", axis.title.x=element_blank(), axis.title.y=element_blank()) 

LTC_g <- ggplot(LTC_dfp, aes(x = x, y = value)) + 
  geom_line(aes(linetype = variable, color = variable)) +
  theme(legend.position = "none", axis.title.x=element_blank(), axis.title.y=element_blank()) 

ZEC_g <- ggplot(ZEC_dfp, aes(x = x, y = value)) + 
  geom_line(aes(linetype = variable, color = variable)) +
  theme(legend.position = "none", axis.title.x=element_blank(), axis.title.y=element_blank()) 




# meny - simulace - grafy
EUR_g <- ggplot(EUR_dfp, aes(x = x, y = value)) + 
  geom_line(aes(linetype = variable, color = variable)) +
  theme(legend.position = "none", axis.title.x=element_blank(), axis.title.y=element_blank()) 

USD_g <- ggplot(USD_dfp, aes(x = x, y = value)) + 
  geom_line(aes(linetype = variable, color = variable)) +
  theme(legend.position = "none", axis.title.x=element_blank(), axis.title.y=element_blank()) 

GBP_g <- ggplot(GBP_dfp, aes(x = x, y = value)) + 
  geom_line(aes(linetype = variable, color = variable)) +
  theme(legend.position = "none", axis.title.x=element_blank(), axis.title.y=element_blank()) 

NOK_g <- ggplot(NOK_dfp, aes(x = x, y = value)) + 
  geom_line(aes(linetype = variable, color = variable)) +
  theme(legend.position = "none", axis.title.x=element_blank(), axis.title.y=element_blank()) 

CHF_g <- ggplot(CHF_dfp, aes(x = x, y = value)) + 
  geom_line(aes(linetype = variable, color = variable)) +
  theme(legend.position = "none", axis.title.x=element_blank(), axis.title.y=element_blank()) 

DKK_g <- ggplot(DKK_dfp, aes(x = x, y = value)) + 
  geom_line(aes(linetype = variable, color = variable)) +
  theme(legend.position = "none", axis.title.x=element_blank(), axis.title.y=element_blank()) 
