rm(list = ls())

# nastavit cestu k souboru
# potreba upravit pro kontrolu scriptu
ROOT <- "D:/Documents/FIS VŠE/02 MAGISTR/3. semestr/Pokročilá ekonometrie 2/seminárka"
setwd(ROOT)

library(readxl)
library(nloptr)
library(ggplot2)
library(gridExtra)
library(reshape2)
# xls soubory - kryptomeny
BTC <- read_excel("gemini_BTCUSD_day_1rok.xls")
ETH <- read_excel("gemini_ETHUSD_day_1rok.xls")
LTC <- read_excel("gemini_LTCUSD_day_1rok.xls")
ZEC <- read_excel("gemini_ZECUSD_day_1rok.xls")

# xls soubor - meny
meny <- read_excel("mena_1rok.xls")

############################## funkce ######################
############################## GARCH  ######################

# optimalizace parametru pomoci balicku "nloptr"
# vice informaci na:
# https://nlopt.readthedocs.io/en/latest/NLopt_Algorithms/

GARCH = function(theta,y) {
  
  # pocet pozorovani
  T = length(y)
  
  # parametry
  omega = theta[1]
  alpha = theta[2]
  beta =  theta[3]
  
  # volatilita
  sig2 = rep(NA,T)
  sig2[1] = var(y)
  
  # GARCH model
  for (t in 1:(T-1))
    {
      sig2[t+1] = omega + alpha * y[t]^2 + beta * sig2[t]
    }
  
  # log likelihood
  l = -(1/2)*log(2*pi) - (1/2)*log(sig2) - (1/2)*y^2/sig2
  llik = sum(l)
  
  # budeme ve funkci minimalizovat
  return(-llik)
}
#############################################################################

# inicializacni theta pro otimalizace
start.theta = c(0.0001,0.03,0.5)


############################## kryptomena #####################
##############################     BTC   ######################

# vstup
BTC_y = as.vector(BTC$Open)

# optimalizace
BTC_opt.result <- nloptr(x0 = start.theta,
                     lb = c(1e-4, 1e-4, 1e-4),
                     ub = c(Inf, Inf, 0.99999),
                     eval_f = GARCH, y = BTC_y,
                     opts = list(algorithm = 'NLOPT_LN_PRAXIS', xtol_rel = 0, maxeval = 1e8))
# odhadnute parametry
BTC_opt.theta <- BTC_opt.result$solution
# likelihood
BTC_opt.likelihood <- -BTC_opt.result$objective

############################## kryptomena #####################
##############################     ETH   ######################

# vstup
ETH_y = as.vector(ETH$Open)

# optimalizace
ETH_opt.result <- nloptr(x0 = start.theta,
                         lb = c(1e-4, 1e-4, 1e-4),
                         ub = c(Inf, Inf, 0.99999),
                         eval_f = GARCH, y = ETH_y,
                         opts = list(algorithm = 'NLOPT_LN_PRAXIS', xtol_rel = 0, maxeval = 1e8))
# odhadnute parametry
ETH_opt.theta <- ETH_opt.result$solution
# likelihood
ETH_opt.likelihood <- -ETH_opt.result$objective

############################## kryptomena #####################
##############################     LTC   ######################

# vstup
LTC_y = as.vector(LTC$Open)

# optimalizace
LTC_opt.result <- nloptr(x0 = start.theta,
                         lb = c(1e-4, 1e-4, 1e-4),
                         ub = c(Inf, Inf, 0.99999),
                         eval_f = GARCH, y = LTC_y,
                         opts = list(algorithm = 'NLOPT_LN_PRAXIS', xtol_rel = 0, maxeval = 1e8))
# odhadnute parametry
LTC_opt.theta <- LTC_opt.result$solution
# likelihood
LTC_opt.likelihood <- -LTC_opt.result$objective


############################## kryptomena #####################
##############################     ZEC   ######################

# vstup
ZEC_y = as.vector(ZEC$Open)

# optimalizace
ZEC_opt.result <- nloptr(x0 = start.theta,
                         lb = c(1e-4, 1e-4, 1e-4),
                         ub = c(Inf, Inf, 0.99999),
                         eval_f = GARCH, y = ZEC_y,
                         opts = list(algorithm = 'NLOPT_LN_PRAXIS', xtol_rel = 0, maxeval = 1e8))
# odhadnute parametry
ZEC_opt.theta <- ZEC_opt.result$solution
# likelihood
ZEC_opt.likelihood <- -ZEC_opt.result$objective


############################## mena #####################
##############################     EUR   ######################

# vstup
EUR_y = as.vector(meny$EUR)

# optimalizace
EUR_opt.result <- nloptr(x0 = start.theta,
                         lb = c(1e-4, 1e-4, 1e-4),
                         ub = c(Inf, Inf, 0.99999),
                         eval_f = GARCH, y = EUR_y,
                         opts = list(algorithm = 'NLOPT_LN_PRAXIS', xtol_rel = 0, maxeval = 1e8))
# odhadnute parametry
EUR_opt.theta <- EUR_opt.result$solution
# likelihood
EUR_opt.likelihood <- -EUR_opt.result$objective


############################## mena #####################
##############################     USD   ######################

# vstup
USD_y = as.vector(meny$USD)

# optimalizace
USD_opt.result <- nloptr(x0 = start.theta,
                         lb = c(1e-4, 1e-4, 1e-4),
                         ub = c(Inf, Inf, 0.99999),
                         eval_f = GARCH, y = USD_y,
                         opts = list(algorithm = 'NLOPT_LN_PRAXIS', xtol_rel = 0, maxeval = 1e8))
# odhadnute parametry
USD_opt.theta <- USD_opt.result$solution
# likelihood
USD_opt.likelihood <- -USD_opt.result$objective

############################## mena #####################
##############################     GBP   ######################

# vstup
GBP_y = as.vector(meny$GBP)

# optimalizace
GBP_opt.result <- nloptr(x0 = start.theta,
                         lb = c(1e-4, 1e-4, 1e-4),
                         ub = c(Inf, Inf, 0.99999),
                         eval_f = GARCH, y = GBP_y,
                         opts = list(algorithm = 'NLOPT_LN_PRAXIS', xtol_rel = 0, maxeval = 1e8))
# odhadnute parametry
GBP_opt.theta <- GBP_opt.result$solution
# likelihood
GBP_opt.likelihood <- -GBP_opt.result$objective


############################## mena #####################
##############################     NOK   ######################

# vstup
NOK_y = as.vector(meny$NOK)

# optimalizace
NOK_opt.result <- nloptr(x0 = start.theta,
                         lb = c(1e-4, 1e-4, 1e-4),
                         ub = c(Inf, Inf, 0.99999),
                         eval_f = GARCH, y = NOK_y,
                         opts = list(algorithm = 'NLOPT_LN_PRAXIS', xtol_rel = 0, maxeval = 1e8))
# odhadnute parametry
NOK_opt.theta <- NOK_opt.result$solution
# likelihood
NOK_opt.likelihood <- -NOK_opt.result$objective


############################## mena #####################
##############################     CHF   ######################

# vstup
CHF_y = as.vector(meny$CHF)

# optimalizace
CHF_opt.result <- nloptr(x0 = start.theta,
                         lb = c(1e-4, 1e-4, 1e-4),
                         ub = c(Inf, Inf, 0.99999),
                         eval_f = GARCH, y = CHF_y,
                         opts = list(algorithm = 'NLOPT_LN_PRAXIS', xtol_rel = 0, maxeval = 1e8))
# odhadnute parametry
CHF_opt.theta <- CHF_opt.result$solution
# likelihood
CHF_opt.likelihood <- -CHF_opt.result$objective


############################## mena #####################
##############################     DKK   ######################

# vstup
DKK_y = as.vector(meny$DKK)

# optimalizace
DKK_opt.result <- nloptr(x0 = start.theta,
                         lb = c(1e-4, 1e-4, 1e-4),
                         ub = c(Inf, Inf, 0.99999),
                         eval_f = GARCH, y = DKK_y,
                         opts = list(algorithm = 'NLOPT_LN_PRAXIS', xtol_rel = 0, maxeval = 1e8))
# odhadnute parametry
DKK_opt.theta <- DKK_opt.result$solution
# likelihood
DKK_opt.likelihood <- -DKK_opt.result$objective



###############################################################
######################### vysledne tabulky ####################
###############################################################


# odhadnute parametry - kryptomeny
tab_krypt = cbind(c(round(BTC_opt.theta,4)),
            c(round(ETH_opt.theta,4)),
            c(round(LTC_opt.theta,4)),
            c(round(ZEC_opt.theta,4)))
colnames(tab_krypt) = c('BTC','ETH','LTC','ZEC')
rownames(tab_krypt) = c('omega', 'alpha', 'beta')


# odhadnute parametry - meny
tab_meny = cbind(c(round(EUR_opt.theta,4)),
                  c(round(USD_opt.theta,4)),
                  c(round(GBP_opt.theta,4)),
                  c(round(NOK_opt.theta,4)),
                  c(round(CHF_opt.theta,4)),
                  c(round(DKK_opt.theta,4)))
colnames(tab_meny) = c('EUR', 'USD', 'GBP', 'NOK', 'CHF', 'DKK')
rownames(tab_meny) = c('omega', 'alpha', 'beta')


################################################
#################### tabulky zde ###############
################################################

tab_krypt
tab_meny


################################################
##################### grafy ####################
################################################

############################## meny

g01 <- ggplot(meny, aes(x = datum, y = Kč) ) +
  geom_line( aes(y = EUR, color="EUR"), size = 1) +
  geom_line( aes(y = USD, color="USD"), size = 1) +
  geom_line( aes(y = GBP, color="GBP"), size = 1) +
  geom_line( aes(y = CHF, color="CHF"), size = 1) +
  ylim(c(20,32)) +
  #ggtitle("Vyrazne hodnotnejsi meny nez Kč") +
  theme_bw()

g02 <- ggplot(meny, aes(x = datum, y = Kč) ) +
  geom_line( aes(y = NOK, color="NOK"), size = 1) +
  geom_line( aes(y = DKK, color="DKK"), size = 1) +
  ylim(c(2,4)) +
  #ggtitle("Lehce hodnotnejsi meny nez Kč") +
  theme_bw()


############################# kryptomeny

g03 <- ggplot(BTC,aes(x = Date, y = USD) ) +
  geom_line( aes(y =     Open), color="yellowgreen", size = 1) +
  ylim(c(28000,69000)) +
  #ggtitle("Vývoj Bitcoinu (BTC) oproti americkemu dolaru") +
  theme_bw()


g04 <- ggplot(BTC,aes(x = Date, y = USD) ) +
  geom_line( aes(y = ETH$Open), color="cyan3", size = 1) +
  ylim(c(1000,5000)) +
  #ggtitle("Vývoj Etheria (ETH) oproti americkemu dolaru") +
  theme_bw()

g05 <- ggplot(LTC,aes(x = Date, y = USD) ) +
  geom_line( aes(y = Open, color="LTC"), size = 1, show.legend = TRUE) +
  geom_line( aes(y = ZEC$Open, color="ZEC"), size = 1,show.legend = TRUE) +
  ylim(c(50,400)) +
  #ggtitle("Vývoj slabsich kryptomen oproti americkemu dolaru") +
  theme_bw()

g01
g02
g03
g04
g05

# grafy do semestralky - export v .pdf (potreba manualne menit cisla 1:5)
# pro bezpeci ctenare zakomentovno
# -> cairo_pdf bude vytvaret novy pdf soubory
# -> dev.off bude ukoncovat tvorene souboru
# porzn. nesmi byt soubory otevrene, jinak chyba

#cairo_pdf(filename = "g01.pdf", 
#          width = 6, height = 3.5)
#{
#  g01
#}
#dev.off()








######################### funkce   ###############################
######################### simulace ###############################
######################### GARCH    ###############################

SIMUL = function (theta,y) {
  
  # pocet pozorovani
  T = length(y)
  
  # parametry
  omega = theta[1]
  alpha = theta[2]
  beta  = theta[3]
  sig1  = abs(omega/(1-alpha-beta))       # ne vzdy kladne cislo, proto absolutni hodnota
  
  # inovace
  epsilon = rnorm(T)
  
  # volatilita
  sig = rep(0,T)
  x = rep(0,T)
  sig[1] = sig1
  
  # GARCH model
  for (t in 1:T) {
    x[t] = sqrt(sig[t]) * epsilon[t]
    sig[t+1] = omega + alpha * x[t]^2 + beta * sig[t]
  }
  
  df = data.frame(sig = sqrt(sig[1:T]), y = x)
  df_g = melt(df)
  df_g$x = rep(1:T,2)
  
  return(df_g)
}

# krypto - simulace
BTC_df_g <- SIMUL(BTC_opt.theta,BTC_y)
ETH_df_g <- SIMUL(ETH_opt.theta,ETH_y)
LTC_df_g <- SIMUL(LTC_opt.theta,LTC_y)
ZEC_df_g <- SIMUL(ZEC_opt.theta,ZEC_y)

# meny - simulace
EUR_df_g <- SIMUL(EUR_opt.theta,EUR_y)
USD_df_g <- SIMUL(USD_opt.theta,USD_y)
GBP_df_g <- SIMUL(GBP_opt.theta,GBP_y)
NOK_df_g <- SIMUL(NOK_opt.theta,NOK_y)
CHF_df_g <- SIMUL(CHF_opt.theta,CHF_y)
DKK_df_g <- SIMUL(DKK_opt.theta,DKK_y)


# krypto - simulace - grafy
BTC_g <- ggplot(BTC_df_g, aes(x = x, y = value)) + 
  geom_line(aes(linetype = variable, color = variable)) +
  theme(legend.position = "none", axis.title.x=element_blank(), axis.title.y=element_blank()) 

ETH_g <- ggplot(ETH_df_g, aes(x = x, y = value)) + 
  geom_line(aes(linetype = variable, color = variable)) +
  theme(legend.position = "none", axis.title.x=element_blank(), axis.title.y=element_blank()) 

LTC_g <- ggplot(LTC_df_g, aes(x = x, y = value)) + 
  geom_line(aes(linetype = variable, color = variable)) +
  theme(legend.position = "none", axis.title.x=element_blank(), axis.title.y=element_blank()) 

ZEC_g <- ggplot(ZEC_df_g, aes(x = x, y = value)) + 
  geom_line(aes(linetype = variable, color = variable)) +
  theme(legend.position = "none", axis.title.x=element_blank(), axis.title.y=element_blank()) 




# meny - simulace - grafy
EUR_g <- ggplot(EUR_df_g, aes(x = x, y = value)) + 
  geom_line(aes(linetype = variable, color = variable)) +
  theme(legend.position = "none", axis.title.x=element_blank(), axis.title.y=element_blank()) 

USD_g <- ggplot(USD_df_g, aes(x = x, y = value)) + 
  geom_line(aes(linetype = variable, color = variable)) +
  theme(legend.position = "none", axis.title.x=element_blank(), axis.title.y=element_blank()) 

GBP_g <- ggplot(GBP_df_g, aes(x = x, y = value)) + 
  geom_line(aes(linetype = variable, color = variable)) +
  theme(legend.position = "none", axis.title.x=element_blank(), axis.title.y=element_blank()) 

NOK_g <- ggplot(NOK_df_g, aes(x = x, y = value)) + 
  geom_line(aes(linetype = variable, color = variable)) +
  theme(legend.position = "none", axis.title.x=element_blank(), axis.title.y=element_blank()) 

CHF_g <- ggplot(CHF_df_g, aes(x = x, y = value)) + 
  geom_line(aes(linetype = variable, color = variable)) +
  theme(legend.position = "none", axis.title.x=element_blank(), axis.title.y=element_blank()) 

DKK_g <- ggplot(DKK_df_g, aes(x = x, y = value)) + 
  geom_line(aes(linetype = variable, color = variable)) +
  theme(legend.position = "none", axis.title.x=element_blank(), axis.title.y=element_blank()) 



#cairo_pdf(filename = "sim_LTC.pdf", 
#          width = 6, height = 3.5)
#{
#  LTC_g
#}
#dev.off()



