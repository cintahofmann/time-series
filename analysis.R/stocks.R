#libraries
library(quantmod)
library(zoo)
library(Quandl)
library(dplyr)

#get the stocks
tickr <- c("GPRO", "VSLR", "TSLA", "NVDA", "AMD", "MSFT", "VXUS",
           "NFLX","BRK.B")
getSymbols(tickr, src="google", from="2005-01-01")

#Nur die Spalte der Schließung
GPRO.Close <- GPRO[,4] #immer nur die Closing-Spalte
VSLR.Close <- VSLR[,4] #bc that's obvious logic
TSLA.Close <- TSLA[,4] #being obvious
NVDA.Close <- NVDA[,4] #wink
AMD.Close <- AMD[,4]
MSFT.Close <- MSFT[,4]
VXUS.Close <- VXUS[,4]
NFLX.Close <- NFLX[,4]
BRK.B.Close <- BRK.B[,4]

#Für die division später
GPRO1 <- as.numeric(GPRO.Close[1])
VSLR1 <- as.numeric(VSLR.Close[1])
TSLA1 <- as.numeric(TSLA.Close[1])
NVDA1 <- as.numeric(NVDA.Close[1])
AMD1 <- as.numeric(AMD.Close[1])
MSFT1 <- as.numeric(MSFT.Close[1])
VXUS1 <- as.numeric(VXUS.Close[1])
NFLX1 <- as.numeric(NFLX.Close[1])
BRK.B1 <- as.numeric(BRK.B.Close[1])

#division
GPRO <- GPRO.Close/GPRO1
VSLR <- VSLR.Close/VSLR1
TSLA <- TSLA.Close/TSLA1
NVDA <- NVDA.Close/NVDA1
AMD <- AMD.Close/AMD1
MSFT <- MSFT.Close/MSFT1
VXUS <- VXUS.Close/VXUS1
NFLX <- NFLX.Close/NFLX1
BRK.B <- BRK.B.Close/BRK.B1

#na aus dem Index entfernen
GPRO <- GPRO[!is.na(index(GPRO))]
VSLR <- VSLR[!is.na(index(VSLR))]
TSLA <- TSLA[!is.na(index(TSLA))]
NVDA <- NVDA[!is.na(index(NVDA))]
AMD <- AMD[!is.na(index(AMD))]
MSFT <- MSFT[!is.na(index(MSFT))]
VXUS <- VXUS[!is.na(index(VXUS))]
NFLX <- NFLX[!is.na(index(NFLX))]
BRK.B <- BRK.B[!is.na(index(BRK.B))]

#merge into one matrix
basket <- cbind(GPRO, VSLR, TSLA, NVDA, AMD,MSFT,VXUS,NFLX,BRK.B)
is.xts(basket)
head(basket)

#als zoo object?
zoo.basket <- as.zoo(basket)

#set colour scheme
tsRainbow <- rainbow(ncol(zoo.basket))
plot(x=zoo.basket, ylab="Cumulative Return", main="Cumulative
     Returns", col=tsRainbow, screen=1)
legend(x="topleft", legend=c("GPRO", "VSLR", "TSLA", "NVDA",
                             "AMD","MSFT","VXUS", "NFLX","BRK.B"),
       lty=1,col=tsRainbow)

#save plot
dev.copy(jpeg, 'rainbow-returns-NFLX.jpeg')

#plot mit xts file
plot(x=basket[,"GPRO.Close"],xlab="Time",ylab="Cumulative
     Return",main="Cumulative Returns",ylim=c(0.0,12),
major.ticks="years",minor.ticks=FALSE,col="red")
lines(x=basket[,"VSLR.Close"],col="darkgreen")
lines(x=basket[,"TSLA.Close"],col="goldenrod")
lines(x=basket[,"NVDA.Close"],col="darkblue")
lines(x=basket[,"AMD.Close"],col="darkviolet")
lines(x=basket[,"MSFT.Close"],col="coral1")
lines(x=basket[,"VXUS.Close"],col="cadetblue")
legend(x='topleft',legend=c("GPRO","VSLR","TSLA","NVDA","AMD",
                   "MSFT","VXUS"),lty=1)

NFLX_NVDA <- lm(NFLX ~ NVDA)
summary(NFLX_NVDA)
plot(NFLX_NVDA)

TSLA_NVDA <- lm(TSLA ~ NVDA)
summary(TSLA_NVDA)

NVDA2 <- NVDA["2010-06-29/2016-11-23"]
TSLA_NVDA <- lm(TSLA ~ NVDA2)
attributes(plot(TSLA_NVDA))

dev.copy(jpeg, 'q-q-NVDA-NFLX')
