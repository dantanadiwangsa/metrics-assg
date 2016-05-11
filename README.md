# metrics-assg

ajr <- read.csv("683896.csv")

#Q1
GDPpc <- exp(ajr$logGDPpc)
hist(GDPpc, main="Histogram of GDP per capita", xlab="GDP per capita")

#Q2a
library(AER)
library(texreg)
eq1 <- lm(logGDPpc~AvExprRisk, data=ajr)

plot(ajr$AvExprRisk, ajr$logGDPpc,
     xlab="Average Expropriation Risk",
     ylab="log(GDP per capita)")
abline(eq1, col="red")

#Q2c
eq2<- lm(AvExprRisk~logSettMort, data=ajr)

plot(ajr$logSettMort, ajr$AvExprRisk,
     xlab="log(Settler Mortality)",
     ylab="Average Expropriation Risk")
abline(eq2, col="red")

#Q2d
eq3 <- ivreg(logGDPpc~AvExprRisk|logSettMort, data=ajr)
test3 <- coeftest(eq3, vcov=vcovHC(eq3))

t.cv <- qt(0.975, df=58)
beta1.U <- test3[2,1] + t.cv*test3[2,2]
beta1.L <- test3[2,1] - t.cv*test3[2,2]
print(cbind(beta1.L, beta1.U))

#Q3a
eq4 <- lm(logGDPpc~Latitude, data=ajr)

#Q3b
eq5 <- ivreg(logGDPpc~AvExprRisk+Latitude|logSettMort+Latitude, data=ajr)

#Q4a
eq6 <- lm(logGDPpc~British, data=ajr)

#Q4b
eq7 <- ivreg(logGDPpc~AvExprRisk+British|logSettMort+British, data=ajr)

#Q5b
eq8 <- lm(AvExprRisk~logSettMort+Democracy1900, data=ajr)

Wald5b <- linearHypothesis(eq8, c("logSettMort=0", "Democracy1900=0"),
                          test="Chisq", vcov=vcovHC(eq8))
print(Wald5b)

#Q5c
eq9 <- ivreg(logGDPpc~AvExprRisk|logSettMort+Democracy1900, data=ajr)

eqall <- list(coeftest(eq1, vcov=vcovHC(eq1)),
              coeftest(eq2, vcov=vcovHC(eq2)),
              coeftest(eq3, vcov=vcovHC(eq3)),
              coeftest(eq4, vcov=vcovHC(eq4)),
              coeftest(eq5, vcov=vcovHC(eq5)),
              coeftest(eq6, vcov=vcovHC(eq6)),
              coeftest(eq7, vcov=vcovHC(eq7)),
              coeftest(eq8, vcov=vcovHC(eq8)),
              coeftest(eq9, vcov=vcovHC(eq9)))
htmlreg(eqall,file="eqall.doc", 
        custom.model.names = c("2a", "2c", "2d", "3a", "3b", "4a", "4b", "5b", "5c"),
        digits=3)
