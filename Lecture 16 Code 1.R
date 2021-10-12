setwd("~/Dropbox/Own/Teaching/Stat 420 Spring 2021/Lecture/Lecture 16")
skin <- read.csv("skin.csv")
latitude = skin$latitude
mortality = skin$mortality
state = skin$state
ocean = skin$ocean

plot(latitude, mortality, type="n")
text(latitude, mortality, as.character(state), col=ocean+1)

fit = lm(mortality ~ latitude + ocean)
summary(fit)

fit1 = lm(mortality ~ latitude)
anova(fit1,fit)

abline(fit$coeff[1],fit$coeff[2],col=1,lty=1)
abline(fit$coeff[1]+fit$coeff[3],fit$coeff[2],col=2,lty=2)


fit2 = lm(mortality ~ latitude + ocean + latitude*ocean )
summary(fit2)

anova(fit, fit2)



fit2 = lm(mortality ~ latitude + ocean + I(latitude*ocean) )



lm(mortality ~ latitude*ocean )
lm(mortality ~ latitude + ocean + latitude*ocean )
lm(mortality ~ latitude + ocean + I(latitude*ocean) )


lm(mortality ~ I(latitude*ocean) )