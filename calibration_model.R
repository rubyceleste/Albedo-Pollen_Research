library(gam)
library(mgcv)
library(ggplot2)

dat_all = readRDS('data/dat_all-winter.RDS')
dat_all = dat_all[which(dat_all$alb != 0),]
dat_all = dat_all[which(!is.na(dat_all$alb)),]
dat_all$alb = dat_all$alb/1000

ggplot()+    
  geom_polygon(data=world_proj, aes(x=x, y=y, group=group), fill='white')+
  geom_point(data=dat_all, aes(x=x,y=y, colour=alb))


#############################################################################
## MODEL 1
#############################################################################

mod = gam(alb ~ te(x, y, k=10) + s(ST, k=5) + s(OL, k=5) + s(ET, k=5), 
          data=dat_all, 
          family=binomial(link="logit"),
          method="REML")

gam.check(mod)

plot(mod)

plot(mod, residuals=TRUE, all.terms=TRUE, pch=1)

new_data = dat_all

preds = predict.gam(mod, 
                    type="response", 
                    data=new_data,
                    se.fit=TRUE)

dat_all$alb_pred = preds$fit

ggplot(data=dat_all) + 
  geom_point(aes(x=alb, y=alb_pred)) +
  geom_abline(slope=1, intercept=0, colour="red") +
  xlim(c(0,1)) + 
  ylim(c(0,1))

#############################################################################
## MODEL 2
#############################################################################

mod = gam(alb ~ te(x, y, k=8) + s(ST, k=5) + s(OL, k=5) + s(ET, k=5), 
          data=dat_all, 
          family=binomial(link="logit"),
          method="REML")

gam.check(mod)

plot(mod)

plot(mod, residuals=TRUE, all.terms=TRUE, pch=1)

new_data = dat_all

preds = predict.gam(mod, 
                    type="response", 
                    data=new_data,
                    se.fit=TRUE)

dat_all$alb_pred = preds$fit

ggplot(data=dat_all) + 
  geom_point(aes(x=alb, y=alb_pred)) +
  geom_abline(slope=1, intercept=0, colour="red") +
  xlim(c(0,1)) + 
  ylim(c(0,1))



#############################################################################
## MODEL 3
#############################################################################

mod = gam(alb ~ s(x, y, bs='tp', k=200) + s(ET, k=5) + s(ST) + s(OL, k=19), 
          data=dat_all, 
          family=gaussian(link="logit"), 
          method="REML")

gam.check(mod)

plot(mod)

plot(mod, residuals=TRUE, all.terms=TRUE, pch=1)

new_data = dat_all

preds = predict.gam(mod, 
                    type="response", 
                    data=new_data,
                    se.fit=TRUE)

dat_all$alb_pred = preds$fit

ggplot(data=dat_all) + 
  geom_point(aes(x=alb, y=alb_pred)) +
  geom_abline(slope=1, intercept=0, colour="red") +
  xlim(c(0,1)) + 
  ylim(c(0,1))


