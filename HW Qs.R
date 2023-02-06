


# Write a function for converting odds to probability
odds2prob<-function(odds){odds/(1+odds)}
# fit with regression


##### Elevation:#####
# Plot combined
ggplot(wolfkde, aes(x=Elevation2, y=used)) + geom_point() +geom_jitter(aes(colour = used), width=0.25, height = 0.05)+ stat_smooth(method="glm", method.args = list(family="binomial"))
# Plot single packs
ggplot(wolfkde, aes(x=Elevation2, y=used, colour=pack)) + geom_point() + geom_jitter(width=0.25, height = 0.05) +stat_smooth(method="glm", method.args = list(family="binomial"), level=0.90)
# Facet
ggplot(wolfkde, aes(x=Elevation2, y=used)) + geom_point() + stat_smooth(method="glm", method.args = list(family="binomial"), level=0.90) + facet_grid(pack~.)
# Fit a model and interpret coefficients
bow_elev <- glm(used ~ Elevation2, family=binomial(logit), data=bow_wolf)
summary(bow_elev)
# get odds ratio
exp(coef(bow_elev))
# Fit a model and interpret coefficients
red_elev <- glm(used ~ Elevation2, family=binomial(logit), data=red_wolf)
summary(red_elev)
# get odds ratio
exp(coef(red_elev))
# Convert to probabilities  
#coef(red_elev)[2]
odds2prob(coef(red_elev)[2])


#### Distance to humans####
# Plot combined
ggplot(wolfkde, aes(x=DistFromHumanAccess2, y=used)) + geom_point() +geom_jitter(aes(colour = used), width=0.25, height = 0.05)+ stat_smooth(method="glm", method.args = list(family="binomial"))
# Plot single packs
ggplot(wolfkde, aes(x=DistFromHumanAccess2, y=used, colour=pack)) + geom_point() + geom_jitter(width=0.25, height = 0.05) +stat_smooth(method="glm", method.args = list(family="binomial"), level=0.90)
# Fit a model and interpret coefficients
bow_use <- glm(used ~ DistFromHumanAccess2, family=binomial(logit), data=bow_wolf)
summary(bow_use)
# get odds ratio
exp(coef(bow_use))
# Fit a model and interpret coefficients
red_use <- glm(used ~ DistFromHumanAccess2, family=binomial(logit), data=red_wolf)
summary(red_use)
# get odds ratio
exp(coef(red_use))
# Convert to probabilities  
#coef(red_elev)[2]
odds2prob(coef(red_use)[2])



#### Distance to high human use ####
# Plot combined
ggplot(wolfkde, aes(x=DistFromHighHumanAccess2, y=used)) + geom_point() +geom_jitter(aes(colour = used), width=0.25, height = 0.05)+ stat_smooth(method="glm", method.args = list(family="binomial"))
# Plot single packs
ggplot(wolfkde, aes(x=DistFromHighHumanAccess2, y=used, colour=pack)) + geom_point() + geom_jitter(width=0.25, height = 0.05) +stat_smooth(method="glm", method.args = list(family="binomial"), level=0.90)
# Fit a model and interpret coefficients
bow_highuse <- glm(used ~ DistFromHighHumanAccess2, family=binomial(logit), data=bow_wolf)
summary(bow_highuse)
# get odds ratio
exp(coef(bow_highuse))
# Fit a model and interpret coefficients
red_highuse <- glm(used ~ DistFromHighHumanAccess2, family=binomial(logit), data=red_wolf)
summary(red_use)
# get odds ratio
exp(coef(red_highuse))
# Convert to probabilities  
#coef(red_elev)[2]
odds2prob(coef(red_highuse)[2])



###### Deer #####
# Plot combined
ggplot(wolfkde, aes(x=deer_w2, y=used)) + geom_point() +geom_jitter(aes(colour = used), width=0.25, height = 0.05)+ stat_smooth(method="glm", method.args = list(family="binomial"))
# Plot single packs
ggplot(wolfkde, aes(x=deer_w2, y=used, colour=pack)) + geom_point() + geom_jitter(width=0.25, height = 0.05) +stat_smooth(method="glm", method.args = list(family="binomial"), level=0.90)
# Fit a model and interpret coefficients
bow_deer <- glm(used ~ deer_w2, family=binomial(logit), data=bow_wolf)
summary(bow_deer)
# get odds ratio
exp(coef(bow_deer))
# Fit a model and interpret coefficients
red_deer <- glm(used ~ deer_w2, family=binomial(logit), data=red_wolf)
summary(red_deer)
# get odds ratio
exp(coef(red_deer))
# Convert to probabilities  
#coef(red_elev)[2]
odds2prob(coef(red_deer)[2])


##### Moose HSI #####
# Plot combined
ggplot(wolfkde, aes(x=moose_w2, y=used)) + geom_point() +geom_jitter(aes(colour = used), width=0.25, height = 0.05)+ stat_smooth(method="glm", method.args = list(family="binomial"))
# Plot single packs
ggplot(wolfkde, aes(x=moose_w2, y=used, colour=pack)) + geom_point() + geom_jitter(width=0.25, height = 0.05) +stat_smooth(method="glm", method.args = list(family="binomial"), level=0.90)
# Fit a model and interpret coefficients
bow_moose <- glm(used ~ moose_w2, family=binomial(logit), data=bow_wolf)
summary(bow_deer)
# get odds ratio
exp(coef(bow_moose))
# Fit a model and interpret coefficients
red_moose <- glm(used ~ moose_w2, family=binomial(logit), data=red_wolf)
summary(red_moose)
# get odds ratio
exp(coef(red_moose))
# Convert to probabilities  
#coef(red_elev)[2]
odds2prob(coef(red_moose)[2])


##### Elk HSI #####
# Plot combined
ggplot(wolfkde, aes(x=elk_w2, y=used)) + geom_point() +geom_jitter(aes(colour = used), width=0.25, height = 0.05)+ stat_smooth(method="glm", method.args = list(family="binomial"))
# Plot single packs
ggplot(wolfkde, aes(x=elk_w2, y=used, colour=pack)) + geom_point() + geom_jitter(width=0.25, height = 0.05) +stat_smooth(method="glm", method.args = list(family="binomial"), level=0.90)
# Fit a model and interpret coefficients
bow_elk <- glm(used ~ elk_w2, family=binomial(logit), data=bow_wolf)
summary(bow_elk)
# get odds ratio
exp(coef(bow_elk))
# Fit a model and interpret coefficients
red_elk <- glm(used ~ elk_w2, family=binomial(logit), data=red_wolf)
summary(red_elk)
# get odds ratio
exp(coef(red_elk))
# Convert to probabilities  
#coef(red_elev)[2]
odds2prob(coef(red_elk)[2])


##### Sheep HSI #####
# Plot combined
ggplot(wolfkde, aes(x=sheep_w2, y=used)) + geom_point() +geom_jitter(aes(colour = used), width=0.25, height = 0.05)+ stat_smooth(method="glm", method.args = list(family="binomial"))
# Plot single packs
ggplot(wolfkde, aes(x=sheep_w2, y=used, colour=pack)) + geom_point() + geom_jitter(width=0.25, height = 0.05) +stat_smooth(method="glm", method.args = list(family="binomial"), level=0.90)
# Fit a model and interpret coefficients
bow_sheep <- glm(used ~ sheep_w2, family=binomial(logit), data=bow_wolf)
summary(bow_sheep)
# get odds ratio
exp(coef(bow_sheep))
# Fit a model and interpret coefficients
red_sheep <- glm(used ~ sheep_w2, family=binomial(logit), data=red_wolf)
summary(red_sheep)
# get odds ratio
exp(coef(red_sheep))
# Convert to probabilities  
#coef(red_elev)[2]
odds2prob(coef(red_sheep)[2])


##### Goat HSI #####
# Plot combined
ggplot(wolfkde, aes(x=goat_w2, y=used)) + geom_point() +geom_jitter(aes(colour = used), width=0.25, height = 0.05)+ stat_smooth(method="glm", method.args = list(family="binomial"))
# Plot single packs
ggplot(wolfkde, aes(x=goat_w2, y=used, colour=pack)) + geom_point() + geom_jitter(width=0.25, height = 0.05) +stat_smooth(method="glm", method.args = list(family="binomial"), level=0.90)
# Fit a model and interpret coefficients
bow_goat <- glm(used ~ goat_w2, family=binomial(logit), data=bow_wolf)
summary(bow_goat)
# get odds ratio
exp(coef(bow_goat))
# Fit a model and interpret coefficients
red_goat <- glm(used ~ goat_w2, family=binomial(logit), data=red_wolf)
summary(red_sheep)
# get odds ratio
exp(coef(red_goat))
# Convert to probabilities  
#coef(red_elev)[2]
odds2prob(coef(red_goat)[2])



##### Table ####
# put the coefficients into a table














sheep <- glm(used ~ sheep_w2, family=binomial(logit), data=wolfkde)
summary(sheep)
# get odds ratio
exp(coef(sheep))
habvalues = 0:7
deer <- glm(used ~ deer_w2, family=binomial(logit), data=wolfkde)
summary(deer)
# get odds ratio 
exp(coef(deer))
#2.85 times - strong effects, wolf prob of use increases by 2.85 for each increse in HSI for deer
elk <- glm(used ~ elk_w2, family=binomial(logit), data=wolfkde)
summary(elk)
exp(coef(deer))
moose <- glm(used ~ moose_w2, family=binomial(logit), data=wolfkde)
summary(moose)
exp(coef(moose))
goat <- glm(used ~ goat_w2, family=binomial(logit), data=wolfkde)
summary(goat)
exp(coef(goat))





