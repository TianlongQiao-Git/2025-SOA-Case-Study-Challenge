# 2025-SOA-Case-Study-Challenge
The code is specified for 2025 SOA Case Study Challenge from Goldteam SWUFE :)
# 2025-SOA-Case-Study-Challenge
# The R code is specified for 2025 SOA Case Study Challenge from Goldteam SWUFE :)
rm(list = ls())
library(readxl)
data <- read_excel("D:\\SOA Case Challenge\\2025-dam data_副本(1).xlsx")

table(data$hazard_level)
table(data$Region)
Flumevale <- data[data$Region=='Flumevale',]
Lyndrassia <- data[data$Region=='Lyndrassia',]
Navaldia <- data[data$Region=='Navaldia',]

# Define blue gradient
blue_gradient <- colorRampPalette(c("#002FA7","lightblue"))
# Generate 8 blue gradient colors
blue_colors <- blue_gradient(10)
## ============9*9 fitted distribution=========================
# =====fitting probability========
library(fitdistrplus)
category1 <- c('1A','1B')
category2 <- c('1C','1D','1E','2A','2B','2C','2D','2E','3A')
category3 <- c('3B','3C','3D','3E')
## Flumevale Cat1
Flumevale_fit_norm_1 <- fitdist(Flumevale$'Probability of Failure'[Flumevale$`haz*ass` %in% category1],
                                "norm")
Flumevale_fit_gamma_1 <- fitdist(Flumevale$'Probability of Failure'[Flumevale$`haz*ass` %in% category1],
                               "gamma")
Flumevale_fit_weibull_1 <- fitdist(Flumevale$'Probability of Failure'[Flumevale$`haz*ass` %in% category1],
                                 "weibull")
Flumevale_fit_exp_1 <- fitdist(Flumevale$'Probability of Failure'[Flumevale$`haz*ass` %in% category1],
                   "exp")
plot(Flumevale_fit_exp_1) # 显然不行，下面比较gamma和weibull
summary(Flumevale_fit_norm_1)
summary(Flumevale_fit_gamma_1)
summary(Flumevale_fit_weibull_1)
# Flumevale_fit_weibull_1 is better
plot(Flumevale_fit_weibull_1)

## Flumevale Cat2
Flumevale_fit_norm_2 <- fitdist(Flumevale$'Probability of Failure'[Flumevale$`haz*ass` %in% category2],
                                "norm")
Flumevale_fit_gamma_2 <- fitdist(Flumevale$'Probability of Failure'[Flumevale$`haz*ass` %in% category2],
                                 "gamma")
Flumevale_fit_weibull_2 <- fitdist(Flumevale$'Probability of Failure'[Flumevale$`haz*ass` %in% category2],
                                   "weibull")
summary(Flumevale_fit_norm_2)
summary(Flumevale_fit_gamma_2)
summary(Flumevale_fit_weibull_2)
# Flumevale_fit_gamma_2 is better
plot(Flumevale_fit_gamma_2)

## Flumevale Cat3
Flumevale_fit_norm_3 <- fitdist(Flumevale$'Probability of Failure'[Flumevale$`haz*ass` %in% category3],
                                 "norm")
Flumevale_fit_gamma_3 <- fitdist(Flumevale$'Probability of Failure'[Flumevale$`haz*ass` %in% category3],
                                 "gamma")
Flumevale_fit_weibull_3 <- fitdist(Flumevale$'Probability of Failure'[Flumevale$`haz*ass` %in% category3],
                                   "weibull")
summary(Flumevale_fit_norm_3)
summary(Flumevale_fit_gamma_3)
summary(Flumevale_fit_weibull_3)
# Flumevale_fit_gamma_3 is better
plot(Flumevale_fit_gamma_3)


## Lyndrassia Cat1
Lyndrassia_fit_norm_1 <- fitdist(Lyndrassia$'Probability of Failure'[Lyndrassia$`haz*ass` %in% category1],
                                "norm")
Lyndrassia_fit_gamma_1 <- fitdist(Lyndrassia$'Probability of Failure'[Lyndrassia$`haz*ass` %in% category1],
                                 "gamma")
Lyndrassia_fit_weibull_1 <- fitdist(Lyndrassia$'Probability of Failure'[Lyndrassia$`haz*ass` %in% category1],
                                   "weibull")
Lyndrassia_fit_exp_1 <- fitdist(Lyndrassia$'Probability of Failure'[Lyndrassia$`haz*ass` %in% category1],
                               "exp")
plot(Lyndrassia_fit_exp_1) # 显然不行，下面比较gamma和weibull
summary(Lyndrassia_fit_norm_1)
summary(Lyndrassia_fit_gamma_1)
summary(Lyndrassia_fit_weibull_1)
# Lyndrassia_fit_gamma_1 is better
plot(Lyndrassia_fit_gamma_1)

## Lyndrassia Cat2
Lyndrassia_fit_norm_2 <- fitdist(Lyndrassia$'Probability of Failure'[Lyndrassia$`haz*ass` %in% category2],
                                 "norm")
Lyndrassia_fit_gamma_2 <- fitdist(Lyndrassia$'Probability of Failure'[Lyndrassia$`haz*ass` %in% category2],
                                  "gamma")
Lyndrassia_fit_weibull_2 <- fitdist(Lyndrassia$'Probability of Failure'[Lyndrassia$`haz*ass` %in% category2],
                                    "weibull")
summary(Lyndrassia_fit_norm_2)
summary(Lyndrassia_fit_gamma_2)
summary(Lyndrassia_fit_weibull_2)
# Lyndrassia_fit_gamma_2 is better
plot(Lyndrassia_fit_gamma_2)

## Lyndrassia Cat3
Lyndrassia_fit_norm_3 <- fitdist(Lyndrassia$'Probability of Failure'[Lyndrassia$`haz*ass` %in% category3],
                                 "norm")
Lyndrassia_fit_gamma_3 <- fitdist(Lyndrassia$'Probability of Failure'[Lyndrassia$`haz*ass` %in% category3],
                                  "gamma")
Lyndrassia_fit_weibull_3 <- fitdist(Lyndrassia$'Probability of Failure'[Lyndrassia$`haz*ass` %in% category3],
                                    "weibull")
Lyndrassia_fit_unit_3 <- fitdist(Lyndrassia$'Probability of Failure'[Lyndrassia$`haz*ass` %in% category3],
                                    "unif")
summary(Lyndrassia_fit_norm_3)
summary(Lyndrassia_fit_gamma_3)
summary(Lyndrassia_fit_weibull_3)
summary(Lyndrassia_fit_unit_3)
# Lyndrassia_fit_unit_3更好
plot(Lyndrassia_fit_unit_3)


## Navaldia Cat1
Navaldia_fit_norm_1 <- fitdist(Navaldia$'Probability of Failure'[Navaldia$`haz*ass` %in% category1],
                                 "norm")
Navaldia_fit_gamma_1 <- fitdist(Navaldia$'Probability of Failure'[Navaldia$`haz*ass` %in% category1],
                                  "gamma")
Navaldia_fit_weibull_1 <- fitdist(Navaldia$'Probability of Failure'[Navaldia$`haz*ass` %in% category1],
                                    "weibull")

summary(Navaldia_fit_norm_1)
summary(Navaldia_fit_gamma_1)
summary(Navaldia_fit_weibull_1)
# Navaldia_fit_gamma_1 is better
plot(Navaldia_fit_gamma_1)
## Navaldia Cat2
Navaldia_fit_norm_2 <- fitdist(Navaldia$'Probability of Failure'[Navaldia$`haz*ass` %in% category2],
                               "norm")
Navaldia_fit_gamma_2 <- fitdist(Navaldia$'Probability of Failure'[Navaldia$`haz*ass` %in% category2],
                                "gamma")
Navaldia_fit_weibull_2 <- fitdist(Navaldia$'Probability of Failure'[Navaldia$`haz*ass` %in% category2],
                                  "weibull")

summary(Navaldia_fit_norm_2)
summary(Navaldia_fit_gamma_2)
summary(Navaldia_fit_weibull_2)
# Navaldia_fit_gamma_2 is better
plot(Navaldia_fit_gamma_2)

## Navaldia Cat3
Navaldia_fit_norm_3 <- fitdist(Navaldia$'Probability of Failure'[Navaldia$`haz*ass` %in% category3],
                               "norm")
Navaldia_fit_gamma_3 <- fitdist(Navaldia$'Probability of Failure'[Navaldia$`haz*ass` %in% category3], "gamma")
Navaldia_fit_weibull_3 <- fitdist(Navaldia$'Probability of Failure'[Navaldia$`haz*ass` %in% category3], "weibull")
Navaldia_fit_beta_3 <- fitdist(Navaldia$'Probability of Failure'[Navaldia$`haz*ass` %in% category3],
                                  "beta",start = list(shape1 = 1, shape2 = 1))
summary(Navaldia_fit_norm_3)
summary(Navaldia_fit_gamma_3)
summary(Navaldia_fit_weibull_3)
summary(Navaldia_fit_beta_3)

# Navaldia_fit_norm_3 is better
plot(Navaldia_fit_beta_3)
##总结：分别是Flumevale_fit_weibull_1，Flumevale_fit_gamma_2,Flumevale_fit_gamma_3,
##            Lyndrassia_fit_gamma_1,Lyndrassia_fit_gamma_2,Lyndrassia_fit_unit_3,
##            Navaldia_fit_gamma_1,Navaldia_fit_gamma_2,Navaldia_fit_norm_3




# =====fitting total_loss=========
## Flumevale Cat1
Flumevale_fit_norm_loss_1 <- fitdist(Flumevale$total_loss[Flumevale$`haz*ass` %in% category1],
                                "norm")
Flumevale_fit_gamma_loss_1 <- fitdist(Flumevale$total_loss[Flumevale$`haz*ass` %in% category1],
                                 "gamma")
Flumevale_fit_weibull_loss_1 <- fitdist(Flumevale$total_loss[Flumevale$`haz*ass` %in% category1],
                                   "weibull")
Flumevale_fit_exp_loss_1 <- fitdist(Flumevale$total_loss[Flumevale$`haz*ass` %in% category1],
                               "exp")
plot(Flumevale_fit_exp_loss_1) # 显然不行，下面比较gamma和weibull
summary(Flumevale_fit_norm_loss_1)
summary(Flumevale_fit_gamma_loss_1)
summary(Flumevale_fit_weibull_loss_1)
# Flumevale_fit_weibull_loss_1 is better 

## Flumevale Cat2
Flumevale_fit_norm_loss_2 <- fitdist(Flumevale$total_loss[Flumevale$`haz*ass` %in% category2],
                                     "norm")
Flumevale_fit_gamma_loss_2 <- fitdist(Flumevale$total_loss[Flumevale$`haz*ass` %in% category2],
                                      "gamma")
Flumevale_fit_weibull_loss_2 <- fitdist(Flumevale$total_loss[Flumevale$`haz*ass` %in% category2],
                                        "weibull")
summary(Flumevale_fit_norm_loss_2)
summary(Flumevale_fit_gamma_loss_2)
summary(Flumevale_fit_weibull_loss_2)
plot(Flumevale_fit_weibull_loss_2)
# Flumevale_fit_weibull_loss_2更好

## Flumevale Cat3
Flumevale_fit_norm_loss_3 <- fitdist(Flumevale$total_loss[Flumevale$`haz*ass` %in% category3],
                                     "norm")
Flumevale_fit_gamma_loss_3 <- fitdist(Flumevale$total_loss[Flumevale$`haz*ass` %in% category3],
                                      "gamma")
Flumevale_fit_weibull_loss_3 <- fitdist(Flumevale$total_loss[Flumevale$`haz*ass` %in% category3],
                                        "weibull")
summary(Flumevale_fit_norm_loss_3)
summary(Flumevale_fit_gamma_loss_3)
summary(Flumevale_fit_weibull_loss_3)
plot(Flumevale_fit_gamma_loss_3)
# Flumevale_fit_gamma_loss_3 is better

## Lyndrassia Cat1
Lyndrassia_fit_norm_loss_1 <- fitdist(Lyndrassia$total_loss[Lyndrassia$`haz*ass` %in% category1],
                                     "norm")
Lyndrassia_fit_gamma_loss_1 <- fitdist(Lyndrassia$total_loss[Lyndrassia$`haz*ass` %in% category1],
                                      "gamma")
Lyndrassia_fit_weibull_loss_1 <- fitdist(Lyndrassia$total_loss[Lyndrassia$`haz*ass` %in% category1],
                                        "weibull")
summary(Lyndrassia_fit_norm_loss_1)
summary(Lyndrassia_fit_gamma_loss_1)
summary(Lyndrassia_fit_weibull_loss_1)
# Lyndrassia_fit_gamma_loss_1 is better

## Lyndrassia Cat 2
Lyndrassia_fit_norm_loss_2 <- fitdist(Lyndrassia$total_loss[Lyndrassia$`haz*ass` %in% category2],
                                      "norm")
Lyndrassia_fit_gamma_loss_2 <- fitdist(Lyndrassia$total_loss[Lyndrassia$`haz*ass` %in% category2],
                                       "gamma")
Lyndrassia_fit_weibull_loss_2 <- fitdist(Lyndrassia$total_loss[Lyndrassia$`haz*ass` %in% category2],
                                         "weibull")
summary(Lyndrassia_fit_norm_loss_2)
summary(Lyndrassia_fit_gamma_loss_2)
summary(Lyndrassia_fit_weibull_loss_2)
plot(Lyndrassia_fit_weibull_loss_2)
# Lyndrassia_fit_weibull_loss_2 is better

## Lyndrassia Cat3
Lyndrassia_fit_norm_loss_3 <- fitdist(Lyndrassia$total_loss[Lyndrassia$`haz*ass` %in% category3],
                                      "norm")
Lyndrassia_fit_gamma_loss_3 <- fitdist(Lyndrassia$total_loss[Lyndrassia$`haz*ass` %in% category3],
                                       "gamma")
Lyndrassia_fit_weibull_loss_3 <- fitdist(Lyndrassia$total_loss[Lyndrassia$`haz*ass` %in% category3],
                                         "weibull")
summary(Lyndrassia_fit_norm_loss_3)
summary(Lyndrassia_fit_gamma_loss_3)
summary(Lyndrassia_fit_weibull_loss_3)
plot(Lyndrassia_fit_weibull_loss_3)
# Lyndrassia_fit_weibull_loss_3 is better


## Navaldia Cat1
Navaldia_fit_norm_loss_1 <- fitdist(Navaldia$total_loss[Navaldia$`haz*ass` %in% category1],
                                      "norm")
Navaldia_fit_gamma_loss_1 <- fitdist(Navaldia$total_loss[Navaldia$`haz*ass` %in% category1],
                                       "gamma")
Navaldia_fit_weibull_loss_1 <- fitdist(Navaldia$total_loss[Navaldia$`haz*ass` %in% category1],
                                         "weibull")
summary(Navaldia_fit_norm_loss_1)
summary(Navaldia_fit_gamma_loss_1)
summary(Navaldia_fit_weibull_loss_1)
plot(Navaldia_fit_weibull_loss_1)
# Navaldia_fit_weibull_loss_1 is better

## Navaldia Cat2
Navaldia_fit_norm_loss_2 <- fitdist(Navaldia$total_loss[Navaldia$`haz*ass` %in% category2],
                                    "norm")
Navaldia_fit_gamma_loss_2 <- fitdist(Navaldia$total_loss[Navaldia$`haz*ass` %in% category2],
                                     "gamma")
Navaldia_fit_weibull_loss_2 <- fitdist(Navaldia$total_loss[Navaldia$`haz*ass` %in% category2],
                                       "weibull")
summary(Navaldia_fit_norm_loss_2)
summary(Navaldia_fit_gamma_loss_2)
summary(Navaldia_fit_weibull_loss_2)
plot(Navaldia_fit_norm_loss_2)
# Navaldia_fit_norm_loss_2 is better

## Navaldia Cat3
Navaldia_fit_norm_loss_3 <- fitdist(Navaldia$total_loss[Navaldia$`haz*ass` %in% category3],
                                    "norm")
Navaldia_fit_gamma_loss_3 <- fitdist(Navaldia$total_loss[Navaldia$`haz*ass` %in% category3],
                                     "gamma")
Navaldia_fit_weibull_loss_3 <- fitdist(Navaldia$total_loss[Navaldia$`haz*ass` %in% category3],
                                       "weibull")
summary(Navaldia_fit_norm_loss_3)
summary(Navaldia_fit_gamma_loss_3)
summary(Navaldia_fit_weibull_loss_3)
plot(Navaldia_fit_gamma_loss_3)
# Navaldia_fit_gamma_loss_3 is better

## Flumevale_fit_weibull_loss_1,Flumevale_fit_weibull_loss_2,Flumevale_fit_gamma_loss_3
## Lyndrassia_fit_weibull_loss_1,Lyndrassia_fit_weibull_loss_2,Lyndrassia_fit_weibull_loss_3
## Navaldia_fit_weibull_loss_1,Navaldia_fit_norm_loss_2,Navaldia_fit_gamma_loss_3



# =====Calculating Premiums and Quartiles=======

# Flumevale Cat1
# 95% quartile
F1_L_q_95 <- qweibull(0.95, shape = Flumevale_fit_weibull_loss_1$estimate["shape"], scale = Flumevale_fit_weibull_loss_1$estimate["scale"])
F1_q_95<- qweibull(0.95, shape=Flumevale_fit_weibull_1$estimate['shape'],scale=Flumevale_fit_weibull_1$estimate['scale'])
F1_q95 <- F1_L_q_95*F1_q_95
F1_q95
# Premium
Flumevale_fit_weibull_loss_1$estimate["scale"]*gamma(1 + 1/Flumevale_fit_weibull_loss_1$estimate["shape"])*Flumevale_fit_norm_1$estimate['mean']

# Flumevale Cat2
# 95% quartile
F2_L_q_95 <- qweibull(0.95, shape = Flumevale_fit_weibull_loss_2$estimate["shape"], scale = Flumevale_fit_weibull_loss_2$estimate["scale"])
F2_q_95<- qgamma(0.95, shape=Flumevale_fit_gamma_2$estimate['shape'],rate=Flumevale_fit_gamma_2$estimate['rate'])
F2_q95 <- F2_L_q_95*F2_q_95
F2_q95
# premium
Flumevale_fit_weibull_loss_2$estimate["scale"]*gamma(1 + 1/Flumevale_fit_weibull_loss_2$estimate["shape"])*Flumevale_fit_gamma_2$estimate['shape']/Flumevale_fit_gamma_2$estimate['rate']

# Flumevale Cat3
# 95% quartile
F3_L_q_95 <- qgamma(0.95, shape = Flumevale_fit_gamma_loss_3$estimate["shape"], rate = Flumevale_fit_gamma_loss_3$estimate["rate"])
F3_q_95<- qgamma(0.95, shape=Flumevale_fit_gamma_3$estimate['shape'],rate=Flumevale_fit_gamma_3$estimate['rate'])
F3_q95 <- F3_L_q_95*F2_q_95
F3_q95
# premium
Flumevale_fit_gamma_loss_3$estimate['shape']/Flumevale_fit_gamma_loss_3$estimate['rate']*Flumevale_fit_gamma_3$estimate['shape']/Flumevale_fit_gamma_3$estimate['rate']

# Lyndrassia Cat1
# 95% quartile
L1_L_q_95 <- qweibull(0.95, shape = Lyndrassia_fit_weibull_loss_1$estimate["shape"], scale = Lyndrassia_fit_weibull_loss_1$estimate["scale"])
L1_q_95<- qgamma(0.95, shape=Lyndrassia_fit_gamma_1$estimate['shape'],rate=Lyndrassia_fit_gamma_1$estimate['rate'])
L1_q95 <- L1_L_q_95*L1_q_95
L1_q95
# 保费
Lyndrassia_fit_weibull_loss_1$estimate["scale"]*gamma(1 + 1/Lyndrassia_fit_weibull_loss_1$estimate["shape"])*Lyndrassia_fit_gamma_1$estimate['shape']/Lyndrassia_fit_gamma_1$estimate['rate']

# Lyndrassia Cat2
# 95% quartile
L2_L_q_95 <- qweibull(0.95, shape = Lyndrassia_fit_weibull_loss_2$estimate["shape"], scale = Lyndrassia_fit_weibull_loss_2$estimate["scale"])
L2_q_95<- qgamma(0.95, shape=Lyndrassia_fit_gamma_2$estimate['shape'],rate=Lyndrassia_fit_gamma_2$estimate['rate'])
L2_q95 <- L2_L_q_95*L2_q_95
L2_q95
# Premium
Lyndrassia_fit_weibull_loss_2$estimate["scale"]*gamma(1 + 1/Lyndrassia_fit_weibull_loss_2$estimate["shape"])*Lyndrassia_fit_gamma_2$estimate['shape']/Lyndrassia_fit_gamma_2$estimate['rate']

# Lyndrassia Cat3
# 95% quartile
L3_L_q_95 <- qweibull(0.95, shape = Lyndrassia_fit_weibull_loss_3$estimate["shape"], scale = Lyndrassia_fit_weibull_loss_3$estimate["scale"])
L3_q_95<- qunif(0.95, min=Lyndrassia_fit_unit_3$estimate['min'],max=Lyndrassia_fit_unit_3$estimate['max'])
L3_q95 <- L3_L_q_95*L3_q_95
L3_q95
# 保费
Lyndrassia_fit_weibull_loss_3$estimate["scale"]*gamma(1 + 1/Lyndrassia_fit_weibull_loss_3$estimate["shape"])*(Lyndrassia_fit_unit_3$estimate['min']+Lyndrassia_fit_unit_3$estimate['max'])/2

# Navaldia Cat1
# 95% quartile
N1_L_q_95 <- qweibull(0.95, shape = Navaldia_fit_weibull_loss_1$estimate["shape"], scale = Navaldia_fit_weibull_loss_1$estimate["scale"])
N1_q_95<- qgamma(0.95, shape=Navaldia_fit_gamma_1$estimate['shape'],rate=Navaldia_fit_gamma_1$estimate['rate'])
N1_q95 <- N1_L_q_95*N1_q_95
N1_q95
# premium
Navaldia_fit_weibull_loss_1$estimate["scale"]*gamma(1 + 1/Navaldia_fit_weibull_loss_1$estimate["shape"])*Navaldia_fit_gamma_1$estimate['shape']/Navaldia_fit_gamma_1$estimate['rate']

# Navaldia Cat2
# 95% quartile
N2_L_q_95 <- qnorm(0.95, mean = Navaldia_fit_norm_loss_2$estimate["mean"], sd = Navaldia_fit_norm_loss_2$estimate["sd"])
N2_q_95<- qgamma(0.95, shape=Navaldia_fit_gamma_2$estimate['shape'],rate=Navaldia_fit_gamma_2$estimate['rate'])
N2_q95 <- N2_L_q_95*N2_q_95
N2_q95
# Premium
Navaldia_fit_norm_loss_2$estimate["mean"]*Navaldia_fit_gamma_2$estimate['shape']/Navaldia_fit_gamma_2$estimate['rate']


# Navaldia cat3
# 95% quartil
N3_L_q_95 <- qgamma(0.95, shape = Navaldia_fit_gamma_loss_3$estimate["shape"], rate = Navaldia_fit_gamma_loss_3$estimate["rate"])
N3_q_95<- qnorm(0.95, mean=Navaldia_fit_norm_3$estimate['mean'],sd=Navaldia_fit_norm_3$estimate['sd'])
N3_q95 <- N3_L_q_95*N3_q_95
N3_q95
# Premium
Navaldia_fit_norm_3$estimate["mean"]*Navaldia_fit_gamma_loss_3$estimate['shape']/Navaldia_fit_gamma_loss_3$estimate['rate']


## Graph
?denscomp
par(mfrow = c(3, 3),mar = c(4, 4, 2, 2), oma = c(4, 4, 4, 4))
denscomp(Flumevale_fit_weibull_loss_1,main='',datacol = blue_colors,fitcol='darkred',fitlwd=1.5)
abline(v = F1_L_q_95,col='black',lwd=1.5,lty=2)
denscomp(Flumevale_fit_weibull_loss_2,main='',datacol = c(rev(blue_colors)[6:10],blue_colors[2:10]),fitcol='darkred',fitlwd=1.5)
abline(v = F2_L_q_95,col='black',lwd=1.5,lty=2)
denscomp(Flumevale_fit_gamma_loss_3,main='',datacol = blue_colors,fitcol='darkred',fitlwd=1.5)
abline(v = F3_L_q_95,col='black',lwd=1.5,lty=2)
denscomp(Lyndrassia_fit_weibull_loss_1,main='',datacol = c(rev(blue_colors)[9:10],blue_colors[2:10]),fitcol='darkred',fitlwd=1.5)
abline(v = L1_L_q_95,col='black',lwd=1.5,lty=2)
denscomp(Lyndrassia_fit_weibull_loss_2,main='',datacol = blue_colors,fitcol='darkred',fitlwd=1.5)
abline(v = L2_L_q_95,col='black',lwd=1.5,lty=2)
denscomp(Lyndrassia_fit_weibull_loss_3,main='',datacol = c(rev(blue_colors)[8:10],blue_colors[2],blue_colors[1:10]),fitcol='darkred',fitlwd=1.5)
abline(v = L3_L_q_95,col='black',lwd=1.5,lty=2)
denscomp(Navaldia_fit_weibull_loss_1,main='',datacol = blue_colors,fitcol='darkred',fitlwd=1.5)
abline(v = N1_L_q_95,col='black',lwd=1.5,lty=2)
denscomp(Navaldia_fit_norm_loss_2,main='',datacol = c(rev(blue_colors)[6:10],blue_colors[2:10]),fitcol='darkred',fitlwd=1.5)
abline(v = N2_L_q_95,col='black',lwd=1.5,lty=2)
denscomp(Navaldia_fit_gamma_loss_3,main='',datacol = c(rev(blue_colors)[9:10],blue_colors[2:10]),fitcol='darkred',fitlwd=1.5)
abline(v = N3_L_q_95,col='black',lwd=1.5,lty=2)

# col names
mtext("Category1", side = 3, outer = TRUE, at = 0.15, line = 0, cex = 0.8)
mtext("Category2", side = 3, outer = TRUE, at = 0.5, line = 0, cex = 0.8)
mtext("Category3", side = 3, outer = TRUE, at = 0.85, line = 0, cex = 0.8)
# row names
mtext("Flumevale", side = 2, outer = TRUE, at = 0.85, line = 0, cex = 0.8)
mtext("Lyndrassia", side = 2, outer = TRUE, at = 0.5, line = 0, cex = 0.8)
mtext("Navaldia", side = 2, outer = TRUE, at = 0.15, line = 0, cex = 0.8)
mtext("Fitting Results of Total Loss", side = 3, outer = TRUE, line = 2, cex = 1.2)

# Graph
par(mfrow = c(3, 3),mar = c(4, 4, 2, 2), oma = c(4, 4, 4, 4))
denscomp(Flumevale_fit_weibull_1,main='',datacol = c(rev(blue_colors),blue_colors[2:10]),fitcol='darkred',fitlwd=1.5)
abline(v = F1_q_95,col='black',lwd=1.5,lty=2)
denscomp(Flumevale_fit_gamma_2,main='',datacol = c(rev(blue_colors)[3:10],blue_colors[2:10]),fitcol='darkred',fitlwd=1.5)
abline(v = F2_q_95,col='black',lwd=1.5,lty=2)
denscomp(Flumevale_fit_gamma_3,main='',datacol = c(rev(blue_colors)[7:10],blue_colors[2:10]),fitcol='darkred',fitlwd=1.5)
abline(v = F3_q_95,col='black',lwd=1.5,lty=2)
denscomp(Lyndrassia_fit_gamma_1,main='',datacol = c(rev(blue_colors)[6:10],blue_colors[2:10]),fitcol='darkred',fitlwd=1.5)
abline(v = L1_q_95,col='black',lwd=1.5,lty=2)
denscomp(Lyndrassia_fit_gamma_2,main='',datacol = c(rev(blue_colors)[5:10],blue_colors[2:10]),fitcol='darkred',fitlwd=1.5)
abline(v = L2_q_95,col='black',lwd=1.5,lty=2)
denscomp(Lyndrassia_fit_unit_3,main='',datacol = blue_colors,fitcol='darkred',fitlwd=1.5)
abline(v = L3_q_95,col='black',lwd=1.5,lty=2)
denscomp(Navaldia_fit_gamma_1,main='',datacol = c(rev(blue_colors)[7:10],blue_colors[2:10]),fitcol='darkred',fitlwd=1.5)
abline(v = N1_q_95,col='black',lwd=1.5,lty=2)
denscomp(Navaldia_fit_gamma_2,main='',datacol = c(rev(blue_colors)[5:10],blue_colors[2:10]),fitcol='darkred',fitlwd=1.5)
abline(v = N2_q_95,col='black',lwd=1.5,lty=2)
denscomp(Navaldia_fit_norm_3,main='',datacol = c(rev(blue_colors)[4:10],blue_colors[2:10]),fitcol='darkred',fitlwd=1.5)
abline(v = N3_q_95,col='black',lwd=1.5,lty=2)
# col names
mtext("Category1", side = 3, outer = TRUE, at = 0.15, line = 0, cex = 0.8)
mtext("Category2", side = 3, outer = TRUE, at = 0.5, line = 0, cex = 0.8)
mtext("Category3", side = 3, outer = TRUE, at = 0.85, line = 0, cex = 0.8)
# row names
# mtext("Flumevale", side = 2, outer = TRUE, at = 0.85, line = 0, cex = 0.8)
mtext("Lyndrassia", side = 2, outer = TRUE, at = 0.5, line = 0, cex = 0.8)
mtext("Navaldia", side = 2, outer = TRUE, at = 0.15, line = 0, cex = 0.8)
mtext("Fitting Results of Failure Probability", side = 3, outer = TRUE, line = 2, cex = 1.2)
# ============Predict Assessment_level================
library(readxl)
data <- read_excel("D:\\SOA Case Challenge\\2025-dam data_副本(1).xlsx")
names(data)
#处理hazard
data$hazard_level[is.na(data$hazard_level)]  <- 1
# Missing values are considered Not Rated
data$assessmen_levelAis.na(data$assessmen_level)] <- 'Not Rated'
 Missing Values Filling for Other Characteristic Variables
 data$'assessment_level'[is.na(data$'data$assessmen_level')]<- 'Not Rated'
data$'Primary Purpose'[is.na(data$'Primary Purpose')] <- 'Recreation'
data$'Primary Type'[is.na(data$'Primary Type')] <- 'Earth'
data$'Year Completed' <- as.numeric(data$'Year Completed')

data$'Year Completed'[is.na(data$'Year Completed')] <- mean(data$'Year Completed'[!is.na(data$'Year Completed')])
data$`Surface (km2)` <- as.numeric(data$`Surface (km2)`)
data$`Surface (km2)`[is.na(data$`Surface (km2)`)] <- mean(data$`Surface (km2)`[!is.na(data$`Surface (km2)`)])

Rated <- data[data$assessmen_level!='Not Rated',]
NotRated <- data[data$assessmen_level=='Not Rated',]

##多项逻辑回归
library(nnet)
Rated$assessmen_level <- as.factor(Rated$assessmen_level)
table(Rated$`Surface (km2)`)
model <- multinom(assessmen_level ~ Region+`Regulated Dam`+`Primary Purpose`+`Primary Type`+`Year Completed`+`Surface (km2)`,data = Rated)

summary(model)
# Prediction
result <- predict(model,NotRated,type = "class")
NotRated$assessmen_level <- result
library(openxlsx)
write.xlsx(Rated, file = "D:\\Rated.xlsx")
write.xlsx(NotRated, file = "D:\\NotRated.xlsx")
