library(tidyverse)
library(GGally)
install.packages("regclass")
library(regclass)
install.packages("pROC")
library(pROC)
install.packages("rsample")
library(rsample)
library(readxl)
library(tidyverse)
library(ggplot2)
library(dplyr)
install.packages("VIF")
library(VIF)
getwd()
toyota <- read.csv("/Users/gabrielengel/Downloads/Ayudantia_DataMining01_2021-main/Ayudantia 9/toyota.csv")

vinos <- read.csv("/Users/gabrielengel/Downloads/Ayudantia_DataMining01_2021-main/Ayudantia 9/winequality-red.csv")

summary(toyota)

toyota %>% head()



toyota$model <- as.factor(toyota$model)
toyota$transmission <- as.factor(toyota$transmission)
toyota$fuelType <- as.factor(toyota$fuelType)

summary(toyota)

toyota %>% filter(engineSize == 0) %>% nrow()

toyota <- toyota %>%  filter(engineSize != 0)

toyota %>% filter(tax == 0) %>% nrow()

## es normal


sum(is.na(toyota))
sum(is.null(toyota))


##listos los datos

toyota %>% select(year, mileage, tax, mpg, engineSize, price) %>% 
  ggpairs(lower = list(continuous = wrap("points", alpha = 0.3, size = 0.5)))


toyota %>% 
  ggplot(aes(transmission, price)) +
  geom_boxplot()

toyota %>% 
  ggplot(aes(fuelType, price)) +
  geom_boxplot()

toyota %>% mutate(model = reorder(model, price)) %>%
  ggplot(aes(price, model)) +
  geom_boxplot()

toyota %>% ggplot(aes(mileage, price)) +
  geom_point(alpha = .1) +
  stat_smooth(method = "gam", formula = y ~ s(x, k=3))


toyota %>% ggplot(aes(year, price)) +
  geom_point(alpha = .1) +
  stat_smooth(method = "gam", formula = y ~ s(x, k=3))

toyota %>% ggplot(aes(mpg, price)) +
  geom_point(alpha = .1) +
  stat_smooth(method = "gam", formula = y ~ s(x, k=3))

toyota %>% ggplot(aes(engineSize, price)) +
  geom_point(alpha = .1) +
  stat_smooth(method = "gam", formula = y ~ s(x, k=3))

toyota %>% filter(., year >= 2005) %>% ggplot(aes(year, price)) +
  geom_point(alpha = .1) +
  stat_smooth(method = "gam", formula = y ~ s(x, k=3))

toyota_esc <- toyota
toyota[,c(2,3,5,7,8,9)] <- scale(audi_sca[,c(2,3,5,7,8,9)])

toyota_esc %>%  head()


reg_simp <- lm(price ~ mileage, data = toyota)
summary(reg_simp)

##los parametros son 0.0001479 para el interecpto y -0.0009901 para el coeficiente asociado a la variable de superficie de terreno

##el coeficiente de determinacion R2 es de 0.08888 lo que significa que el 8.8% de la varianza del precio esta explicado por este modelo, lo cual es poco.



reg_mult <- lm(price ~ model + year*mileage + engineSize + mpg, data = toyota_esc)
summary(reg_mult)


VIF(reg_mult)

install.packages("olsrr")
library(olsrr)


fuerza_bruta <- ols_step_all_possible(reg_mult)

plot(fuerza_bruta)


#reg lineal de vinos

summary(vinos)

vinos %>% filter(citric.acid == 0) %>% nrow()
#es noraml

sum(is.na(vinos))
sum(is.null(vinos))
#no hay na o null


vinos %>% head()
glimpse(vinos)





ggplot(vinos,aes(x=factor(quality))) +
  geom_bar(col ="black",fill="#993333",alpha=0.5) +
  theme(axis.text.x = element_text(face="bold", size=10)) +
  scale_x_discrete("Hotel") +
  scale_y_continuous("Count")


set.seed(123)

glm.fit <- glm(quality, data = vinos , family = "binomial")
