library(readxl)
library(ggplot2)
library(tidyr)
library(reshape2)

data <- read_excel("ENB2012_data.xlsx")
data <- as.data.frame(scale(data))

str(data)

# Data Cleaning

is.null(data$X1)
is.null(data$X2)
is.null(data$X3)
is.null(data$X4)
is.null(data$X5)
is.null(data$X6)
is.null(data$X7)
is.null(data$X8)
is.null(data$Y1)
is.null(data$Y2)


# using Heating Load

fit_data_x1 <- lm(data = data, Y1~X1)
fit_data_x2 <- lm(data = data, Y1~X2)
fit_data_x3 <- lm(data = data, Y1~X3)
fit_data_x4 <- lm(data = data, Y1~X4)
fit_data_x5 <- lm(data = data, Y1~X5)
fit_data_x6 <- lm(data = data, Y1~X6)
fit_data_x7 <- lm(data = data, Y1~X7)
fit_data_x8 <- lm(data = data, Y1~X8)


par(mfrow=c(2,3))
plot(data$Y1~data$X1, main = "Compactness")
abline(fit_data_x1)

plot(data$Y1~data$X2, main = "Surface Area")
abline(fit_data_x2)

plot(data$Y1~data$X3, main = "Wall Area")
abline(fit_data_x3)

plot(data$Y1~data$X4, main = "Roof Area")
abline(fit_data_x4)

plot(data$Y1~data$X5, main = "Overall Height") # not significant
abline(fit_data_x5)

plot(data$Y1~data$X6, main = "Orientation") # not significant
abline(fit_data_x6)

plot(data$Y1~data$X6, main = "Glazing Area") # not significant
abline(fit_data_x7)

plot(data$Y1~data$X6, main = "Glazing Distribution") # not significant
abline(fit_data_x8)


# Using Cooling load

fit_data_x1 <- lm(data = data, Y2~X1)
fit_data_x2 <- lm(data = data, Y2~X2)
fit_data_x3 <- lm(data = data, Y2~X3)
fit_data_x4 <- lm(data = data, Y2~X4)
fit_data_x5 <- lm(data = data, Y2~X5)
fit_data_x6 <- lm(data = data, Y2~X6)
fit_data_x7 <- lm(data = data, Y2~X7)
fit_data_x8 <- lm(data = data, Y2~X8)


par(mfrow=c(2,3))
plot(data$Y2~data$X1, main = "Compactness")
abline(fit_data_x1)

plot(data$Y2~data$X2, main = "Surface Area")
abline(fit_data_x2)

plot(data$Y2~data$X3, main = "Wall Area")
abline(fit_data_x3)

plot(data$Y2~data$X4, main = "Roof Area")
abline(fit_data_x4)

plot(data$Y2~data$X5, main = "Overall Height") # not significant
abline(fit_data_x5)

plot(data$Y2~data$X6, main = "Orientation") # not significant
abline(fit_data_x6)

plot(data$Y2~data$X6, main = "Glazing Area") # not significant
abline(fit_data_x7)

plot(data$Y2~data$X6, main = "Glazing Distribution") # not significant
abline(fit_data_x8)


library(readxl)
library(ggplot2)
data_temp <- read_excel("ENB2012_data.xlsx")
data_temp <- as.data.frame(scale(data_temp))

colnames(data_temp) <- c("Relative_Compactness", "Surface_Area", "Wall_Area", "Roof_Area", "Overall_Height", "Orientation", "Glazing_Area", "Glazing_Distribution", "Heating_Load", "Cooling_Load")


data_long <- tidyr::gather(data, key = "variable", value = "value")

# Distribution Graphs

ggplot(data_long, aes(x = value)) +
  geom_histogram(fill = "#908269", color = "#434343", binwidth = 0.1) +
  facet_wrap(~ variable, scales = "free") +
  labs(title = "Distribution of Variables")

ggplot(data_long, aes(x = variable, y = value)) +
  geom_boxplot() +
  labs(x = "Attribute", y = "Value", title = "Boxplot for Each Attribute") +
  facet_wrap(~variable, scales = "free")

# Correlation Matrix 
cor_matrix <- cor(data)
cor_matrix_long <- melt(cor_matrix)

ggplot(cor_matrix_long, aes(Var1, Var2)) +
  geom_tile(aes(fill = value), color = "NA") +
  geom_text(aes(label = round(value, 2)), color = "white") +  # Add correlation values as text
  scale_fill_gradient2(low = "#908269", mid = "grey", high = "#434343", midpoint = 0, na.value = "grey50") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Correlation Matrix Plot")


data_long <- tidyr::gather(data, key = "variable", value = "value")

ggplot(data_long, aes(x = value)) +
  geom_histogram(fill = "#908269", color = "#434343", binwidth = 0.1) +
  facet_wrap(~ variable, scales = "free") +
  labs(title = "Distribution of Variables")


#PCA

library(FactoMineR)
library(ggcorrplot)
library(corrr)
library(factoextra)

pca_temp <- subset(data_temp, select = -Cooling_Load)

corr_mat <- cor(pca_temp)
ggcorrplot(corr_mat,type = 'lower', hc.order = T, lab = T)

cat(" ")

data_pca <- princomp(corr_mat)
summary(data_pca)

#from the summary(data_pca) the first principle component explains about 85% fo the total variance. We can say about 85% of the data can be represented in the first principal component. The second principle component explains about 6 % of the total variance. From this data, we conclude that the first two (mostly the first) accurately represent the data. 

data_pca$loadings[,1:2]

# The magnitude of the loadings indicate the variables contribution to the component. The sign of the loadings indicates wether the variable is positively or negativley correlated to the principal component.

fviz_eig(data_pca, addlabels = T, barfill = "#908269", barcolor = "black")

# visualizing the principle components importance 

fviz_pca_var(data_pca, col.var = "black")

# variables that are grouped together are positively correlated to each other. The magnitude of the distance from the variable to the origin indicates how well the variable is represented.

fviz_cos2(data_pca, choice = "var", axes = 1:2, fill = "#908269", color = "black")

# low values indicate that the variable is not well represented by the first two principle components. A high value is the opposite. cos^2 is a good metric to use for this instance. 
# Roof Area, Overall Height, and Surface Area are the top three variables with the highest Cos^2 value, hence contributing the most to Principal component 1 and 2.

fviz_pca_var(data_pca, col.var = "cos2", 
             gradient.cols = c("grey" , "#908269", "black"),
             repel = TRUE)

# Biplot with the color representing the cos^2 value. 


# Summary: 

linear_r_model <- lm(Heating_Load ~ Relative_Compactness + Surface_Area + Wall_Area + Roof_Area + Overall_Height + Orientation + Glazing_Area + Glazing_Distribution, data = data_temp)

linear_r_model <- lm(Cooling_Load ~ Relative_Compactness + Surface_Area + Wall_Area + Roof_Area + Overall_Height + Orientation + Glazing_Area + Glazing_Distribution, data = data_temp)

linear_r_model <- lm(cbind(Heating_Load, Cooling_Load) ~ Relative_Compactness + Surface_Area + Wall_Area + Roof_Area + Overall_Height + Orientation + Glazing_Area + Glazing_Distribution, data = data_temp)
summary(linear_r_model )


library(car)

lin_model <- lm(Heating_Load ~ Relative_Compactness + Surface_Area + Wall_Area + Roof_Area + Overall_Height + Orientation + Glazing_Area + Glazing_Distribution, data = data_temp)

# check normality assumption 
model_res <- lin_model$residuals

hist(model_res, breaks = 20, col = "#908269")
# histogram indicates that the residuals are mostly normal with some skew to the left 

qqnorm(model_res)
qqline(model_res)
# qqplot of residuals indicates that the residuals mostly do not follow a normal distribution. we know this since many of the points do not fall on the line. 

# next we will find the best model using the data from PCA. we will build a model without orientation, wall area, glazing area, and glazing distribution 

lin_model_2 <- lm(Heating_Load ~ Relative_Compactness + Surface_Area + Wall_Area + Overall_Height + Glazing_Area + Glazing_Distribution, data = data_temp)

anova(lin_model, lin_model_2)

vif(lin_model_2)



#stepwise subset 

int_only <- lm(data = data_temp, Heating_Load  ~ 1)

all <- lm(Heating_Load ~ Relative_Compactness + Surface_Area + Wall_Area + Roof_Area + Overall_Height + Orientation + Glazing_Area + Glazing_Distribution, data = data_temp)

both <- step(int_only, direction = 'both', scope = formula(all), trace = 0)

both$anova

both$coefficients



library(pls)

set.seed(123)

pcr_model <- pcr(Heating_Load ~ Relative_Compactness + Surface_Area + Wall_Area + Roof_Area + Overall_Height + Orientation + Glazing_Area + Glazing_Distribution, data = data_temp, validation = "CV")

summary(pcr_model)

validationplot(pcr_model, val.type="MSEP")


# split data for predictions
# split 70% for training and 30% for test

sample <- sample(c(TRUE, FALSE), nrow(data_temp), replace=TRUE, prob=c(0.7,0.3))

train_data <- data_temp[sample, ]
temp <- data_temp[!sample, ]
test_data <- subset(temp, select = -Heating_Load)
test_data_y <- subset(temp, select = Heating_Load)


model_train <- pcr(Heating_Load ~ Relative_Compactness + Surface_Area + Wall_Area + Roof_Area + Overall_Height + Orientation + Glazing_Area + Glazing_Distribution, data = train_data, validation = "CV")

predict_pcr <- predict(model_train, test_data, ncomp = 2)

sqrt(mean((as.numeric(predict_pcr) - test_data_y$Heating_Load)^2))

mod_summ <- summary(model_train)

model_train$coefficients

