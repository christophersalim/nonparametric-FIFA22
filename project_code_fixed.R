rm(list = ls())

library(dplyr)
library(tidyverse)
# load the data
data <- read_csv("./dataset/players_22.csv")

# after checking the head, we need to eliminate 
# a lot of unnecessary columns
head(data)

# there are multiple parameters associated with attacking
# such as finishing, crossing, volley kicks, etc.
# We want to average all of them into just 1 attacking parameter
avg_param <- function(df, param_name) {
  df %>% select(starts_with(param_name)) %>% 
    rowMeans(na.rm=TRUE)
}

data$attacking_avg <- avg_param(data, "attacking")
# do the same for other parameters
data$skill_avg <- avg_param(data, "skill")
data$movement_avg <- avg_param(data, "movement")
data$power_avg <- avg_param(data, "power")
data$mentality_avg <- avg_param(data, "mentality")
data$defending_avg <- avg_param(data, "defending")
data$goalkeeping_avg <- avg_param(data, "goalkeeping")
data$league_level <- factor(data$league_level)
data$international_reputation <- factor(data$international_reputation)

cols_selected <- c("long_name","value_eur",
                   "age", "league_level", "international_reputation",
                   "physic","attacking_avg", "club_contract_valid_until", 
                   "skill_avg","movement_avg", 
                   "power_avg","mentality_avg", 
                   "defending_avg")

data_selected <- data %>% select(cols_selected) %>% mutate(club_contract_valid_until = club_contract_valid_until - 2021) %>%
  rename(contract_remaining_yr = club_contract_valid_until)

data_selected$contract_remaining_yr <- factor(data_selected$contract_remaining_yr)
head(data_selected)
  
# check the distribution of the soccer metric scores
boxplot(data_selected$physic,
        data_selected$attacking_avg,
        data_selected$skill_avg,
        data_selected$movement_avg,
        data_selected$power_avg,
        data_selected$mentality_avg,
        data_selected$defending_avg,
        names = c("Physical",
                  "Attacking", "Skill", "Movement",
                  "Power", "Mentality", "Defending"), 
        xlab = "Parameters",
        ylab = "Value (0-100)", cex.axis = 0.7, 
        cex.lab = 0.7) 

# who are the most expensive players?
data_selected_sorted <- data_selected %>% arrange(desc(value_eur))
head(data_selected_sorted, 20)

# how many missing values?
sum(is.na(data_selected))

# There are 2328 observations with missing values, preparing the data for regression
data_selected_regression <- data_selected %>% select(-long_name) %>% drop_na()
# EDA
library(ggplot2)
library(ggpubr)

ggplot(data_selected_regression, aes(x = value_eur)) + geom_histogram(bins=100) + labs(title = "The distribution of market value")

# check for normality
ggqqplot(data_selected_regression$value_eur)
data_selected_regression$value_eur <- log(data_selected_regression$value_eur)

# correlation heatmap
# Get lower triangle of the correlation matrix
library(reshape2)
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}

cormat <- round(cor(data_selected_regression %>% select(-c("international_reputation", "league_level", "contract_remaining_yr")) %>% drop_na()), 2)
cormat <- reorder_cormat(cormat)
upper_tri <- get_upper_tri(cormat)

melted_cormat <- melt(upper_tri, na.rm = TRUE)

ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 60, vjust = 1, 
                                   size = 8, hjust = 1))+
  coord_fixed() + geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(1.5, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))
ggheatmap
# ---------- PARAMETRIC REGRESSION ---------------

# function needed later
# function for evaluating metrics
eval_results <- function(true, predicted, df) {
  SSE <- sum((predicted - true)^2)
  MSE = SSE/nrow(df)
  
  # Model performance metrics
  MSE_table <- data.frame(MSE = MSE)
  print(MSE_table)
}

# multiple linear regression
model <- lm(value_eur ~ ., data_selected_regression)

sink("lm.txt")
print(summary(model))
sink()

AIC(model)
BIC(model)
# ---------- NONPARAMETRIC REGRESSION -------------
# using GAM
library(mgcv)

model_gam <- gam(value_eur ~ (s(attacking_avg) + s(age) + league_level + international_reputation + s(physic) + contract_remaining_yr + s(skill_avg) + s(movement_avg) + s(power_avg) + s(mentality_avg) + s(defending_avg)), family = gaussian(), data = data_selected_regression)


sink("gam.txt")
print(summary(model_gam))
sink()

AIC(model_gam)
BIC(model_gam)

# comparing MSE
eval_results(data_selected_regression$value_eur, model$fitted.values, data_selected_regression)
eval_results(data_selected_regression$value_eur, model_gam$fitted.values, data_selected_regression)

ggplot(data_selected_regression, aes(x = attacking_avg, y = value_eur)) + geom_point() + geom_smooth(method = "lm", aes(color = "Parametric")) + geom_smooth(method = "gam", aes(colour="Nonparametric")) + labs(x = "Attacking Score", y = "log(market value in EUR)", title="True vs Predicted Value") + scale_colour_manual(name="legend", values=c("blue", "red"))
ggplot(data_selected_regression, aes(x = defending_avg, y = value_eur)) + geom_point() + geom_smooth(method = "lm", aes(color = "Parametric")) + geom_smooth(method = "gam", aes(colour="Nonparametric")) + labs(x = "Defending Score", y = "log(market value in EUR)", title="True vs Predicted Value") + scale_colour_manual(name="legend", values=c("blue", "red"))

ggplot(data_selected_regression, aes(x = age, y = value_eur)) + geom_point() + geom_smooth(method = "lm", aes(color = "Parametric")) + geom_smooth(method = "gam", aes(colour="Nonparametric")) + labs(x = "Age", y = "log(market value in EUR)", title="True vs Predicted Value") + scale_colour_manual(name="legend", values=c("blue", "red"))
ggplot(data_selected_regression, aes(x = skill_avg, y = value_eur)) + geom_point() + geom_smooth(method = "lm", aes(color = "Parametric")) + geom_smooth(method = "gam", aes(colour="Nonparametric")) + labs(x = "Skill Score", y = "log(market value in EUR)", title="True vs Predicted Value") + scale_colour_manual(name="legend", values=c("blue", "red"))

ggplot(data_selected_regression, aes(x = mentality_avg, y = value_eur)) + geom_point() + geom_smooth(method = "lm", aes(color = "Parametric")) + geom_smooth(method = "gam", aes(colour="Nonparametric")) + labs(x = "Mentality Score", y = "log(market value in EUR)", title="True vs Predicted Value") + scale_colour_manual(name="legend", values=c("blue", "red"))
ggplot(data_selected_regression, aes(x = power_avg, y = value_eur)) + geom_point() + geom_smooth(method = "lm", aes(color = "Parametric")) + geom_smooth(method = "gam", aes(colour="Nonparametric")) + labs(x = "Power Score", y = "log(market value in EUR)", title="True vs Predicted Value") + scale_colour_manual(name="legend", values=c("blue", "red"))


ggplot(data_selected_regression, aes(x = movement_avg, y = value_eur)) + geom_point() + geom_smooth(method = "lm", aes(color = "Parametric")) + geom_smooth(method = "gam", aes(colour="Nonparametric")) + labs(x = "Movement Score", y = "log(market value in EUR)", title="True vs Predicted Value") + scale_colour_manual(name="legend", values=c("blue", "red"))
ggplot(data_selected_regression, aes(x = physic, y = value_eur)) + geom_point() + geom_smooth(method = "lm", aes(color = "Parametric")) + geom_smooth(method = "gam", aes(colour="Nonparametric")) + labs(x = "Physical Score", y = "log(market value in EUR)", title="True vs Predicted Value") + scale_colour_manual(name="legend", values=c("blue", "red"))
