# Load the necessary packages
library(car)
library(effsize)
library(sjPlot)

# Create example data set
factor1 <- rep(c("A", "B"), each = 6)
factor2 <- rep(c("X", "Y"), times = 6)
variable <- c(10, 12, 14, 18, 20, 22, 25, 27, 29, 33, 35, 37)

data <- data.frame(factor1, factor2, variable)

# Check the structure of the data set
str(data)

# Check the summary of the data set
summary(data)

# Check for missing values
sum(is.na(data))

# Check normality assumptions with Shapiro-Wilk test
shapiro.test(data$variable)

# Check homogeneity of variances with Levene's test
data$interaction <- interaction(data$factor1, data$factor2)
leveneTest(variable ~ interaction, data = data)

# Run the Type III ANOVA
model <- aov(variable ~ factor1 * factor2, data = data)
anova <- Anova(model, type="III")

# Check the effect size
etasq(model)

# Check for interaction effect
plot_model(model, type = "pred", terms = c("factor1", "factor2"))

# Perform Tukey's pairwise post hoc tests
tukey <- TukeyHSD(model)
tukey
