# Loading packages
library(dplyr)
library(tidyr)
library(ggpubr)
library(usdm)
library(psych)
library(ggplot2)
library(MASS)
library(AER)
library(glmtoolbox)

# Loading data
data <- read.csv("/Users/stella/Downloads/Bats_day_all.csv")
str(data)

data$TYPE <- factor(data$TYPE)
data$Historical_Sites <- factor(data$Historical_Sites)

data1 <- data
data2 <- data %>% filter(TYPE != "Garden")

# Check histogram
par(mfrow=c(2,2))
hist(data$PASSES, main="Histogram", xlab="Value", ylab="Frequency")
plot(density(data1$PASSES), main="Density Plot", xlab="Value")
boxplot(data1$PASSES, main="Boxplot", ylab="Value")

par(mfrow=c(2,2))
hist(data2$PASSES, main="Histogram", xlab="Value", ylab="Frequency")
plot(density(data2$PASSES), main="Density Plot", xlab="Value")
boxplot(data2$PASSES, main="Boxplot", ylab="Value")

# Check potential collinearity 
pairs.panels(data[,c("HEIGHT", "SITE_VEGETATION_COVER", "GREEN_COVERAGE_100", 
                     "GREEN_COVERAGE_150","GREEN_COVERAGE_200", "BLUE_COVERAGE_100", 
                     "BLUE_COVERAGE_150","BLUE_COVERAGE_200")])
pairs.panels(data[,c("HEIGHT", "SITE_VEGETATION_COVER", "Mean_100", "SD_100",
                     "Mean_150", "SD_150","Mean_200", "SD_200")])

# Model1: bat activities by habitat type
M1 <- glm(PASSES~TYPE,  data=data1, family = poisson(link = "log"))
summary(M1)
print(paste("adjusted_r2:", adjR2(M1)))

# To account for over dispersion
M1.1 <- glm.nb(PASSES~TYPE,control = glm.control(maxit = 3000),  data=data1, init.theta = 1)
summary(M1.1)
print(paste("adjusted_r2:", adjR2(M1.1)))

# Model2: envrionmental factors' influence on bat activites on rooftops 
# Choose 200m buffer after examing the AIC values. In addition, blue_cover_100/150 has too many zeros, and failed to fit the models.
# From the maximum model
M2 <- glm(PASSES ~ factor(TYPE) + factor(Historical_Sites) + HEIGHT + 
               SITE_VEGETATION_COVER + GREEN_COVERAGE_200 + BLUE_COVERAGE_200 + 
               AVE_TEMP + Precipitation + Mean_200 + SD_200, data = data2, family = poisson(link = "log"))
summary(M2)
print(paste("adjusted_r2:", adjR2(M2)))
dispersiontest(M2)

# -Historical_stie
M3 <- glm(PASSES ~ factor(TYPE) + HEIGHT + SITE_VEGETATION_COVER + 
            GREEN_COVERAGE_200 + BLUE_COVERAGE_200 + AVE_TEMP + 
            Precipitation + Mean_200 + SD_200, data = data2, family = poisson(link = "log"))
summary(M3)
print(paste("adjusted_r2:", adjR2(M3)))
dispersiontest(M3)

# Compare M2 and M3
anova(M2, M3, test="Chisq")
# No significant difference -> choose M3

# -Mean_200
M4 <- glm(PASSES ~ factor(TYPE) + factor(Historical_Sites) + HEIGHT + 
            SITE_VEGETATION_COVER + GREEN_COVERAGE_200 + BLUE_COVERAGE_200 + 
            AVE_TEMP + Precipitation + SD_200, data = data2, family = poisson(link = "log"))
summary(M4)
print(paste("adjusted_r2:", adjR2(M4)))
dispersiontest(M4)

# Compare M2 and M3
anova(M3, M4, test="Chisq")
# No significant difference -> choose M4

# All variables are significant
# Fit negative bionomial to account for over dispersion
# Negative bionomial
M4.1 <- glm.nb(PASSES ~ factor(TYPE)  + HEIGHT + SITE_VEGETATION_COVER + GREEN_COVERAGE_200 + 
                 BLUE_COVERAGE_200 + AVE_TEMP + Precipitation  + SD_200, 
               data = data2, control = glm.control(maxit = 3000),  init.theta = 1)
summary(M4.1)
print(paste("adjusted_r2:", adjR2(M4.1)))

# -Blue coverage
M4.2.1 <- glm.nb(PASSES ~ factor(TYPE)  + HEIGHT + SITE_VEGETATION_COVER + GREEN_COVERAGE_200 + 
                 + AVE_TEMP + Precipitation  + SD_200, 
               data = data2, control = glm.control(maxit = 3000),  init.theta = 1)
summary(M4.2.1)
print(paste("adjusted_r2:", adjR2(M4.2.1)))

anova(M4.1, M4.2.1, test="Chisq")
#  No significant difference -> choose M4.2.1

# -precipitation
M4.2.2 <- glm.nb(PASSES ~ factor(TYPE)  + HEIGHT + SITE_VEGETATION_COVER + GREEN_COVERAGE_200  + 
                   AVE_TEMP + SD_200, data = data2, control = glm.control(maxit = 3000),  init.theta = 1)
summary(M4.2.2)
print(paste("adjusted_r2:", adjR2(M4.2.2)))
anova(M4.2.1, M4.2.2, test="Chisq")
#  No significant difference -> choose M4.2.2

# To check the deviance explained by variables
anova(M4.2.2, test="Chisq")

# Check diagnostic plots
par(mfrow=c(2,2))
plot(M4.2.2)
sum(cooks.distance(M4.2.2)>1)

# Make figures
# Make figure 1: bat activites by habitat type
df <- read.csv("/Users/stella/Downloads/Bats_data_all.csv")
df$TYPE <- factor(df$TYPE)
ave_type <- df %>%
  group_by(TYPE) %>%
  summarise(
    P = mean(PASSES),
    F = mean(FEEDINGBUZZ),
    S = mean(SOCIALCALL),
    P_se = sd(PASSES) / sqrt(n()), # Standard Error for Passes
    F_se = sd(FEEDINGBUZZ) / sqrt(n()), # Standard Error for Feeding Buzz
    S_se = sd(SOCIALCALL) / sqrt(n()), # Standard Error for Social Call
    .groups = "drop"
  )

# Convert TYPE to a factor to control the order
order <- c("Conventional", "Extensive", "Intensive", "Garden")
ave_type$TYPE <- factor(ave_type$TYPE, levels = order)

# Melt the data to long format for easier plotting
ave_type_long <- ave_type %>%
  pivot_longer(cols = c(P, F, S), names_to = "Metric", values_to = "Value") %>%
  pivot_longer(cols = c(P_se, F_se, S_se), names_to = "Metric_se", values_to = "SE")

# Filter to match Metric and Metric_se
ave_type_long <- ave_type_long %>%
  filter(substring(Metric, 1, 1) == substring(Metric_se, 1, 1))

# Convert Metric to a factor to control display order (P, F, S)
ave_type_long$Metric <- factor(ave_type_long$Metric, levels = c("P", "F", "S"))

# Define color palette
color_palette <- c("P" = "#1b9e77", "F" = "#d95f02", "S" = "#7570b3")

# Plot bar chart with matching error bar colors
plot1 <- ggplot(ave_type_long, aes(x = TYPE, y = Value, fill = Metric, color = Metric)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.6) + # Adjust bar width and position
  geom_errorbar(aes(ymin = Value, ymax = Value + SE),
                position = position_dodge(width = 0.8), width = 0.2) + # Error bars with matching colors
  labs(x = "Habitat type",
       y = "Mean number of bat call sequences by habitat type",
       fill = NULL) +
  scale_fill_manual(values = color_palette, # Use the same color palette for fill
                    labels = c("Pass", "Feeding buzz", "Social call")) +
  scale_color_manual(values = color_palette, # Use the same color palette for color
                     guide = "none") + # No separate legend for color
  theme_classic(base_size = 13, base_family = "serif") + # Use a classic theme with serif font
  theme(legend.position = "top", # Legend at the top
        panel.grid.major = element_line(color = "grey80", linetype = "dotted"), # Subtle grid lines
        panel.grid.minor = element_blank(), # Remove minor grid lines
        axis.line = element_line(color = "black"),
        axis.title.x = element_text(face = "bold"),  # Bold x-axis title
        axis.title.y = element_text(face = "bold"),  # Bold y-axis title
        axis.text.x = element_text(face = "bold"),   # Bold x-axis labels
        axis.text.y = element_text(face = "bold"),   # Bold y-axis labels
        legend.text = element_text(face = "bold"))   # Bold legend tex

plot(plot1)

# Plot figure 2: significant envrionmental variables 
# Make fitted lines
# Generate new_data and predict function
generate_predicted_data <- function(var_name, fixed_vars, model, data) {
  min_val <- min(data[[var_name]], na.rm = TRUE)
  max_val <- max(data[[var_name]], na.rm = TRUE)
  range <- max_val - min_val
  
  new_data <- data.frame(
    Value = seq(min_val - 0.1*range, max_val + 0.1*range, length.out = 100)
  )
  
  # fixed variables
  for (v in names(fixed_vars)) {
    new_data[[v]] <- fixed_vars[[v]]
  }
  
  # make prediction for current variable
  new_data[[var_name]] <- new_data$Value
  new_data$predicted_values <- predict(model, newdata = new_data, type = "response")
  
  return(new_data)
}

# Define fixed variables
fixed_vars <- list(
  TYPE = factor("Extensive", levels = levels(data2$TYPE)),
  HEIGHT = mean(data2$HEIGHT),
  GREEN_COVERAGE_200 = mean(data2$GREEN_COVERAGE_200),
  AVE_TEMP = mean(data2$AVE_TEMP),
  SD_200 = mean(data2$SD_200),
  SITE_VEGETATION_COVER = mean(data2$SITE_VEGETATION_COVER)
)

# Make predictions for every variable
new_data_height <- generate_predicted_data("HEIGHT", fixed_vars, M4.2.2, data2)
new_data_green <- generate_predicted_data("GREEN_COVERAGE_200", fixed_vars, M4.2.2, data2)
new_data_temp <- generate_predicted_data("AVE_TEMP", fixed_vars, M4.2.2, data2)

# A special case of SD_200 for better plotting
new_data_sd <- data.frame(
  SD_200 = seq(min(data2$SD_200), max(data2$SD_200), length.out = 100),
  TYPE = factor("Extensive", levels = levels(data2$TYPE)),  # Use Convenitonal Level
  GREEN_COVERAGE_200 = mean(data2$GREEN_COVERAGE_200),
  AVE_TEMP = mean(data2$AVE_TEMP),
  HEIGHT = mean(data2$HEIGHT),
  SITE_VEGETATION_COVER = mean(data2$SITE_VEGETATION_COVER))
new_data_sd$predicted_values <- predict(M4.2.2, newdata = new_data_sd, type="response")
new_data_sd$Value <-  new_data_sd$SD_200

# Define color and shape palette
color_palette <- c("#d95f02", "#1b9e77", "#66a61e", "#7570b3")
shape_palette <- c(16, 17, 16, 17)

# Define labels
custom_labels <- c(
  "HEIGHT" = "(a) Roof height (m)",
  "GREEN_COVERAGE_200" = "(b) Green coverage (200m Buffer, %)",
  "AVE_TEMP" = "(c) Average temperature (°C)",
  "SD_200" = "(d) Height standard deviation (200m Buffer, m)"
)

# Turn data into longer dataframe
data_long <- data2 %>%
  pivot_longer(cols = c(HEIGHT, GREEN_COVERAGE_200, AVE_TEMP, SD_200), 
               names_to = "Variable", 
               values_to = "Value") %>%
  mutate(Variable = factor(Variable, levels = c("HEIGHT", "GREEN_COVERAGE_200", "AVE_TEMP", "SD_200")))

# Create projections for color and shape
data_long <- data_long %>%
  mutate(Color = factor(Variable, levels = levels(Variable), labels = color_palette),
         Shape = factor(Variable, levels = levels(Variable), labels = shape_palette))

# Create long dataframe
predicted_long <- bind_rows(
  new_data_height %>% mutate(Variable = "HEIGHT"),
  new_data_green %>% mutate(Variable = "GREEN_COVERAGE_200"),
  new_data_temp %>% mutate(Variable = "AVE_TEMP"),
  new_data_sd %>% mutate(Variable = "SD_200")
) %>%
  rename(predicted_passes = predicted_values) %>%
  mutate(Variable = factor(Variable, levels = c("HEIGHT", "GREEN_COVERAGE_200", "AVE_TEMP", "SD_200")))

# Create plot 
combined_plot <- ggplot(data_long, aes(x = Value, y = PASSES, color = Variable, shape = Variable)) +
  geom_point(size = 2, alpha = 0.9) +
  geom_line(data = predicted_long, aes(y = predicted_passes), size = 0.8, alpha=0.8) +
  scale_color_manual(values = color_palette) +
  scale_shape_manual(values = shape_palette) +
  labs(x = NULL,
       y = "Number of Bat Passes per night") +
  facet_wrap(~ Variable, scales = "free_x", ncol = 2,
             labeller = as_labeller(custom_labels)) +
  theme_classic(base_size = 11, base_family = "serif") +
  theme(
    legend.position = "none",
    panel.grid.major = element_line(color = "grey90", linetype = "dotted"),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    strip.background = element_rect(fill = "grey95", color = "black", size = 0.5),
    strip.text = element_text(face = "bold", size = 11),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    axis.title.y = element_text(linewidth = 13, face = "bold")
  ) +
  scale_y_continuous(labels = scales::comma)

plot(combined_plot)














