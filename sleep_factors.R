
#Analysis of Factors Affecting Sleep Quality

Sleep_health_and_lifestyle_dataset <- read_excel("C:/Users/gonca/Downloads/Sleep_health_and_lifestyle_dataset.xlsx")

sleep_dataset <- Sleep_health_and_lifestyle_dataset

sleep_dataset <- as.data.frame(sleep_dataset)

head(sleep_dataset)
str(sleep_dataset)

columns_to_remove <- c("Person ID","Age", "BMI Category", "Blood Pressure", "Heart Rate", "Daily Steps")

sleep_dataset <- sleep_dataset[, !(names(sleep_dataset) %in% columns_to_remove)]

sum(is.na(sleep_dataset))

str(sleep_dataset)


# EDA

library(readr)
library(ggplot2)
library(gridExtra)
library(reshape2)
library(forcats)
library(dplyr)
library(grid)

# sleep quality

histogram <- ggplot(sleep_dataset, aes(x = `Quality of Sleep`)) + 
  geom_histogram(binwidth = 1, fill = "steelblue3", color = "black") +
  labs(title = NULL,
       x = "Sleep quality", y = "Frequency") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))

boxplot_plot <- ggplot(sleep_dataset, aes(x = NULL, y = `Quality of Sleep`)) +
  geom_boxplot(fill = "steelblue3") +
  labs(title = NULL, x = "", 
       y = "Sleep quality" ) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))

grid.arrange(
  histogram, boxplot_plot,
  ncol = 2,
  top = textGrob(
    "Distribution of sleep quality",
    gp = gpar(fontsize = 14, fontface = "bold")
  )
)

# physical activity

histogram2 <- ggplot(sleep_dataset, aes(x = `Physical Activity Level`)) +
  geom_histogram(binwidth = 10 ,fill = "lightgreen", color = "black") +
  labs(title = NULL, x = "Physical activity (min/daily)", y = "Frequency") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))

boxplot_plot2 <- ggplot(sleep_dataset, aes(x="", y = `Physical Activity Level`)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = NULL, x = "" , y = "Physical activity (min/daily)") +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

grid.arrange(
  histogram2, boxplot_plot2, 
  ncol = 2,
  top = textGrob("Distribution of physical activity level",
                 gp = gpar(fontsize = 14, fontface = "bold"))
)

# Sleep duration

histogram3 <- ggplot(sleep_dataset, aes(x = `Sleep Duration`)) +
  geom_histogram(binwidth = 1, fill = "pink3", color = "black") +
  labs(title = NULL, x = "Sleep duration", y = "Frequency") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))

boxplot_plot3 <- ggplot(sleep_dataset, aes(x="", y = `Sleep Duration`)) +
  geom_boxplot(fill = "pink3") +
  labs(title = NULL, x = "" , y = "Sleep duration") +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

grid.arrange(
  histogram3, boxplot_plot3, 
  ncol = 2,
  top = textGrob("Distribution of sleep duration",
                 gp = gpar(fontsize = 14, fontface = "bold"))
)


# Stress level

histogram4 <- ggplot(sleep_dataset, aes(x = `Stress Level`)) +
  geom_histogram(binwidth = 1, fill = "orange", color = "black") +
  labs(title = NULL, x = "Stress level", y = "Frequency") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))

boxplot_plot4 <- ggplot(sleep_dataset, aes(x="", y = `Stress Level`)) +
  geom_boxplot(fill = "orange") +
  labs(title = NULL, x = "" , y = "Stress level") +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

grid.arrange(
  histogram4, boxplot_plot4, 
  ncol = 2,
  top = textGrob("Distribution of stress levels",
                 gp = gpar(fontsize = 14, fontface = "bold"))
)

# Gender

freq_dist <- table(sleep_dataset$`Gender`)
relative_freq <- prop.table(freq_dist)

freq_table <- data.frame(
  gender = names(freq_dist),
  Frequency = as.integer(freq_dist),
  Relative_Frequency = round(as.numeric(relative_freq), 4))

print(freq_table)


ggplot(freq_table, aes(x = fct_reorder(gender, Relative_Frequency, .desc = TRUE), 
                       y = Relative_Frequency)) +
  geom_bar(stat = "identity", fill = "coral2") +
  labs(title = "Distribution of gender",x = "Gender",
       y = "Relative Frequency") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(size = 10, hjust = 0.5))


ggplot(freq_table, aes(x = "", y = Relative_Frequency, fill = gender)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Distribution of gender") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank()) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  geom_text(aes(label = paste0(round(Relative_Frequency * 100, 1), "%")), 
            position = position_stack(vjust = 0.50),size=3)


# Sleep disorder

freq_dist2 <- table(sleep_dataset$`Sleep Disorder`)
relative_freq2 <- prop.table(freq_dist2)

freq_table2 <- data.frame(
  disorder = names(freq_dist2), 
  Frequency = as.integer(freq_dist2),  
  Relative_Frequency = round(as.numeric(relative_freq2), 4)
)

print(freq_table2)

ggplot(freq_table2, aes(x = factor(disorder), y = Relative_Frequency)) +
  geom_bar(stat = "identity", fill = "springgreen3") +
  labs(
    title = "Distribution of sleep disorder",
    x = "Sleep disorder",
    y = "Relative Frequency"
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))


ggplot(freq_table2, aes(x = "", y = Relative_Frequency, fill = disorder)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Sleep disorder") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank()) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  geom_text(aes(label = paste0(round(Relative_Frequency * 100, 1), "%")), 
            position = position_stack(vjust = 0.50),size=3)


# Quality of sleep x stress level

biv_plot1 <- ggplot(sleep_dataset, aes(x = `Stress Level`, y = `Quality of Sleep`)) +
  geom_point(color = "blue", alpha = 0.7) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Quality of sleep vs Stress level",
       x = "Stress level",
       y = "Quality of sleep") +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

print(biv_plot1)

# Quality of sleep x Physical activity level

biv_plot2 <- ggplot(sleep_dataset, aes(x = `Physical Activity Level`, y = `Quality of Sleep`)) +
  geom_point(color = "purple", alpha = 0.7) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Quality of sleep vs Physical activity level",
       x = "Physical activity level",
       y = "Quality of sleep") +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

print(biv_plot2)

# Quality of sleep x Sleep duration

biv_plot3 <- ggplot(sleep_dataset, aes(x = `Sleep Duration`, y = `Quality of Sleep`)) +
  geom_point(color = "darkgreen", alpha = 0.7) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Quality of sleep vs Sleep duration",
       x = "Sleep duration",
       y = "Quality of sleep") +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

print(biv_plot3)



# Quality of sleep x Sleep disorder

barplot_disorder <- ggplot(sleep_dataset, aes(x = `Sleep Disorder`, y = `Quality of Sleep`, fill = `Sleep Disorder`)) +
  stat_summary(fun = "mean", geom = "bar", position = "dodge") +
  labs(
    title = "Quality of sleep by sleep disorder",
    x = "Sleep disorder",
    y = "Quality of sleep",
    fill = "Sleep Disorder"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    legend.position = "none"
  )

print(barplot_disorder)

boxplot_disorder <- ggplot(sleep_dataset, aes(x = `Sleep Disorder`, y = `Quality of Sleep`, fill = `Sleep Disorder`)) +
  geom_boxplot(show.legend = FALSE, 
               outlier.shape = NA,     
               notch = FALSE,          
               color = "black",        
               size = 0.5,             
               fatten = 2) +           
  labs(
    title = "Quality of Sleep by Sleep Disorder",
    x = "Sleep Disorder",
    y = "Quality of Sleep"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 0, hjust = 0.5)
  )

print(boxplot_disorder)


# Quality of sleep x Gender

barplot_gender <- ggplot(sleep_dataset, aes(x = `Gender`, y = `Quality of Sleep`, fill = `Gender`)) +
  stat_summary(fun = "mean", geom = "bar", position = "dodge") +
  labs(
    title = "Quality of sleep by gender",
    x = "Gender",
    y = "Quality of sleep",
    fill = "Gender"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    legend.position = "none"
  )

print(barplot_gender)

boxplot_gender <- ggplot(sleep_dataset, aes(x = `Gender`, y = `Quality of Sleep`, fill = `Gender`)) +
  geom_boxplot() + 
  labs(
    title = "Quality of Sleep by Gender",
    x = "Gender",
    y = "Quality of Sleep",
    fill = "Gender"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    legend.position = "none"
  )

print(boxplot_gender)

# Statistical analysis

## Spearman correlation test (quality of sleep vs sleep duration, physical activity, stress level)

# Spearman correlation: Quality of Sleep vs Sleep Duration
correlation_sleep_duration <- cor.test(
  sleep_dataset$`Quality of Sleep`, 
  sleep_dataset$`Sleep Duration`, 
  method = "spearman"
)
print(correlation_sleep_duration)

# Spearman correlation: Quality of Sleep vs Physical Activity
correlation_physical_activity <- cor.test(
  sleep_dataset$`Quality of Sleep`, 
  sleep_dataset$`Physical Activity Level`, 
  method = "spearman"
)
print(correlation_physical_activity)

# Spearman correlation: Quality of Sleep vs Stress Level
correlation_stress_level <- cor.test(
  sleep_dataset$`Quality of Sleep`, 
  sleep_dataset$`Stress Level`, 
  method = "spearman"
)
print(correlation_stress_level)

correlation_results <- data.frame(
  Variable = c("Sleep Duration", "Physical Activity Level", "Stress Level"),
  Correlation = c(
    correlation_sleep_duration$estimate,
    correlation_physical_activity$estimate,
    correlation_stress_level$estimate
  )
)

# build a heatmap with correlation coeff

heatmap_plot <- ggplot(correlation_results, aes(x = Variable, y = "Quality of Sleep", fill = Correlation)) +
  geom_tile(color = "white", width = 0.6, height = 0.6) +  
  geom_text(aes(label = round(Correlation, 2)), size = 3, color = "black") +  
  scale_fill_gradient2(
    low = "blue", high = "red", mid = "white",
    midpoint = 0, limit = c(-1, 1),
    name = "Spearman\nCorrelation"
  ) +
  theme_minimal() +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_text(size = 9),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    plot.title = element_text(hjust = 0.5)
  ) +
  labs(
    title = "Correlation with Quality of Sleep",
    x = "Variables"
  ) +
  coord_fixed()

print(heatmap_plot)


## Kruskal-wallis test: comparison of quality of sleep across sleep disorders

kruskal_test <- kruskal.test(
  `Quality of Sleep` ~ `Sleep Disorder`, 
  data = sleep_dataset
)

print(kruskal_test)

library(dunn.test) # post hoc

dunn_results <- dunn.test(
  x = sleep_dataset$`Quality of Sleep`, 
  g = sleep_dataset$`Sleep Disorder`, 
  method = "bonferroni"  # Bonferroni correction for multiple comparisons
)

print(dunn_results)

## Mann-Whitney: compare sleep quality between males and females

mann_whitney_test <- wilcox.test(
  `Quality of Sleep` ~ Gender,
  data = sleep_dataset,
  exact = FALSE 
)

print(mann_whitney_test)

medians <- tapply(sleep_dataset$`Quality of Sleep`, sleep_dataset$Gender, median)
print(medians)


# effect size 
total_n <- length(sleep_dataset$`Quality of Sleep`)
z_value <- qnorm(1 - mann_whitney_test$p.value / 2)  
cat("Effect size (r):", effect_size, "\n")

# end
