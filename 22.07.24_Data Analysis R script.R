library(tidyverse)

#file.choose()
data <- read.csv("C:\\Users\\benib\\OneDrive\\Bioscience DTP\\DPhil\\Charly Treiber - rotation 2\\T-maze\\Results\\19.07.24_T-maze_LTM_TeqvsCS\\19.07.24_R_datasheet_template_PI.csv")

data = na.omit(data) # remove any NA values from dataset.

# Calculating Mean and SEM
bar_plot <- data %>%
  group_by(Memory_type, Genotype) %>%
  summarise(
    Mean_PI = mean(PI),
    SEM = sd(PI) / sqrt(n()),
    .groups = 'drop'
  )

# Create the plot
ggplot() +
  geom_col(data = bar_plot, aes(x = Memory_type, y = Mean_PI), fill = "lightblue", width = 0.3) +
  geom_errorbar(data = bar_plot, aes(x = Memory_type, ymin = Mean_PI - SEM, ymax = Mean_PI + SEM), width = 0.1) +
  geom_jitter(data = data, aes(x = Memory_type, y = PI), color = "blue", width = 0.1) +
  facet_wrap(~ Genotype) +
  scale_y_continuous(limits = c(-0.4, 0.5)) + # change this to whatever is appropriate
  labs(title = "Performance Index by Memory Type",
       x = "Memory Type",
       y = "Performance Index (PI)") +
  theme_minimal()


