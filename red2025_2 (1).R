# required packages
install.packages(c("dplyr", "ggplot2", "tidyr", "gridExtra", "ggpubr", "extrafont",
                   "vcd", "ggmosaic", "broom", "knitr", "purr", "rlang", "scales",
                   "car", "effects", "tidyverse"))
extrafont::font_import()
extrafont::loadfonts(device = "win")

# libraries
library(tidyverse)

library(dplyr)
library(ggplot2)
library(tidyr)
library(gridExtra)
library(ggpubr)
library(extrafont)
library(vcd)

library(ggmosaic)
library(broom)
library(knitr)
library(purrr)
library(rlang)
library(scales)

library(car)
library(effects)

# color palette
professional_palette <- c("#BFDCEC", "#2D4D83", "#E15759", "#771718")

# theme with Times New Roman
theme_linguistic <- function() {
  theme_minimal(base_family = "Times New Roman") +
    theme(
      plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
      axis.title = element_text(size = 10),
      axis.text = element_text(size = 8),
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 8),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(fill = NA, color = "gray80"),
      panel.background = element_rect(fill = "white", color = NA)
    )
}

df <- read.csv("df2_CHECKED_23_06_25.csv", header = TRUE, stringsAsFactor = TRUE) # сюда вставляешь название файла

# 1. Remove participants who only answered "no"
# participants with only "no" answers
no_only_ids <- df %>%
  group_by(informant_id) %>%
  summarize(all_no = all(ANSWER == "no")) %>%
  filter(all_no) %>%
  pull(informant_id)
length(no_only_ids)

# participants with only "yes" answers
no_only_ids <- df %>%
  group_by(informant_id) %>%
  summarize(all_no = all(ANSWER == "yes")) %>%
  filter(all_no) %>%
  pull(informant_id)
length(no_only_ids)

# filtered df
df_filtered <- df %>%
  filter(!informant_id %in% no_only_ids)

cat("Participants:", length(unique(df_filtered$informant_id)), "\n")
cat("Observations:", nrow(df_filtered), "\n\n")

# 1. Count unique informant_id in 'bias' and 'no_bias' contexts
unique_ids_bias <- df_filtered %>%
  filter(context == "bias") %>%
  distinct(informant_id) %>%
  nrow()

unique_ids_no_bias <- df_filtered %>%
  filter(context == "no_bias") %>%
  distinct(informant_id) %>%
  nrow()

cat("Number of unique informant_id in 'bias' context:", unique_ids_bias, "\n")
cat("Number of unique informant_id in 'no_bias' context:", unique_ids_no_bias, "\n")

# 2. Check for intersections between informant_id in 'bias' and 'no_bias' contexts
ids_bias <- df_filtered %>%
  filter(context == "bias") %>%
  distinct(informant_id) %>%
  pull(informant_id)

ids_no_bias <- df_filtered %>%
  filter(context == "no_bias") %>%
  distinct(informant_id) %>%
  pull(informant_id)

intersection <- intersect(ids_bias, ids_no_bias)

if (length(intersection) > 0) {
  cat("There is an intersection between informant_id in 'bias' and 'no_bias' contexts.\n")
  cat("Intersecting informant_id:", intersection, "\n")
} else {
  cat("There is NO intersection between informant_id in 'bias' and 'no_bias' contexts.\n")
}

#1. Count number of unique informant_ids in each context group
context_counts <- df %>%
  group_by(context) %>%
  summarise(unique_informants = n_distinct(informant_id))

# Display the counts
print(context_counts)

# 2. If no_bias group is smaller, subset the bias group to match its size
if (context_counts$unique_informants[context_counts$context == "no_bias"] < 
    context_counts$unique_informants[context_counts$context == "bias"]) {
  
  # Get the number of unique informants in no_bias group
  no_bias_count <- context_counts$unique_informants[context_counts$context == "no_bias"]
  
  # Get the first n unique informant_ids from bias group (where n = no_bias_count)
  bias_informants <- unique(df$informant_id[df$context == "bias"])
  selected_informants <- bias_informants[1:no_bias_count]
  
  # Filter the original dataframe to include only these informants in bias group
  balanced_df <- df %>%
    filter(context == "no_bias" | (context == "bias" & informant_id %in% selected_informants))
  
  # Verify the counts
  balanced_counts <- balanced_df %>%
    group_by(context) %>%
    summarise(unique_informants = n_distinct(informant_id))
  
  print("Balanced counts:")
  print(balanced_counts)
  
} else {
  print("no_bias group is not smaller - no balancing needed")
  balanced_df <- df
}

glimpse(balanced_df)
cat("Observations:", nrow(balanced_df), "\n\n")

# split by readings
#df_strict <- df_filtered %>% filter(STR.SL == "str")
#df_sloppy <- df_filtered %>% filter(STR.SL == "sl")

df_strict <- balanced_df %>% filter(STR.SL == "str")
df_sloppy <- balanced_df %>% filter(STR.SL == "sl")

glimpse(df_strict)
glimpse(df_sloppy)

df_strict = droplevels(df_strict)
df_sloppy = droplevels(df_sloppy)

# 2. Visualizations

process_reading_data <- function(data_filtered, reading_type) {
  # 1. Answer distribution (percentage)
  answer_counts <- data_filtered %>%
    count(ANSWER) %>%
    mutate(percentage = n/sum(n)*100,
           label = paste0(round(percentage), "%"))
  
  plot1 <- ggplot(answer_counts, aes(x = "", y = n, fill = ANSWER)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y", start = 0) +
    geom_text(aes(label = label),
              position = position_stack(vjust = 0.5),
              color = "white", size = 5, family = "Times New Roman") +
    scale_fill_manual(values = professional_palette[1:2]) +
    labs(title = paste("Answer distribution -", reading_type, " (2 experiment)"),
         fill = "Answer") +
    theme_linguistic() +
    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          panel.border = element_blank())
  
  # 2. Acceptance by conjunction (percentage)
  conj_data <- data_filtered %>%
    count(CONJ, ANSWER) %>%
    group_by(CONJ) %>%
    mutate(percentage = n / sum(n) * 100)
  
  plot2 <- ggplot(conj_data, aes(x = CONJ, y = percentage, fill = ANSWER)) +
    geom_col(position = "fill") +
    scale_fill_manual(values = professional_palette[1:2]) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(title = "By conjunction type (2 experiment)",
         x = "Conjunction", y = "Percentage") +
    theme_linguistic()
  
  # 3. Acceptance by IC type (percentage)
  ic_data <- data_filtered %>%
    count(IC, ANSWER) %>%
    group_by(IC) %>%
    mutate(percentage = n / sum(n) * 100)
  
  plot3 <- ggplot(ic_data, aes(x = IC, y = percentage, fill = ANSWER)) +
    geom_col(position = "fill") +
    scale_fill_manual(values = professional_palette[1:2]) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(title = "By IC type (2 experiment)",
         x = "IC type", y = "Percentage") +
    theme_linguistic()
  
  # 4. Acceptance by context (percentage)
  context_data <- data_filtered %>%
    count(context, ANSWER) %>%
    group_by(context) %>%
    mutate(percentage = n / sum(n) * 100)
  
  plot4 <- ggplot(context_data, aes(x = context, y = percentage, fill = ANSWER)) +
    geom_col(position = "fill") +
    scale_fill_manual(values = professional_palette[1:2]) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(title = "By context type (2 experiment)",
         x = "Context", y = "Percentage") +
    theme_linguistic()
  
  # 5. Parameter contribution heatmap
  contribution_data <- data_filtered %>%
    group_by(CONJ, IC, context) %>%
    summarize(acceptance_rate = mean(ANSWER == "yes") * 100, .groups = "drop")
  
  plot5 <- ggplot(contribution_data, 
                  aes(x = interaction(CONJ, IC, sep = " + "), 
                      y = context, 
                      fill = acceptance_rate)) +
    geom_tile(color = "white") +
    geom_text(aes(label = paste0(round(acceptance_rate), "%")), 
              color = "white", size = 3) +
    scale_fill_gradient(low = professional_palette[3], 
                        high = professional_palette[4],
                        name = "Acceptance rate %") +
    labs(title = paste("Parameter contribution to", reading_type, "reading acceptance (2 experiment)"),
         x = "Conjunction & IC type",
         y = "Context") +
    theme_linguistic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom")
  
  # 6. Parameter contribution heatmap
  contribution_data <- data_filtered %>%
    group_by(CONJ, IC, context) %>%
    summarize(acceptance_rate = mean(ANSWER == "no") * 100, .groups = "drop")
  
  plot6 <- ggplot(contribution_data, 
                  aes(x = interaction(CONJ, IC, sep = " + "), 
                      y = context, 
                      fill = acceptance_rate)) +
    geom_tile(color = "white") +
    geom_text(aes(label = paste0(round(acceptance_rate), "%")), 
              color = "white", size = 3) +
    scale_fill_gradient(low = professional_palette[3], 
                        high = professional_palette[4],
                        name = "Rejection rate %") +
    labs(title = paste("Parameter contribution to", reading_type, "reading rejection (2 experiment)"),
         x = "Conjunction & IC type",
         y = "Context") +
    theme_linguistic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom")
  
  # Print plots in sequence rather than combining
  print(plot1)
  print(plot2)
  print(plot3)
  print(plot4)
  print(plot5)
  print(plot6)
  
  # Return statistics
  cat("\n=== Statistics for", reading_type, "reading ===\n")
  list(
    participants = length(unique(data_filtered$informant_id)),
    observations = nrow(data_filtered),
    answer_distribution = table(data_filtered$ANSWER),
    acceptance_rates = contribution_data
  )
}

# Process both reading types
strict_results <- process_reading_data(df_strict, "strict")
sloppy_results <- process_reading_data(df_sloppy, "sloppy")



# 3. Analysis

## 1. Logistic Regression Models ##

# With effects interaction or without?
#strict
model_STR0 <- glm(ANSWER ~ CONJ * IC * context + (1|informant_id), data = df_strict, family = binomial)
summary(model_STR0) # модель с взаимодействием

model_STR1 <- glm(ANSWER ~ CONJ + IC + context + (1|informant_id), data = df_strict, family = binomial)
summary(model_STR1) # модель без взаимодействия
anova(model_STR0, model_STR1, test="Chisq")
anova(model_STR0, model_STR1, test="LRT")

#sloppy
model_SL0 <- glm(ANSWER ~ CONJ * IC * context + (1|informant_id), data = df_sloppy, family = binomial)
summary(model_SL0) # модель с взаимодействием

model_SL1 <- glm(ANSWER ~ CONJ + IC + context + (1|informant_id), data = df_sloppy, family = binomial)
summary(model_SL1) # модель без взаимодействия
anova(model_SL0, model_SL1, test="Chisq")
anova(model_SL0, model_SL1, test="LRT")

# Model for sloppy readings

model_sloppy <- glm(ANSWER ~ CONJ * IC * context, 
                    data = df_sloppy, 
                    family = binomial())

# Model for strict readings
model_strict <- glm(ANSWER ~ CONJ * IC * context, 
                    data = df_strict, 
                    family = binomial())

# Summary of models
summary(model_sloppy)
summary(model_strict)

# Anova to see significant effects
Anova(model_strict, type = 3)
anova_results <- anova(model_strict, test = "Chisq")
anova_results
chisq.test(table(df_strict$IC, df_strict$context))

Anova(model_sloppy, type = 3)
anova_results <- anova(model_sloppy, test = "Chisq")
anova_results
chisq.test(table(df_sloppy$IC, df_sloppy$context))

# sloppy
# Создание упрощённых моделей
reduced_model_CONJ_sl <- update(model_sloppy, . ~ . - CONJ)
reduced_model_IC_sl <- update(model_sloppy, . ~ . - IC)
reduced_model_context_sl <- update(model_sloppy, . ~ . - context)

# Проведение тестов отношения правдоподобия
anova_result_CONJ_sl <- anova(model_sloppy, reduced_model_CONJ_sl, test = "Chisq")
anova_result_IC_sl <- anova(model_sloppy, reduced_model_IC_sl, test = "Chisq")
anova_result_context_sl <- anova(model_sloppy, reduced_model_context_sl, test = "Chisq")

# Выведем результаты
anova_result_CONJ_sl
anova_result_IC_sl
anova_result_context_sl

# strict
# Создание упрощённых моделей
reduced_model_CONJ <- update(model_strict, . ~ . - CONJ)
reduced_model_IC <- update(model_strict, . ~ . - IC)
reduced_model_context <- update(model_strict, . ~ . - context)

# Проведение тестов отношения правдоподобия
anova_result_CONJ <- anova(model_strict, reduced_model_CONJ, test = "Chisq")
anova_result_IC <- anova(model_strict, reduced_model_IC, test = "Chisq")
anova_result_context <- anova(model_strict, reduced_model_context, test = "Chisq")

# Выведем результаты
anova_result_CONJ
anova_result_IC
anova_result_context

## 2. Contingency Tables and Mosaic Plots ##

# 1
create_mosaic_gg <- function(data, formula, title) {
  professional_palette <- c("#BFDCEC", "#2D4D83", "#E15759", "#771718")
  ggplot(data) +
    geom_mosaic(aes(x = product(!!!rlang::parse_exprs(all.vars(formula)[-1])), 
                    fill = !!sym(all.vars(formula)[1])),
                color = "white") +
    labs(title = title) +
    theme_linguistic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_fill_manual(values = professional_palette[1:2])
}

create_mosaic_gg(df_sloppy, ~ ANSWER * IC * context, "IC × context interaction (sloppy reading), 2 experiment")
create_mosaic_gg(df_sloppy, ~ ANSWER * context * CONJ, "context × CONJ interaction (sloppy reading), 2 experiment")
create_mosaic_gg(df_sloppy, ~ ANSWER * IC * CONJ, "IC × CONJ interaction (sloppy reading), 2 experiment")

create_mosaic_gg(df_strict, ~ ANSWER * IC * context, "IC × context interaction (strict reading), 2 experiment")
create_mosaic_gg(df_strict, ~ ANSWER * context * CONJ, "context × CONJ interaction (strict reading), 2 experiment")
create_mosaic_gg(df_strict, ~ ANSWER * IC  CONJ, "IC × CONJ interaction (strict reading), 2 experiment")


# For three-way interactions with ggmosaic
ggplot(df_sloppy) +
  geom_mosaic(
    aes(
      x = product(context, CONJ, IC),  # Nest context > CONJ > IC
      fill = ANSWER,                    # Color by acceptance (yes/no)
    ),
    offset = 0.02,                     # Adjust spacing
    divider = mosaic("v"),              # Vertical splits
    color = "white"
  ) +
  labs(
    title = "Parameter Interaction: context × CONJ × IC (2 experiment)",
    x = "ANSWER:CONJ",
    fill = "Sloppy reading (accepted?)"
  ) +
  scale_fill_manual(values = professional_palette[3:4]) +
  theme_linguistic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

ggplot(df_strict) +
  geom_mosaic(
    aes(
      x = product(context, CONJ, IC),  # Nest context > CONJ > IC
      fill = ANSWER,                    # Color by acceptance (yes/no)
    ),
    offset = 0.02,                     # Adjust spacing
    divider = mosaic("v"),              # Vertical splits
    color = "white"
  ) +
  labs(
    title = "Parameter Interaction: context × CONJ × IC (2 experiment)",
    x = "ANSWER:CONJ",
    fill = "Strict reading (accepted?)"
  ) +
  scale_fill_manual(values = professional_palette[3:4]) +
  theme_linguistic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

## 3. Visualizing Model Effects ##

# sloppy
model_sloppy <- glm(ANSWER ~ CONJ * IC * context, 
                    data = df_sloppy, 
                    family = binomial())

# Model for strict readings
model_strict <- glm(ANSWER ~ CONJ * IC * context, 
                    data = df_strict, 
                    family = binomial())

# Plotting the most important effects
plot(allEffects(model_sloppy),
     main = "Effects on sloppy reading acceptance (2 experiment)",
     lines = list(col = c("steelblue", "#E15759")), 
     band = list(col = "#BFDCEC80"))
plot(allEffects(model_strict),
     lines = list(col = c("steelblue", "#E15759")),
     band = list(col = "#BFDCEC80"))

# Variable importance plots
# For sloppy readings
var_imp_sloppy <- anova(model_sloppy) %>% 
  as_tibble() %>% 
  mutate(Term = rownames(anova(model_sloppy))) %>% 
  filter(!Term %in% c("Residuals", "(Intercept)")) %>% 
  arrange(desc(`Pr(>Chi)`))

ggplot(var_imp_sloppy, aes(x = reorder(Term, `Pr(>Chi)`), y = -log10(`Pr(>Chi)`))) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Parameter importance for sloppy reading acceptance (2 experiment)",
       x = "Parameter",
       y = "-log10(p-value)") +
  theme_linguistic()

# For strict readings
var_imp_strict <- anova(model_strict) %>% 
  as_tibble() %>% 
  mutate(Term = rownames(anova(model_strict))) %>% 
  filter(!Term %in% c("Residuals", "(Intercept)")) %>% 
  arrange(desc(`Pr(>Chi)`))

ggplot(var_imp_strict, aes(x = reorder(Term, `Pr(>Chi)`), y = -log10(`Pr(>Chi)`))) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Parameter importance for strict reading acceptance (2 experiment)",
       x = "Parameter",
       y = "-log10(p-value)") +
  theme_linguistic()

