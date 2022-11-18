require(tidyverse)
require(magrittr)
require(data.table)
require(lubridate)
require(ggthemes)
require(likert)
require(RColorBrewer)
require(gridExtra)

## User Inputs ----------------------------------------------------------------
year_to_observe <- 2021
single_month_observation <- "April"
questions <- c("calls_msgs",
               "remote_forms",
               "real_time_support",
               "flexibility",
               "work_life")

## Data Loading and Manipulation ----------------------------------------------
responses <- fread("survey_responses.csv") %>% as.data.frame()
responses$Timestamp %<>% mdy_hms()
responses$year <- year(responses$Timestamp)
responses$month <- month.name[month(responses$Timestamp)]


likert_levels <- c(1:5)
likert_lables <- c("Very Disatisfied",
                   "Disatisfied",
                   "Neutral",
                   "Satisfied",
                   "Very Satisfied")

responses[, c("Clinic",
              "pronouns",
              "depends",
              "urg")] %<>% lapply(., factor)

for (i in questions){
  responses[, i] %<>% factor(levels = likert_levels, labels = likert_lables, ordered = TRUE)
}



# Plotting -------------------------------------------------------------------
## Bar Chart ----------------------------------------------------------------
### Clinic --------------------------------------------------------------------

ques1 <- responses %>%
  filter(year == year_to_observe & month == single_month_observation) %>%
  ggplot(aes(y = calls_msgs, fill = Clinic)) + 
  geom_bar(position = position_dodge2(preserve = "single")) + 
  coord_flip() + 
  theme_fivethirtyeight() + 
  labs(title = "How satisfied are you with:\nHow the nursing staff handles calls and messages?",
       x = "",
       y = "Count") + 
  scale_fill_brewer(palette = "Set2") + 
  theme(plot.title = element_text(size = 14, hjust = .5))

ques2 <- responses %>%
  filter(year == year_to_observe & month == single_month_observation) %>%
  ggplot(aes(y = remote_forms, fill = Clinic)) + 
  geom_bar(position = position_dodge2(preserve = "single")) + 
  coord_flip() + 
  theme_fivethirtyeight() + 
  labs(title = "How satisfied are you with:\nThe process for completing forms when working remotely?",
       x = "",
       y = "Count") + 
  scale_fill_brewer(palette = "Set2") + 
  theme(plot.title = element_text(size = 14, hjust = .5))

ques3 <- responses %>%
  filter(year == year_to_observe & month == single_month_observation) %>%
  ggplot(aes(y = real_time_support, fill = Clinic)) + 
  geom_bar(position = position_dodge2(preserve = "single")) + 
  coord_flip() + 
  theme_fivethirtyeight() + 
  labs(title = "How satisfied are you with:\nYour ability to reach team members for real time support when working remotely?",
       x = "",
       y = "Count") + 
  scale_fill_brewer(palette = "Set2") + 
  theme(plot.title = element_text(size = 14, hjust = .5))

ques4 <- responses %>%
  filter(year == year_to_observe & month == single_month_observation) %>%
  ggplot(aes(y = flexibility, fill = Clinic)) + 
  geom_bar(position = position_dodge2(preserve = "single")) + 
  coord_flip() + 
  theme_fivethirtyeight() + 
  labs(title = "How satisfied are you with:\nyour options for flexibility?",
       x = "",
       y = "Count") + 
  scale_fill_brewer(palette = "Set2") + 
  theme(plot.title = element_text(size = 14, hjust = .5))

ques5 <- responses %>%
  filter(year == year_to_observe & month == single_month_observation) %>%
  ggplot(aes(y = work_life, fill = Clinic)) + 
  geom_bar(position = position_dodge2(preserve = "single")) + 
  coord_flip() + 
  theme_fivethirtyeight() + 
  labs(title = "How satisfied are you with:\nyour overall work-life balance?",
       x = "",
       y = "Count") + 
  scale_fill_brewer(palette = "Set2") + 
  theme(plot.title = element_text(size = 14, hjust = .5))

clinicarrangement <- grid.arrange(ques1, ques2, ques3, ques4, ques5, ncol = 2, top = "Sorted by Clinic")
ggsave("clinic_bar.png", clinicarrangement, width = 16, height = 24, dpi = 400)

## Separations----------------------------------------------------------------
### Pronouns -----------------------------------------------------------------
ques1 <- responses %>%
  filter(year == year_to_observe & month == single_month_observation) %>%
  ggplot(aes(y = calls_msgs, fill = pronouns)) + 
  geom_bar(position = position_dodge2(preserve = "single")) + 
  coord_flip() + 
  theme_fivethirtyeight() + 
  labs(title = "How satisfied are you with:\nHow the nursing staff handles calls and messages?",
       x = "",
       y = "Count",
       fill = "Pronouns") + 
  scale_fill_brewer(palette = "Set2") + 
  theme(plot.title = element_text(size = 14, hjust = .5))

ques2 <- responses %>%
  filter(year == year_to_observe & month == single_month_observation) %>%
  ggplot(aes(y = remote_forms, fill = pronouns)) + 
  geom_bar(position = position_dodge2(preserve = "single")) + 
  coord_flip() + 
  theme_fivethirtyeight() + 
  labs(title = "How satisfied are you with:\nThe process for completing forms when working remotely?",
       x = "",
       y = "Count",
       fill = "Pronouns") + 
  scale_fill_brewer(palette = "Set2") + 
  theme(plot.title = element_text(size = 14, hjust = .5))

ques3 <- responses %>%
  filter(year == year_to_observe & month == single_month_observation) %>%
  ggplot(aes(y = real_time_support, fill = pronouns)) + 
  geom_bar(position = position_dodge2(preserve = "single")) + 
  coord_flip() + 
  theme_fivethirtyeight() + 
  labs(title = "How satisfied are you with:\nYour ability to reach team members for real time support when working remotely?",
       x = "",
       y = "Count",
       fill = "Pronouns") + 
  scale_fill_brewer(palette = "Set2") + 
  theme(plot.title = element_text(size = 14, hjust = .5))

ques4 <- responses %>%
  filter(year == year_to_observe & month == single_month_observation) %>%
  ggplot(aes(y = flexibility, fill = pronouns)) + 
  geom_bar(position = position_dodge2(preserve = "single")) + 
  coord_flip() + 
  theme_fivethirtyeight() + 
  labs(title = "How satisfied are you with:\nyour options for flexibility?",
       x = "",
       y = "Count",
       fill = "Pronouns") + 
  scale_fill_brewer(palette = "Set2") + 
  theme(plot.title = element_text(size = 14, hjust = .5))

ques5 <- responses %>%
  filter(year == year_to_observe & month == single_month_observation) %>%
  ggplot(aes(y = work_life, fill = pronouns)) + 
  geom_bar(position = position_dodge2(preserve = "single")) + 
  coord_flip() + 
  theme_fivethirtyeight() + 
  labs(title = "How satisfied are you with:\nyour overall work-life balance?",
       x = "",
       y = "Count",
       fill = "Pronouns") + 
  scale_fill_brewer(palette = "Set2") + 
  theme(plot.title = element_text(size = 14, hjust = .5))

clinicarrangement <- grid.arrange(ques1, ques2, ques3, ques4, ques5, ncol = 2, top = "Sorted by Pronouns")
ggsave("pronouns_bar.png", clinicarrangement, width = 16, height = 24, dpi = 400)

### Dependents --------------------------------------------------------------
ques1 <- responses %>%
  filter(year == year_to_observe & month == single_month_observation) %>%
  ggplot(aes(y = calls_msgs, fill = depends)) + 
  geom_bar(position = position_dodge2(preserve = "single")) + 
  coord_flip() + 
  theme_fivethirtyeight() + 
  labs(title = "How satisfied are you with:\nHow the nursing staff handles calls and messages?",
       x = "",
       y = "Count",
       fill = "Dependents at Home") + 
  scale_fill_brewer(palette = "Set2") + 
  theme(plot.title = element_text(size = 14, hjust = .5)) 
  

ques2 <- responses %>%
  filter(year == year_to_observe & month == single_month_observation) %>%
  ggplot(aes(y = remote_forms, fill = depends)) + 
  geom_bar(position = position_dodge2(preserve = "single")) + 
  coord_flip() + 
  theme_fivethirtyeight() + 
  labs(title = "How satisfied are you with:\nThe process for completing forms when working remotely?",
       x = "",
       y = "Count",
       fill = "Dependents at Home") + 
  scale_fill_brewer(palette = "Set2") + 
  theme(plot.title = element_text(size = 14, hjust = .5))

ques3 <- responses %>%
  filter(year == year_to_observe & month == single_month_observation) %>%
  ggplot(aes(y = real_time_support, fill = depends)) + 
  geom_bar(position = position_dodge2(preserve = "single")) + 
  coord_flip() + 
  theme_fivethirtyeight() + 
  labs(title = "How satisfied are you with:\nYour ability to reach team members for real time support when working remotely?",
       x = "",
       y = "Count",
       fill = "Dependents at Home") + 
  scale_fill_brewer(palette = "Set2") + 
  theme(plot.title = element_text(size = 14, hjust = .5))

ques4 <- responses %>%
  filter(year == year_to_observe & month == single_month_observation) %>%
  ggplot(aes(y = flexibility, fill = depends)) + 
  geom_bar(position = position_dodge2(preserve = "single")) + 
  coord_flip() + 
  theme_fivethirtyeight() + 
  labs(title = "How satisfied are you with:\nyour options for flexibility?",
       x = "",
       y = "Count",
       fill = "Dependents at Home") + 
  scale_fill_brewer(palette = "Set2") + 
  theme(plot.title = element_text(size = 14, hjust = .5))

ques5 <- responses %>%
  filter(year == year_to_observe & month == single_month_observation) %>%
  ggplot(aes(y = work_life, fill = depends)) + 
  geom_bar(position = position_dodge2(preserve = "single")) + 
  coord_flip() + 
  theme_fivethirtyeight() + 
  labs(title = "How satisfied are you with:\nyour overall work-life balance?",
       x = "",
       y = "Count",
       fill = "Dependents at Home") + 
  scale_fill_brewer(palette = "Set2") + 
  theme(plot.title = element_text(size = 14, hjust = .5))

clinicarrangement <- grid.arrange(ques1, ques2, ques3, ques4, ques5, ncol = 2, top = "Sorted by Dependents Y/N")
ggsave("dependents_bar.png", clinicarrangement, width = 16, height = 24, dpi = 400)

### URG ----------------------------------------------------------------------
ques1 <- responses %>%
  filter(year == year_to_observe & month == single_month_observation) %>%
  ggplot(aes(y = calls_msgs, fill = urg)) + 
  geom_bar(position = position_dodge2(preserve = "single")) + 
  coord_flip() + 
  theme_fivethirtyeight() + 
  labs(title = "How satisfied are you with:\nHow the nursing staff handles calls and messages?",
       x = "",
       y = "Count",
       fill = "Under-Represented Group") + 
  scale_fill_brewer(palette = "Set2") + 
  theme(plot.title = element_text(size = 14, hjust = .5))

ques2 <- responses %>%
  filter(year == year_to_observe & month == single_month_observation) %>%
  ggplot(aes(y = remote_forms, fill = urg)) + 
  geom_bar(position = position_dodge2(preserve = "single")) + 
  coord_flip() + 
  theme_fivethirtyeight() + 
  labs(title = "How satisfied are you with:\nThe process for completing forms when working remotely?",
       x = "",
       y = "Count",
       fill = "Under-Represented Group") + 
  scale_fill_brewer(palette = "Set2") + 
  theme(plot.title = element_text(size = 14, hjust = .5))

ques3 <- responses %>%
  filter(year == year_to_observe & month == single_month_observation) %>%
  ggplot(aes(y = real_time_support, fill = urg)) + 
  geom_bar(position = position_dodge2(preserve = "single")) + 
  coord_flip() + 
  theme_fivethirtyeight() + 
  labs(title = "How satisfied are you with:\nYour ability to reach team members for real time support when working remotely?",
       x = "",
       y = "Count",
       fill = "Under-Represented Group") + 
  scale_fill_brewer(palette = "Set2") + 
  theme(plot.title = element_text(size = 14, hjust = .5))

ques4 <- responses %>%
  filter(year == year_to_observe & month == single_month_observation) %>%
  ggplot(aes(y = flexibility, fill = urg)) + 
  geom_bar(position = position_dodge2(preserve = "single")) + 
  coord_flip() + 
  theme_fivethirtyeight() + 
  labs(title = "How satisfied are you with:\nyour options for flexibility?",
       x = "",
       y = "Count",
       fill = "Under-Represented Group") + 
  scale_fill_brewer(palette = "Set2") + 
  theme(plot.title = element_text(size = 14, hjust = .5))

ques5 <- responses %>%
  filter(year == year_to_observe & month == single_month_observation) %>%
  ggplot(aes(y = work_life, fill = urg)) + 
  geom_bar(position = position_dodge2(preserve = "single")) + 
  coord_flip() + 
  theme_fivethirtyeight() + 
  labs(title = "How satisfied are you with:\nyour overall work-life balance?",
       x = "",
       y = "Count",
       fill = "Under-Represented Group") + 
  scale_fill_brewer(palette = "Set2") + 
  theme(plot.title = element_text(size = 14, hjust = .5))

clinicarrangement <- grid.arrange(ques1, ques2, ques3, ques4, ques5, ncol = 2, top = "Sorted by Under-Represented Group Y/N")
ggsave("urg_bar.png", clinicarrangement, width = 16, height = 24, dpi = 400)

## Violins ------------------------------------------------------------------
### Re- Uptake --------------------------------------------------------------
responses <- fread("survey_responses.csv") %>% as.data.frame()
responses$Timestamp %<>% mdy_hms()
responses$year <- year(responses$Timestamp)
responses$month <- factor(month.name[month(responses$Timestamp)], levels = month.name)

responses[, c("Clinic",
              "pronouns",
              "depends",
              "urg")] %<>% lapply(., factor)

### Clinics -----------------------------------------------------------------

ques1 <- responses %>%
  filter(year == year_to_observe) %>%
  ggplot(aes(y = calls_msgs, x = month, fill = Clinic)) + 
  geom_violin(position = "dodge") + 
  geom_jitter(alpha = .5) + 
  scale_y_discrete(limits = c(1:5), labels = likert_lables) +
  theme_fivethirtyeight() + 
  labs(title = "How satisfied are you with:\nHow the nursing staff handles calls and messages?",
       x = paste0("Year of", year_to_observe),
       y = "Count",
       fill = "Clinic") + 
  scale_fill_brewer(palette = "Set2") + 
  theme(plot.title = element_text(size = 14, hjust = .5))
ggsave("Clinic_violin_ques1.png", ques1, width = 24, height = 16, dpi = 400)

ques2 <- responses %>%
  filter(year == year_to_observe) %>%
  ggplot(aes(y = remote_forms, x = month, fill = Clinic)) + 
  geom_violin(position = "dodge") + 
  geom_jitter(alpha = .5) + 
  scale_y_discrete(limits = c(1:5), labels = likert_lables) + 
  theme_fivethirtyeight() + 
  labs(title = "How satisfied are you with:\nThe process for completing forms when working remotely?",
       x = "",
       y = "Count",
       fill = "Clinic") + 
  scale_fill_brewer(palette = "Set2") + 
  theme(plot.title = element_text(size = 14, hjust = .5))
ggsave("Clinic_violin_ques2.png", ques2, width = 24, height = 16, dpi = 400)

ques3 <- responses %>%
  filter(year == year_to_observe) %>%
  ggplot(aes(y = real_time_support, x = month, fill = Clinic)) + 
  geom_violin(position = "dodge") + 
  geom_jitter(alpha = .5) + 
  scale_y_discrete(limits = c(1:5), labels = likert_lables) +
  theme_fivethirtyeight() + 
  labs(title = "How satisfied are you with:\nYour ability to reach team members for real time support when working remotely?",
       x = "",
       y = "Count",
       fill = "Clinic") + 
  scale_fill_brewer(palette = "Set2") + 
  theme(plot.title = element_text(size = 14, hjust = .5))
ggsave("Clinic_violin_ques3.png", ques3, width = 24, height = 16, dpi = 400)

ques4 <- responses %>%
  filter(year == year_to_observe) %>%
  ggplot(aes(y = flexibility, x = month, fill = Clinic)) + 
  geom_violin(position = "dodge") + 
  geom_jitter(alpha = .5) + 
  scale_y_discrete(limits = c(1:5), labels = likert_lables) +
  theme_fivethirtyeight() + 
  labs(title = "How satisfied are you with:\nyour options for flexibility?",
       x = "",
       y = "Count",
       fill = "Clinic") + 
  scale_fill_brewer(palette = "Set2") + 
  theme(plot.title = element_text(size = 14, hjust = .5))
ggsave("Clinic_violin_ques4.png", ques4, width = 24, height = 16, dpi = 400)

ques5 <- responses %>%
  filter(year == year_to_observe) %>%
  ggplot(aes(y = work_life, x = month, fill = Clinic)) + 
  geom_violin(position = "dodge") + 
  geom_jitter(alpha = .5) + 
  scale_y_discrete(limits = c(1:5), labels = likert_lables) +
  theme_fivethirtyeight() + 
  labs(title = "How satisfied are you with:\nyour overall work-life balance?",
       x = "",
       y = "Count",
       fill = "Clinic") + 
  scale_fill_brewer(palette = "Set2") + 
  theme(plot.title = element_text(size = 14, hjust = .5))
ggsave("Clinic_violin_ques5.png", ques5, width = 24, height = 16, dpi = 400)
### Pronouns ----------------------------------------------------------------

ques1 <- responses %>%
  filter(year == year_to_observe) %>%
  ggplot(aes(y = calls_msgs, x = month, fill = pronouns)) + 
  geom_violin(position = "dodge") + 
  geom_jitter(alpha = .5) + 
  scale_y_discrete(limits = c(1:5), labels = likert_lables) +
  theme_fivethirtyeight() + 
  labs(title = "How satisfied are you with:\nHow the nursing staff handles calls and messages?",
       x = paste0("Year of", year_to_observe),
       y = "Count",
       fill = "Pronouns") + 
  scale_fill_brewer(palette = "Set2") + 
  theme(plot.title = element_text(size = 14, hjust = .5))
ggsave("pronouns_violin_ques1.png", ques1, width = 24, height = 16, dpi = 400)

ques2 <- responses %>%
  filter(year == year_to_observe) %>%
  ggplot(aes(y = remote_forms, x = month, fill = pronouns)) + 
  geom_violin(position = "dodge") + 
  geom_jitter(alpha = .5) + 
  scale_y_discrete(limits = c(1:5), labels = likert_lables) + 
  theme_fivethirtyeight() + 
  labs(title = "How satisfied are you with:\nThe process for completing forms when working remotely?",
       x = "",
       y = "Count",
       fill = "Pronouns") + 
  scale_fill_brewer(palette = "Set2") + 
  theme(plot.title = element_text(size = 14, hjust = .5))
ggsave("pronouns_violin_ques2.png", ques2, width = 24, height = 16, dpi = 400)

ques3 <- responses %>%
  filter(year == year_to_observe) %>%
  ggplot(aes(y = real_time_support, x = month, fill = pronouns)) + 
  geom_violin(position = "dodge") + 
  geom_jitter(alpha = .5) + 
  scale_y_discrete(limits = c(1:5), labels = likert_lables) +
  theme_fivethirtyeight() + 
  labs(title = "How satisfied are you with:\nYour ability to reach team members for real time support when working remotely?",
       x = "",
       y = "Count",
       fill = "Pronouns") + 
  scale_fill_brewer(palette = "Set2") + 
  theme(plot.title = element_text(size = 14, hjust = .5))
ggsave("pronouns_violin_ques3.png", ques3, width = 24, height = 16, dpi = 400)
  
ques4 <- responses %>%
  filter(year == year_to_observe) %>%
  ggplot(aes(y = flexibility, x = month, fill = pronouns)) + 
  geom_violin(position = "dodge") + 
  geom_jitter(alpha = .5) + 
  scale_y_discrete(limits = c(1:5), labels = likert_lables) +
  theme_fivethirtyeight() + 
  labs(title = "How satisfied are you with:\nyour options for flexibility?",
       x = "",
       y = "Count",
       fill = "Pronouns") + 
  scale_fill_brewer(palette = "Set2") + 
  theme(plot.title = element_text(size = 14, hjust = .5))
ggsave("pronouns_violin_ques4.png", ques4, width = 24, height = 16, dpi = 400)
  
ques5 <- responses %>%
  filter(year == year_to_observe) %>%
  ggplot(aes(y = work_life, x = month, fill = pronouns)) + 
  geom_violin(position = "dodge") + 
  geom_jitter(alpha = .5) + 
  scale_y_discrete(limits = c(1:5), labels = likert_lables) +
  theme_fivethirtyeight() + 
  labs(title = "How satisfied are you with:\nyour overall work-life balance?",
       x = "",
       y = "Count",
       fill = "Pronouns") + 
  scale_fill_brewer(palette = "Set2") + 
  theme(plot.title = element_text(size = 14, hjust = .5))
ggsave("pronouns_violin_ques5.png", ques5, width = 24, height = 16, dpi = 400)

### Dependents --------------------------------------------------------------

ques1 <- responses %>%
  filter(year == year_to_observe) %>%
  ggplot(aes(y = calls_msgs, x = month, fill = depends)) + 
  geom_violin(position = "dodge") + 
  geom_jitter(alpha = .5) + 
  scale_y_discrete(limits = c(1:5), labels = likert_lables) +
  theme_fivethirtyeight() + 
  labs(title = "How satisfied are you with:\nHow the nursing staff handles calls and messages?",
       x = "",
       y = "Count",
       fill = "Dependents Y/N") + 
  scale_fill_brewer(palette = "Set2") + 
  theme(plot.title = element_text(size = 14, hjust = .5))
ggsave("depends_violin_ques1.png", ques1, width = 24, height = 16, dpi = 400)

ques2 <- responses %>%
  filter(year == year_to_observe) %>%
  ggplot(aes(y = remote_forms, x = month, fill = depends)) + 
  geom_violin(position = "dodge") + 
  geom_jitter(alpha = .5) + 
  scale_y_discrete(limits = c(1:5), labels = likert_lables) + 
  theme_fivethirtyeight() + 
  labs(title = "How satisfied are you with:\nThe process for completing forms when working remotely?",
       x = "",
       y = "Count",
       fill = "Dependents Y/N") + 
  scale_fill_brewer(palette = "Set2") + 
  theme(plot.title = element_text(size = 14, hjust = .5))
ggsave("depends_violin_ques2.png", ques2, width = 24, height = 16, dpi = 400)

ques3 <- responses %>%
  filter(year == year_to_observe) %>%
  ggplot(aes(y = real_time_support, x = month, fill = depends)) + 
  geom_violin(position = "dodge") + 
  geom_jitter(alpha = .5) + 
  scale_y_discrete(limits = c(1:5), labels = likert_lables) +
theme_fivethirtyeight() + 
  labs(title = "How satisfied are you with:\nYour ability to reach team members for real time support when working remotely?",
       x = "",
       y = "Count",
       fill = "Dependents Y/N") + 
  scale_fill_brewer(palette = "Set2") + 
  theme(plot.title = element_text(size = 14, hjust = .5))
ggsave("depends_violin_ques3.png", ques3, width = 24, height = 16, dpi = 400)

ques4 <- responses %>%
  filter(year == year_to_observe) %>%
  ggplot(aes(y = flexibility, x = month, fill = depends)) + 
  geom_violin(position = "dodge") + 
  geom_jitter(alpha = .5) + 
  scale_y_discrete(limits = c(1:5), labels = likert_lables) +
theme_fivethirtyeight() + 
  labs(title = "How satisfied are you with:\nyour options for flexibility?",
       x = "",
       y = "Count",
       fill = "Dependents Y/N") + 
  scale_fill_brewer(palette = "Set2") + 
  theme(plot.title = element_text(size = 14, hjust = .5))
ggsave("depends_violin_ques4.png", ques4, width = 24, height = 16, dpi = 400)

ques5 <- responses %>%
  filter(year == year_to_observe) %>%
  ggplot(aes(y = work_life, x = month, fill = depends)) + 
  geom_violin(position = "dodge") + 
  geom_jitter(alpha = .5) + 
  scale_y_discrete(limits = c(1:5), labels = likert_lables) +
theme_fivethirtyeight() + 
  labs(title = "How satisfied are you with:\nyour overall work-life balance?",
       x = "",
       y = "Count",
       fill = "Dependents Y/N") + 
  scale_fill_brewer(palette = "Set2") + 
  theme(plot.title = element_text(size = 14, hjust = .5))
ggsave("depends_violin_ques5.png", ques5, width = 24, height = 16, dpi = 400)

### URG ---------------------------------------------------------------------

ques1 <- responses %>%
  filter(year == year_to_observe) %>%
  ggplot(aes(y = calls_msgs, x = month, fill = urg)) + 
  geom_violin(position = "dodge") + 
  geom_jitter(alpha = .5) + 
  scale_y_discrete(limits = c(1:5), labels = likert_lables) +
  theme_fivethirtyeight() + 
  labs(title = "How satisfied are you with:\nHow the nursing staff handles calls and messages?",
       x = "",
       y = "Count",
       fill = "Under-Represented Group Y/N") + 
  scale_fill_brewer(palette = "Set2") + 
  theme(plot.title = element_text(size = 14, hjust = .5))
ggsave("urg_violin_ques1.png", ques1, width = 24, height = 16, dpi = 400)

ques2 <- responses %>%
  filter(year == year_to_observe) %>%
  ggplot(aes(y = remote_forms, x = month, fill = urg)) + 
  geom_violin(position = "dodge") + 
  geom_jitter(alpha = .5) + 
  scale_y_discrete(limits = c(1:5), labels = likert_lables) + 
  theme_fivethirtyeight() + 
  labs(title = "How satisfied are you with:\nThe process for completing forms when working remotely?",
       x = "",
       y = "Count",
       fill = "Dependents Y/N") + 
  scale_fill_brewer(palette = "Set2") + 
  theme(plot.title = element_text(size = 14, hjust = .5))
ggsave("urg_violin_ques2.png", ques2, width = 24, height = 16, dpi = 400)

ques3 <- responses %>%
  filter(year == year_to_observe) %>%
  ggplot(aes(y = real_time_support, x = month, fill = urg)) + 
  geom_violin(position = "dodge") + 
  geom_jitter(alpha = .5) + 
  scale_y_discrete(limits = c(1:5), labels = likert_lables) +
theme_fivethirtyeight() + 
  labs(title = "How satisfied are you with:\nYour ability to reach team members for real time support when working remotely?",
       x = "",
       y = "Count",
       fill = "Dependents Y/N") + 
  scale_fill_brewer(palette = "Set2") + 
  theme(plot.title = element_text(size = 14, hjust = .5))
ggsave("urg_violin_ques3.png", ques3, width = 24, height = 16, dpi = 400)

ques4 <- responses %>%
  filter(year == year_to_observe) %>%
  ggplot(aes(y = flexibility, x = month, fill = urg)) + 
  geom_violin(position = "dodge") + 
  geom_jitter(alpha = .5) + 
  scale_y_discrete(limits = c(1:5), labels = likert_lables) +
theme_fivethirtyeight() + 
  labs(title = "How satisfied are you with:\nyour options for flexibility?",
       x = "",
       y = "Count",
       fill = "Under-Represented Group Y/N") + 
  scale_fill_brewer(palette = "Set2") + 
  theme(plot.title = element_text(size = 14, hjust = .5))
ggsave("urg_violin_ques4.png", ques4, width = 24, height = 16, dpi = 400)

ques5 <- responses %>%
  filter(year == year_to_observe) %>%
  ggplot(aes(y = work_life, x = month, fill = urg)) + 
  geom_violin(position = "dodge") + 
  geom_jitter(alpha = .5) + 
  scale_y_discrete(limits = c(1:5), labels = likert_lables) +
theme_fivethirtyeight() + 
  labs(title = "How satisfied are you with:\nyour overall work-life balance?",
       x = "",
       y = "Count",
       fill = "Under-Represented Group Y/N") + 
  scale_fill_brewer(palette = "Set2") + 
  theme(plot.title = element_text(size = 14, hjust = .5))
ggsave("urg_violin_ques5.png", ques5, width = 24, height = 16, dpi = 400)
