library(haven)
library(countrycode)
library(scales)
library(ggplot2)
library(plotly)
library(corrplot)
library(reshape2)

# read data
survey <- read_dta("data/ZA7575.dta")

# load variables 
paradata <- survey %>% 
  select(c(country, isocntry, qc19, d60, p3, p4, p5))

# basic look of variables 
unique(paradata$qc19)
sapply(paradata, class)

# transform data 
paradata <- paradata %>% 
  mutate(
    trans_name_na = if_else(qc19 == 3, 1, 0), 
    countries = country_name(isocntry, to = "simple", 
                             verbose = TRUE, poor_matches = TRUE), 
    difficulties_bills = as.factor(if_else(d60 == 4, NA, d60)),
    int_minutes = p3, 
    int_persons = as.factor(p4), 
    int_cooperation = as.factor(p5) 
    ) 

# NAs by country 
na_country <- paradata %>% 
  group_by(countries, isocntry) %>% 
  summarise(
    na_target = sum(trans_name_na), 
    total_values = n()
  ) %>% 
  mutate(
    na_proportion = (na_target / total_values)
  )

# tables, plots ----------------------------------

prop.table(table(paradata$isocntry, paradata$trans_name_na), margin = 1)*100

na_country %>%
  arrange(desc(na_proportion)) %>% 
  select(cntry, missing_pct)

p <- ggplot(na_country, 
       aes(x = reorder(countries, -na_proportion), 
           y = na_proportion, 
           fill = na_proportion)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "darkmagenta", 
                      high = "orange", 
                      name = "") +
  geom_text(aes(label = percent(na_proportion, accuracy = 1)), 
            position = position_stack(vjust = 0.5), 
            color = "white", 
            size = 2) +
  labs(x = "", y = "Proportion of NA", 
       title = "Proportion of NA in CACA", 
       subtitle = "Ordered by Proportion") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12),
        legend.position = "none") +
  guides(fill = guide_legend(reverse = TRUE))

p

ggplotly(p, tooltip = "text")

ggplot(na_country, 
       aes(x = reorder(countries, -na_proportion), 
           y = na_proportion, 
           fill = na_proportion)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis_b() +
  labs(
    title = "% missing answers by country",
    x = "country",
    y = "% of missing values"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45))

# correlation matrix just for funsies ------------

sapply(numeric_data, class)

numeric_data <- paradata %>%
  select(c(difficulties_bills, 
             int_minutes, 
             int_persons, 
             int_cooperation)) %>% 
  mutate(int_persons = as.numeric(int_persons), 
         int_cooperation = as.numeric(int_cooperation))

cor_matrix <- cor(numeric_data, use = "complete.obs")

corrplot(cor_matrix, 
         method = "color",         # Color-based heatmap
         type = "upper",           # Only upper triangle
         tl.col = "black",         # Color for text labels
         tl.srt = 45,              # Rotate text labels for better readability
         diag = FALSE)  



# model ------------------------------------------
model <- glm(trans_name_na ~ 
               difficulties_bills + 
               int_minutes + 
               int_persons + 
               int_cooperation, 
             data = paradata, 
             family = "binomial")
summary(model)

# checking linear effect between missing values and int_minutes using natural splines: 
# we can only check lineary like she does in the homework 
# (i honestly have no idea what the fuck she does in the part where she does age^2), 
# on continuous (non-factor) variables, so int_minutes is the only one where we could do it on 
# but int_minutes is useless anyways so this whole part is pointless lol 

library(splines)
model_non_linear <- glm(trans_name_na ~ 
                          difficulties_bills + 
                          int_persons + 
                          int_cooperation + 
                          ns(int_minutes, df = 4), 
                        data = paradata, 
                        family = binomial)

summary(model_non_linear)
