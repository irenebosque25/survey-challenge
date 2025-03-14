library(haven)
library(countrycode)
library(scales)
library(ggplot2)
library(plotly)
library(corrplot)
library(reshape2)
library(tidyverse)
library(countries)
library(RColorBrewer)

#Finally, for an extra analysis we will analyze the paradata from the survey
# read data
survey <- read_dta("ZA7575.dta")

# load variables 
paradata <- survey %>% 
  select(c(country, isocntry, qc19, d60, p3, p4, p5))
#p3 -> minutes the interview lasted
#p4 -> Number of persons present during the interview
#p5 -> respondent cooperation

# basic look of variables 
unique(paradata$qc19)
sapply(paradata, class)



# transform data in order to work correctly with the variables
paradata <- paradata %>% 
  mutate(
    trans_name_na = if_else(qc19 == 3, 1, 0), 
    difficulties_bills = as.factor(if_else(d60 == 4, NA, d60)),
    int_minutes = p3, 
    int_persons = as.factor(p4), 
    int_cooperation = as.factor(p5) 
  ) 

# Country names: DE-E and DE-W were problems 
paradata <- paradata |>   
  mutate(country = country_name(isocntry, to = "simple", 
                                verbose = TRUE, poor_matches = TRUE),
         isocntry = countrycode::countrycode(
           country, origin = "country.name", destination = "iso2c"))

# NAs by country: Here we are creating a table with the proportion of NA's in the target variable in every country
na_country <- paradata %>% 
  group_by(country, isocntry) %>% 
  summarise(
    na_target = sum(trans_name_na), 
    total_values = n()
  ) %>% 
  mutate(
    na_proportion = (na_target / total_values)
  ) 

# Here we are creating a table with the proportion of response NO in the target variable in every country
na_country_plot <- na_country %>%
  left_join(paradata %>% filter(qc19 == 2) %>%
              group_by(country) %>%
              summarise(count_qc19_no = n()) %>%
              ungroup(),
            by = "country") %>%
  mutate(
    proportion_qc19_no = count_qc19_no / total_values 
  )

# tables, plots ----------------------------------

p <- ggplot(na_country, 
            aes(x = reorder(country, -na_proportion), 
                y = na_proportion, 
                fill = na_proportion)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = percent(na_proportion, accuracy = 1)), 
            position = position_stack(vjust = 0.5), 
            color = "white", 
            size = 2) +
  labs(x = "", y = "Proportion of NA", 
       title = "Proportion of NA in Qc_19 by Country", 
       subtitle = "Ordered by Proportion",
       x = "country",
       y = "% of missing values") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12),
        legend.position = "none") +
  guides(fill = guide_legend(reverse = TRUE))

ggplotly(p, tooltip = "text")

#This chart illustrates the proportion of missing values (NA) in the target variable across different countries. We notice that Eastern European countries (like Bulgaria, Poland, and Slovakia) tend to have higher NA rates, while Western European countries (like Belgium, Austria, and the Netherlands) have lower ones.  Notably, there appears to be a potential relationship between the proportion of missing data and levels of support for the trans movement, as countries with higher NA rates are often those with lower reported support. To assess this possible correlation, we will conduct further analysis using a scatterplot.
# Relationship between NA's and No responses in Qc19 by country
p <- ggplot(na_country_plot, aes(x = na_proportion, y = proportion_qc19_no, text = country)) +
  geom_point(aes(color = proportion_qc19_no), size = 3, shape = 21, stroke = 1, fill = "white") +  
  geom_smooth(method = "lm", se = FALSE, color = "red3", size = 1.5, linetype = "solid") +  
  scale_color_gradientn(colors = "darkblue") +  
  labs(
    title = "Relationship between NA's and No responses in Qc19 by country",
    x = "NA Proportion",
    y = "No Responses Proportion"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    legend.position = "none",  # Mostrar leyenda
    panel.grid.major = element_line(color = "grey80", linetype = "dashed"),
    panel.grid.minor = element_blank()
  )


lm_model <- lm(proportion_qc19_no ~ na_proportion, data = na_country_plot)

regression_line <- data.frame(
  na_proportion = seq(min(na_country_plot$na_proportion), max(na_country_plot$na_proportion), length.out = 100),
  proportion_qc19_no = predict(lm_model, newdata = data.frame(na_proportion = seq(min(na_country_plot$na_proportion), max(na_country_plot$na_proportion), length.out = 100)))
)

ggplotly(p, tooltip = "text") %>%
  add_lines(data = regression_line, x = ~na_proportion, y = ~proportion_qc19_no, 
            line = list(color = 'orange', width = 1), name = 'Línea de regresión')

#The scatterplot suggests a positive correlation between NA proportion and "No" responses to the question on transgender rights. This could indicate that in countries with more missing responses, there is also greater reluctance to support legal gender changes. Possible explanations include social desirability bias, where respondents prefer to skip rather than explicitly oppose, lack of awareness or engagement with the issue, or cultural and political factors that make the topic more sensitive.

# model ------------------------------------------
model <- glm(trans_name_na ~ 
               difficulties_bills + 
               int_minutes + 
               int_persons + 
               int_cooperation, 
             data = paradata, 
             family = "binomial")
summary(model)

#This model predicts the probability of having a NA  in trans_name_na based on factors such as payment difficulties, interview duration, number of people present, and respondent cooperation. For payment difficulties, severe difficulties (difficulties_bills7) significantly increase the probability of not responding to the trans_doc question. However, other categories of difficulties (difficulties_bills2 and difficulties_bills3) do not show strong effects, although difficulties_bills3 suggests a slight increase in probability.The interview time has no significant effect on the probability of trans_doc being NA, as its p-value is high. Similarly, the number of people present does not show a clear effect, though having three people present (int_persons3) decreases the probability of trans_name_na being NA. Finally, respondent cooperation has a positive and significant effect on the probability of not responding to the qc_19 question. The more cooperative the respondent, the higher the probability that trans_name_na will be 1, with a particularly notable increase for higher levels of cooperation. In summary, the main variables significantly affecting the probability of trans_name_na being 1 are payment difficulties and respondent cooperation, while interview time and number of people present have no relevant effect.
