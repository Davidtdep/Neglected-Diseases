################################################################################
# 0. LOAD LIBRARIES
################################################################################

library(openxlsx)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(RColorBrewer)
library(forcats)



################################################################################
# 1. LOAD DATA
################################################################################

# Input
income.regression = read_excel("~/Neglected-Diseases/data/Supplementary Material 2.xlsx")
income.meta = read_excel("~/Neglected-Diseases/data/Supplementary Material 3.xlsx")
income.meta.reg = read_excel("~/Neglected-Diseases/data/Supplementary Material 4.xlsx")

# Filter only some indicators in the list
indicators = c(  "Antiretroviral.therapy.coverage....of.people.living.with.HIV.",
                 "Antiretroviral.therapy.coverage.for.PMTCT....of.pregnant.women.living.with.HIV.",
                 "Children..0.14..living.with.HIV",
                 "Children..ages.0.14..newly.infected.with.HIV",
                 "Children.with.fever.receiving.antimalarial.drugs....of.children.under.age.5.with.fever.",
                 "Incidence.of.HIV..all..per.1.000.uninfected.population.",
                 "Incidence.of.malaria..per.1.000.population.at.risk.",
                 "Incidence.of.tuberculosis..per.100.000.people.",
                 "Tuberculosis.case.detection.rate.....all.forms.",
                 "Tuberculosis.treatment.success.rate....of.new.cases.",
                 "NUMBER.OF.DALYS",
                 "NUMBER.OF.DEATHS",
                 "DEATH.RATE",
                 "Number.of.people.requiring.treatment.against.neglected.tropical.diseases",
                 "Number.of.people.with.mild.or.severe.anemia.from.neglected.tropical.diseases",
                 "Death.rate.from.venomous.snakes",
                 "Deaths.from.cysticercosis",
                 "Deaths.from.rabies.by.world.region",
                 "Dengue.fever.deaths",
                 "Number.of.people.requiring.preventive.treatment.for.lymphatic.filariasis",
                 "Number.of.people.requiring.preventive.treatment.for.schistosomiasis",
                 "Reported.cases.of.leprosy",
                 "Antibiotic.consumption.rate")


income.regression <- income.regression[income.regression$dependent_var %in% indicators | 
                                           income.regression$independent_var %in% indicators, ]

income.meta <- income.meta[income.meta$dependent_var %in% indicators | 
                             income.meta$independent_var %in% indicators, ]





################################################################################
# 2. PLOTTING
################################################################################

# Define sub-categories for different indicators
hiv_indicators <- c("Antiretroviral.therapy.coverage....of.people.living.with.HIV.",
                    "Antiretroviral.therapy.coverage.for.PMTCT....of.pregnant.women.living.with.HIV.",
                    "Children..0.14..living.with.HIV",
                    "Children..ages.0.14..newly.infected.with.HIV",
                    "Incidence.of.HIV..all..per.1.000.uninfected.population.")

tb_indicators <- c("Incidence.of.tuberculosis..per.100.000.people.",
                   "Tuberculosis.case.detection.rate.....all.forms.",
                   "Tuberculosis.treatment.success.rate....of.new.cases.")

malaria_indicators <- c("Incidence.of.malaria..per.1.000.population.at.risk.",
                        "Children.with.fever.receiving.antimalarial.drugs....of.children.under.age.5.with.fever.")

ntd_indicators <- c("Number.of.people.requiring.treatment.against.neglected.tropical.diseases",
                    "Number.of.people.with.mild.or.severe.anemia.from.neglected.tropical.diseases",
                    "Number.of.people.requiring.preventive.treatment.for.lymphatic.filariasis",
                    "Number.of.people.requiring.preventive.treatment.for.schistosomiasis",
                    "Reported.cases.of.leprosy")

ntd_deaths_indicators <- c("Death.rate.from.venomous.snakes",
                           "Deaths.from.cysticercosis",
                           "Deaths.from.rabies.by.world.region",
                           "Dengue.fever.deaths")

general_indicators <- c("NUMBER.OF.DALYS",
                        "NUMBER.OF.DEATHS",
                        "DEATH.RATE",
                        "Antibiotic.consumption.rate")








#-------------------------------------------------------------------------------
# HEATMAP
#-------------------------------------------------------------------------------


# Create mapping for clean indicator names
indicator_names <- c(
  "Antiretroviral.therapy.coverage....of.people.living.with.HIV." = 
    "Antiretroviral therapy coverage (% of people living with HIV)",
  "Antiretroviral.therapy.coverage.for.PMTCT....of.pregnant.women.living.with.HIV." = 
    "Antiretroviral therapy coverage for PMTCT (% of pregnant women living with HIV)",
  "Children..0.14..living.with.HIV" = 
    "Children (0-14) living with HIV",
  "Children..ages.0.14..newly.infected.with.HIV" = 
    "Children (ages 0-14) newly infected with HIV",
  "Children.with.fever.receiving.antimalarial.drugs....of.children.under.age.5.with.fever." = 
    "Children with fever receiving antimalarial drugs (% of children under age 5 with fever)",
  "Incidence.of.HIV..all..per.1.000.uninfected.population." = 
    "Incidence of HIV, all (per 1,000 uninfected population)",
  "Incidence.of.malaria..per.1.000.population.at.risk." = 
    "Incidence of malaria (per 1,000 population at risk)",
  "Incidence.of.tuberculosis..per.100.000.people." = 
    "Incidence of tuberculosis (per 100,000 people)",
  "Tuberculosis.case.detection.rate.....all.forms." = 
    "Tuberculosis case detection rate (%, all forms)",
  "Tuberculosis.treatment.success.rate....of.new.cases." = 
    "Tuberculosis treatment success rate (% of new cases)",
  "NUMBER.OF.DALYS" = 
    "Number of DALYs",
  "NUMBER.OF.DEATHS" = 
    "Number of Deaths",
  "DEATH.RATE" = 
    "Death Rate",
  "Number.of.people.requiring.treatment.against.neglected.tropical.diseases" = 
    "Number of people requiring treatment against neglected tropical diseases",
  "Number.of.people.with.mild.or.severe.anemia.from.neglected.tropical.diseases" = 
    "Number of people with mild or severe anemia from neglected tropical diseases",
  "Death.rate.from.venomous.snakes" = 
    "Death rate from venomous snakes",
  "Deaths.from.cysticercosis" = 
    "Deaths from cysticercosis",
  "Deaths.from.rabies.by.world.region" = 
    "Deaths from rabies by world region",
  "Dengue.fever.deaths" = 
    "Dengue fever deaths",
  "Number.of.people.requiring.preventive.treatment.for.lymphatic.filariasis" = 
    "Number of people requiring preventive treatment for lymphatic filariasis",
  "Number.of.people.requiring.preventive.treatment.for.schistosomiasis" = 
    "Number of people requiring preventive treatment for schistosomiasis",
  "Reported.cases.of.leprosy" = 
    "Reported cases of leprosy",
  "Antibiotic.consumption.rate" = 
    "Antibiotic consumption rate"
)

# Define disease categories
hiv_indicators <- c(
  "Antiretroviral therapy coverage (% of people living with HIV)",
  "Antiretroviral therapy coverage for PMTCT (% of pregnant women living with HIV)",
  "Children (0-14) living with HIV",
  "Children (ages 0-14) newly infected with HIV",
  "Incidence of HIV, all (per 1,000 uninfected population)"
)

tb_indicators <- c(
  "Incidence of tuberculosis (per 100,000 people)",
  "Tuberculosis case detection rate (%, all forms)",
  "Tuberculosis treatment success rate (% of new cases)"
)

malaria_indicators <- c(
  "Incidence of malaria (per 1,000 population at risk)",
  "Children with fever receiving antimalarial drugs (% of children under age 5 with fever)"
)

ntd_indicators <- c(
  "Number of people requiring treatment against neglected tropical diseases",
  "Number of people with mild or severe anemia from neglected tropical diseases",
  "Number of people requiring preventive treatment for lymphatic filariasis",
  "Number of people requiring preventive treatment for schistosomiasis",
  "Reported cases of leprosy"
)

ntd_deaths_indicators <- c(
  "Death rate from venomous snakes",
  "Deaths from cysticercosis",
  "Deaths from rabies by world region",
  "Dengue fever deaths"
)

general_indicators <- c(
  "Number of DALYs",
  "Number of Deaths",
  "Death Rate",
  "Antibiotic consumption rate"
)

# Step 1: Create a rank column for coefficients within each indicator and add clean names
income_regression_ranked <- income.regression %>%
  group_by(dependent_var) %>%
  mutate(
    # Rank by actual value (not absolute value)
    coef_rank = rank(estimate, ties.method = "min"),
    # Convert ranks to factor levels (1 = lowest value, 4 = highest value)
    rank_category = factor(coef_rank,
                           levels = 1:4,
                           labels = c("Lowest", "Low", "High", "Highest")),
    # Add clean names for indicators
    indicator_name = indicator_names[dependent_var]
  ) %>%
  ungroup()

# Step 2: Create category mapping with clean names
category_mapping <- data.frame(
  indicator_name = c(hiv_indicators, tb_indicators, malaria_indicators, 
                     ntd_indicators, ntd_deaths_indicators, general_indicators),
  category = c(rep("HIV", length(hiv_indicators)),
               rep("Tuberculosis", length(tb_indicators)),
               rep("Malaria", length(malaria_indicators)),
               rep("NTDs", length(ntd_indicators)),
               rep("NTD Deaths", length(ntd_deaths_indicators)),
               rep("General Health", length(general_indicators)))
)

# Merge category labels with data
income_regression_ranked <- income_regression_ranked %>%
  left_join(category_mapping, by = "indicator_name")

# Step 3: Add significance symbols based on p-values
income_regression_ranked <- income_regression_ranked %>%
  mutate(significance = case_when(
    p_value < 0.001 ~ "***",
    p_value < 0.01 ~ "**",
    p_value < 0.05 ~ "*",
    TRUE ~ ""
  ))

# Step 4: Rename income groups to abbreviations
income_abbreviations <- c(
  "High income" = "HIC",
  "Upper middle income" = "UMIC",
  "Lower middle income" = "LMIC",
  "Low income" = "LIC"
)

income_regression_ranked$income_abbrev <- income_abbreviations[income_regression_ranked$income]

# Step 5: Create a label for each cell that includes significance
income_regression_ranked <- income_regression_ranked %>%
  mutate(cell_label = paste0(significance))

# Step 6: Prepare data for plotting by ordering categories and indicators
# Define order of categories
category_order <- c("HIV", "Tuberculosis", "Malaria", "NTDs", "NTD Deaths", "General Health")

# Order indicators within each category
plot_data <- income_regression_ranked %>%
  mutate(category = factor(category, levels = category_order)) %>%
  arrange(category, indicator_name) %>%
  mutate(indicator_ordered = factor(indicator_name, levels = unique(indicator_name[order(match(category, category_order))])))

# Step 7: Create the heatmap with different shapes for positive/negative coefficients
plot <- ggplot(plot_data, 
               aes(x = factor(income_abbrev, levels = c("HIC", "UMIC", "LMIC", "LIC")), 
                   y = indicator_ordered)) +
  # For positive coefficients: use squares (geom_tile)
  geom_tile(data = subset(plot_data, estimate >= 0),
            aes(fill = rank_category), color = "white", size = 0.5) +
  # For negative coefficients: use circles (geom_point with large size)
  geom_point(data = subset(plot_data, estimate < 0),
             aes(fill = rank_category), color = "white", size = 6, shape = 21) +
  # Add significance symbols
  geom_text(aes(label = significance), size = 2.5) +
  scale_fill_brewer(palette = "RdBu", direction = -1) +  # Red-Blue color palette
  facet_grid(category ~ ., scales = "free_y", space = "free_y") +
  labs(
    #title = "Effect of Scientific Publications on Disease Indicators by Income Group",
    #subtitle = "Regression coefficients ranked by magnitude across income groups",
    x = "Country Income Classification",
    y = "",
    fill = "Coefficient\nMagnitude",
    #caption = "* p < 0.05, ** p < 0.01, *** p < 0.001\nCircles indicate negative coefficients, squares indicate positive coefficients"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "right",
    axis.text.y = element_text(size = 8, hjust = 1),
    axis.text.x = element_text(size = 9, face = "bold", angle = 45, hjust = 1),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text(size = 8, hjust = 0),
    strip.text.y = element_text(angle = 0, face = "bold", hjust = 0),
    strip.background = element_rect(fill = "grey95", color = NA),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
  )

# Print the plot
print(plot)

# Save the plot as a high-resolution PDF for publication
#ggsave("disease_indicators_heatmap.pdf", plot, width = 11, height = 14, dpi = 300)







