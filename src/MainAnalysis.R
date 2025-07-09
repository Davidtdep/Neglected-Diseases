################################################################################
#### 0. LIBRARIES
################################################################################

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(purrr)




################################################################################
#### 1. DATA INPUT
################################################################################


# Read as text an excel
data = read_excel("~/Bibliometrix-Export-File-CONSOLIDADO VF.xlsx", col_types = "text")

################################################################################
### 2. DEPURATION 
################################################################################

# Replace some regions in data$region
data_filtered = data
data_filtered$region[data_filtered$region == "East Asia & Pacific"] = "South-East Asia"
data_filtered$region[data_filtered$region == "Latin America & Caribbean"] = "Americas"
data_filtered$region[data_filtered$region == "Middle East & North Africa"] = "Eastern Mediterranean"
data_filtered$region[data_filtered$region == "NO"] = NA

# Replace some income groups in data$income
data_filtered$income[data_filtered$income == "NO"] = NA

# Replace some quartile values
data_filtered$Cuartil[data_filtered$Cuartil == "-"] = NA

# convert PY to numeric
data_filtered$PY = as.numeric(data_filtered$PY)

# Eliminate rows with NA in the region column (it is the same as NAs in $income)
data_filtered = data_filtered[!is.na(data_filtered$region), ]








################################################################################
# Convert to numeric values the columns 16:70, 77:99
################################################################################


# Improved function to convert various formats to numeric in a robust way
convert_to_numeric_improved <- function(x) {
  # If already NA, keep it as NA
  if (is.na(x)) return(NA)
  
  # If empty, return NA
  if (x == "") return(NA)
  
  # Convert to character
  x <- as.character(x)
  
  # Handle percentages (e.g., "9.9%")
  if (grepl("%", x)) {
    return(suppressWarnings(as.numeric(gsub("%", "", x)) * 0.01))
  }
  
  # Specific handling for scientific notation (e.g., "4.9000000000000002E-2")
  if (grepl("E[-+]?\\d+", x, ignore.case = TRUE)) {
    return(suppressWarnings(as.numeric(x)))
  }
  
  # For other cases, extract the numeric part, preserving numbers, dots, commas, and negative signs
  x_clean <- gsub("[^0-9\\.,\\-]", "", x)
  
  # Remove commas (assuming they are thousands separators)
  x_clean <- gsub(",", "", x_clean)
  
  # Convert to numeric
  return(suppressWarnings(as.numeric(x_clean)))
}

# Define columns to convert
cols_to_convert <- c(16:70, 77:99)

# Count initial NAs
cat("Counting initial NAs...\n")
initial_na_counts <- sapply(data_filtered[, cols_to_convert], function(col) sum(is.na(col)))

# Store a copy of the original data for comparison
original_data <- data_filtered[, cols_to_convert]

# Apply the conversion
cat("Converting columns to numeric format...\n")
for (i in cols_to_convert) {
  data_filtered[[i]] <- sapply(data_filtered[[i]], convert_to_numeric_improved)
}

# Count final NAs
final_na_counts <- sapply(data_filtered[, cols_to_convert], function(col) sum(is.na(col)))

# Check if new NAs were introduced
new_nas <- final_na_counts - initial_na_counts
columns_with_new_nas <- names(new_nas)[new_nas > 0]

# Report results
if (length(columns_with_new_nas) > 0) {
  cat("\nWarning: New NAs were introduced in the following columns:\n")
  for (col in columns_with_new_nas) {
    col_index <- which(names(data_filtered) == col)
    cat("  - Column", col_index, "(", col, "):", initial_na_counts[col], "initial NAs,", 
        final_na_counts[col], "final NAs (", new_nas[col], "new NAs)\n")
    
    # Show examples of problematic values
    col_pos <- match(col, names(original_data))
    if (!is.na(col_pos)) {
      original_col_values <- original_data[[col_pos]]
      converted_col_values <- data_filtered[[col]]
      problem_indices <- which(!is.na(original_col_values) & is.na(converted_col_values))
      
      if (length(problem_indices) > 0) {
        cat("    Examples of values that were converted to NA:\n")
        for (j in 1:min(5, length(problem_indices))) {
          cat("      - Original:", original_col_values[problem_indices[j]], "\n")
          
          # Try manually these problematic values
          original_val <- original_col_values[problem_indices[j]]
          cat("      - Attempting manual conversion for this specific case...\n")
          
          # Specific correction for scientific values with different format
          if (grepl("E", original_val, ignore.case = TRUE)) {
            parts <- strsplit(original_val, "E", ignore.case = TRUE)[[1]]
            if (length(parts) == 2) {
              base <- as.numeric(gsub("[^0-9\\.]", "", parts[1]))
              if (grepl("\\-", parts[2])) {
                exponent <- as.numeric(gsub("[^0-9]", "", parts[2])) * -1
              } else {
                exponent <- as.numeric(gsub("[^0-9]", "", parts[2]))
              }
              result <- base * (10^exponent)
              cat("      - Manual conversion:", result, "\n")
              data_filtered[problem_indices[j], col] <- result
            }
          }
        }
      }
    }
  }
} else {
  cat("\nSuccess: No new NAs were introduced during conversion.\n")
}

# Check NAs after manual corrections
final_fixed_na_counts <- sapply(data_filtered[, cols_to_convert], function(col) sum(is.na(col)))
new_nas_after_fix <- final_fixed_na_counts - initial_na_counts

# Conversion summary
cat("\nConversion summary:\n")
for (i in cols_to_convert) {
  col_name <- names(data_filtered)[i]
  cat("  - Column", i, "(", col_name, "): Now class", 
      class(data_filtered[[i]]), "\n")
}

# NA totals
cat("\nFinal NA summary:\n")
cat("  - Total initial NAs:", sum(initial_na_counts), "\n")
cat("  - Total final NAs:", sum(final_fixed_na_counts), "\n")
cat("  - Difference after corrections:", sum(final_fixed_na_counts) - sum(initial_na_counts), "\n")

# Show columns that still have problems
columns_with_remaining_nas <- names(new_nas_after_fix)[new_nas_after_fix > 0]
if (length(columns_with_remaining_nas) > 0) {
  cat("\nColumns that still have additional NA values:\n")
  for (col in columns_with_remaining_nas) {
    cat("  -", col, ":", new_nas_after_fix[col], "new NAs\n")
  }
  
  # Specific solution for problematic columns with scientific notation
  cat("\nApplying specific solution for remaining problematic columns...\n")
  
  for (col in columns_with_remaining_nas) {
    col_pos <- match(col, names(original_data))
    if (!is.na(col_pos)) {
      original_col_values <- original_data[[col_pos]]
      
      # Recreate the column from scratch with a more specific method
      data_filtered[[col]] <- sapply(original_col_values, function(val) {
        if (is.na(val)) return(NA)
        if (val == "") return(NA)
        
        val_str <- as.character(val)
        
        # Try direct conversion first
        direct_result <- suppressWarnings(as.numeric(val_str))
        if (!is.na(direct_result)) return(direct_result)
        
        # Handle percentages
        if (grepl("%", val_str)) {
          return(suppressWarnings(as.numeric(gsub("%", "", val_str)) * 0.01))
        }
        
        # Handle scientific notation with variations
        if (grepl("E[-+]?\\d+|E-?\\d+", val_str, ignore.case = TRUE)) {
          # Handle cases like 4.9000000000000002E-2 or 6.2E-2
          parts <- strsplit(val_str, "E", ignore.case = TRUE)[[1]]
          if (length(parts) == 2) {
            base <- as.numeric(gsub("[^0-9\\.]", "", parts[1]))
            if (grepl("\\-", parts[2])) {
              exponent <- as.numeric(gsub("[^0-9]", "", parts[2])) * -1
            } else {
              exponent <- as.numeric(gsub("[^0-9]", "", parts[2]))
            }
            return(base * (10^exponent))
          }
        }
        
        # For other cases, clean and convert
        val_clean <- gsub("[^0-9\\.,\\-]", "", val_str)
        val_clean <- gsub(",", "", val_clean)
        return(suppressWarnings(as.numeric(val_clean)))
      })
    }
  }
  
  # Final verification
  final_check_na_counts <- sapply(data_filtered[, cols_to_convert], function(col) sum(is.na(col)))
  final_difference <- final_check_na_counts - initial_na_counts
  
  cat("\nFinal verification after all corrections:\n")
  cat("  - Total initial NAs:", sum(initial_na_counts), "\n")
  cat("  - Total final NAs:", sum(final_check_na_counts), "\n")
  cat("  - Final difference:", sum(final_check_na_counts) - sum(initial_na_counts), "\n")
}

# Save the cleaned data
write.csv(data_filtered, "~/data_filtered.csv", row.names = FALSE)












################################################################################
#### 3. CATEGORIZING INDICATORS
################################################################################

# Save the names of the columns c(16:70, 77:99)
columns_to_group <- names(data_filtered)[c(16:70, 77:96)]


# Health System and Healthcare Access Indicators
health.system.indicators <- c(
  "CURRENT HEALTH EXPENDITURE (% OF GDP)",
  "PHYSICIANS (PER 1,000 PEOPLE)",
  "NURSES AND MIDWIVES (PER 1,000 PEOPLE)",
  "HEALTHCARE ACCESS AND QUALITY",
  "OUT-OF-POCKED EXPENDITURE ON HEALTH"
)

# Disease Burden and Infectious Disease Indicators
disease.indicators <- c(
  "Antiretroviral therapy coverage (% of people living with HIV)",
  "Antiretroviral therapy coverage for PMTCT (% of pregnant women living with HIV)",
  "Children (0-14) living with HIV",
  "Children (ages 0-14) newly infected with HIV",
  "Children with fever receiving antimalarial drugs (% of children under age 5 with fever)",
  "Incidence of HIV, all (per 1,000 uninfected population)",
  "Incidence of malaria (per 1,000 population at risk)",
  "Incidence of tuberculosis (per 100,000 people)",
  "Tuberculosis case detection rate (%, all forms)",
  "Tuberculosis treatment success rate (% of new cases)",
  "NUMBER OF DALYS",
  "NUMBER OF DEATHS",
  "DEATH RATE",
  "Number of people requiring treatment against neglected tropical\r\ndiseases",
  "Number of people with mild or severe anemia from neglected tropical\r\ndiseases",
  "Death rate from venomous snakes",
  "Deaths from cysticercosis",
  "Deaths from rabies by world region",
  "Dengue fever deaths",
  "Number of people requiring preventive treatment for lymphatic filariasis",
  "Number of people requiring preventive treatment for schistosomiasis",
  "Reported cases of leprosy",
  "Antibiotic consumption rate"
)

# Demographic and Population Health Indicators
demographic.indicators <- c(
  "Population in year",
  "Population,ages 65+",
  "Child mortality rate",
  "Life expectancy at birth",
  "Sex ratio",
  "Sex gap in life expectancy",
  "Healthy life expectancy",
  "Lifespan Inequality in wome",
  "Lifespan Inequality in man",
  "International migrant stock, total",
  "Prevalence of anemia among pregnant women (%)",
  "Homelessness rate"
)

# Economic and Poverty Indicators
economic.indicators <- c(
  "GDP per capita",
  "Gini Coefficient",
  "Multidimensional Poverty Index",
  "Number of people living in extreme poverty",
  "Income inequality: Atkinson index",
  "Prevalence of moderate or severe food insecurity in the population (%)",
  "Prevalence of severe food insecurity in the population (%)"
)

# Education and Development Indicators
education.indicators <- c(
  "Share of population with no formal education",
  "Average years of schooling",
  "Elderly Literacy Rate",
  "Youth Literacy Rate",
  "Human Development Index"
)

# Governance and Political Indicators
governance.indicators <- c(
  "Human rights index",
  "Private civil liberties index",
  "LGBT+ legal equality index",
  "Percentage of territory effectively controlled by government",
  "Rigorous and impartial public administration index",
  "State capacity index",
  "Functioning government index",
  "Political corruption index",
  "Corruption Perception Index"
)

# Water, Sanitation, and Hygiene Indicators
wash.indicators <- c(
  "Share of the population using safely managed drinking water sources",
  "Share of the population with access to basic handwashing facilities",
  "Share of the population using basic sanitation service",
  "Share of deaths attributed to unsafe sanitation",
  "Share of people practicing open defecation"
)

# Research and Development Indicators
research.development.indicators <- c(
  "RESEARCH AND DEVELOPMENT EXPENDITURE (% OF GDP)",
  "CHARGES FOR THE USE OF INTELLECTUAL PROPERTY, PAYMENTS (BOP, CURRENT US$)",
  "Annual research & development funding for infectious diseases",
  "higher education institutions offering disciplines related to research for health in 2023 by region",
  "higher education institutions offering disciplines related to research for health in 2023 by income",
  "drugs pipeline for neglected tropical diseases by country",
  "drugs pipeline for neglected tropical diseases by region",
  "drugs pipeline for neglected tropical diseases by income",
  "Distribution of R&D funding flows for neglected diseases by country"
)

# Print the number of indicators in each category to verify
cat("Health system indicators:", length(health.system.indicators), "\n")
cat("Disease indicators:", length(disease.indicators), "\n")
cat("Demographic indicators:", length(demographic.indicators), "\n")
cat("Economic indicators:", length(economic.indicators), "\n")
cat("Education indicators:", length(education.indicators), "\n")
cat("Governance indicators:", length(governance.indicators), "\n")
cat("WASH indicators:", length(wash.indicators), "\n")
cat("R&D indicators:", length(research.development.indicators), "\n")
cat("Total indicators covered:", 
    length(health.system.indicators) + 
      length(disease.indicators) + 
      length(demographic.indicators) + 
      length(economic.indicators) + 
      length(education.indicators) + 
      length(governance.indicators) + 
      length(wash.indicators) + 
      length(research.development.indicators), "\n")







################################################################################
#### 4. AGGREGATION OF INDICATORS
################################################################################


# ------------------------------------------------------
# AGGREGATION OF NEGLECTED DISEASES PUBLICATIONS DATA
# WITH SOCIOECONOMIC AND HEALTH INDICATORS
# ------------------------------------------------------

# ------------------------------------------------------
# DATA DIAGNOSTICS FUNCTION
# ------------------------------------------------------

# Function to check indicator data quality
check_data_quality <- function(data, group_var, time_period = "all") {
  if (time_period == "recent") {
    data <- data %>% filter(PY >= 2000 & PY <= 2024)
    period_desc <- "2000-2024"
  } else {
    period_desc <- "all years"
  }
  
  cat("\nData quality check for", group_var, "dataset (", period_desc, "):\n")
  cat("Dimensions:", dim(data), "\n")
  cat("Number of groups:", length(unique(data[[group_var]])), "\n")
  cat("Years range:", min(data$PY, na.rm = TRUE), "to", max(data$PY, na.rm = TRUE), "\n")
  
  # Check publication counts
  pub_summary <- summary(data$publication_count)
  cat("Publication count summary:", paste(names(pub_summary), pub_summary, sep="=", collapse=", "), "\n")
  
  # Check indicators with data
  indicator_cols <- setdiff(names(data), c(group_var, "PY", "publication_count"))
  non_na_counts <- sapply(data[indicator_cols], function(x) sum(!is.na(x)))
  indicators_with_data <- names(non_na_counts[non_na_counts > 0])
  
  cat("Found", length(indicators_with_data), "indicators with at least some data\n")
  
  # Sample of indicator availability
  if(length(indicators_with_data) > 0) {
    sample_size <- min(5, length(indicators_with_data))
    sample_indicators <- sample(indicators_with_data, sample_size)
    
    for(ind in sample_indicators) {
      non_na <- sum(!is.na(data[[ind]]))
      cat("Indicator", ind, "has", non_na, "non-NA values (", 
          round(100*non_na/nrow(data), 1), "%)\n")
    }
  }
  
  return(indicators_with_data)
}

# ------------------------------------------------------
# AGGREGATION FUNCTIONS
# ------------------------------------------------------

# Aggregate data by region/income and year
aggregate_data <- function(data_filtered, group_var, columns_to_group) {
  # Verify the grouping variable exists
  if (!group_var %in% names(data_filtered)) {
    stop(paste("Grouping variable", group_var, "not found in dataset"))
  }
  
  # Count publications by group and year
  publications_count <- data_filtered %>%
    group_by(!!sym(group_var), PY) %>%
    summarise(publication_count = n(), .groups = "drop")
  
  # For each indicator, calculate the weighted average by country within each group and year
  indicators_by_group <- data.frame()
  
  for (col_name in columns_to_group) {
    # Skip if column doesn't exist
    if (!col_name %in% names(data_filtered)) {
      warning(paste("Column", col_name, "not found in dataset. Skipping."))
      next
    }
    
    # Try to aggregate the indicator
    tryCatch({
      # Check if there's any data for this indicator
      if(all(is.na(data_filtered[[col_name]]))) {
        warning(paste("Column", col_name, "contains only NA values. Skipping."))
        next
      }
      
      # Group by region/income and year, then calculate weighted average of the indicator
      indicator_agg <- data_filtered %>%
        group_by(!!sym(group_var), PY, country) %>%
        # First aggregate by country within group and year
        summarise(indicator_value = mean(!!sym(col_name), na.rm = TRUE),
                  country_pubs = n(),
                  .groups = "drop_last") %>%
        # Then aggregate by group and year, weighting by publications count
        summarise(!!col_name := weighted.mean(indicator_value, w = country_pubs, na.rm = TRUE),
                  .groups = "drop")
      
      # If this is the first indicator, create the dataframe
      if (nrow(indicators_by_group) == 0) {
        indicators_by_group <- indicator_agg
      } else {
        # Otherwise join with the existing dataframe
        indicators_by_group <- full_join(
          indicators_by_group, 
          indicator_agg, 
          by = c(group_var, "PY")
        )
      }
      
      cat("Successfully aggregated indicator:", col_name, "\n")
    }, error = function(e) {
      warning(paste("Error aggregating indicator", col_name, ":", e$message))
    })
  }
  
  # Join publications count with aggregated indicators
  result <- full_join(publications_count, indicators_by_group, by = c(group_var, "PY"))
  
  # Handle missing values
  result <- result %>%
    mutate(across(everything(), ~ifelse(is.nan(.), NA, .)))
  
  cat("Aggregated data has", ncol(result)-2, "indicators and", nrow(result), "rows\n")
  
  return(result)
}



# Aggregate data by income group and year
cat("\n===== AGGREGATING DATA BY INCOME GROUP =====\n")
income_data <- aggregate_data(data_filtered, "income", columns_to_group)
write.csv(income_data, "~/Desktop/neglectedDiseases/data/aggregated_data_by_income.csv", row.names = FALSE)
cat("Income group data aggregation complete.\n")

# Check quality of income data, focusing on 2000-2024
check_data_quality(income_data, "income", time_period = "recent")

# Print completion message
cat("\nData aggregation complete. Aggregated datasets saved to CSV files.\n")


# elminate everything excerpt data and data_filtered
rm(list = setdiff(ls(), c("data", "data_filtered", "region_data", "income_data")))








################################################################################
#### 5. REGRESION ANALYSIS
################################################################################


# Input the datasets maintaining the name of the columns
data_filtered <- read.csv("~/Desktop/neglectedDiseases/data/data_filtered.csv")
income_data <- read.csv("~/Desktop/neglectedDiseases/data/aggregated_data_by_income.csv")



# Filter years 2000 - 2024 in income_data
income_data_filtered <- income_data %>%
  filter(PY >= 2000 & PY <= 2024)


# ------------------------------------------------------
# CATEGORIZING INDICATORS AS DEPENDENT OR INDEPENDENT VARIABLES
# ------------------------------------------------------

# Define which indicators are likely dependent variables (health outcomes) 
# vs independent variables (resources/investments)
categorize_indicators <- function(columns_to_group) {
  # Keywords that suggest a variable is a dependent outcome
  outcome_keywords <- c(
    "prevalence", "incidence", "deaths", "mortality", "case", "dalys",
    "people requiring treatment", "anemia", "death rate", "requiring preventive",
    "reported cases", "attributed to", "homelessness"
  )
  
  # Keywords that suggest a variable is an independent predictor
  predictor_keywords <- c(
    "expenditure", "physicians", "nurses", "research and development", 
    "funding", "intellectual property", "education", "index", 
    "coefficient", "gdp", "years of schooling", "literacy", "capacity", "corruption"
  )
  
  # Specific indicators that must be treated as dependent variables (overriding keyword classification)
  forced_dependent_vars <- c(
    "DEATH.RATE",
    "Healthy.life.expectancy",
    "Antiretroviral.therapy.coverage....of.people.living.with.HIV.",
    "Antiretroviral.therapy.coverage.for.PMTCT....of.pregnant.women.living.with.HIV.",
    "Children..0.14..living.with.HIV",
    "Children..ages.0.14..newly.infected.with.HIV",
    "Children.with.fever.receiving.antimalarial.drugs....of.children.under.age.5.with.fever.",
    "HEALTHCARE.ACCESS.AND.QUALITY",
    "Life.expectancy.at.birth",
    "Antibiotic.consumption.rate",
    "Share.of.the.population.using.safely.managed.drinking.water.sources",
    "Share.of.the.population.with.access.to.basic.handwashing.facilities",
    "Share.of.the.population.using.basic.sanitation.service",
    "Share of deaths attributed to unsafe sanitation",
    "Share.of.people.practicing.open.defecation",
    "drugs.pipeline.for.neglected.tropical.diseases.by.country",
    "drugs.pipeline.for.neglected.tropical.diseases.by.region",
    "drugs.pipeline.for.neglected.tropical.diseases.by.income",
    "Number.of.people.requiring.treatment.against.neglected.tropical.diseases",
    "Death.rate.from.venomous.snakes",
    "Number.of.people.requiring.preventive.treatment.for.lymphatic.filariasis",
    "Number.of.people.requiring.preventive.treatment.for.schistosomiasis"
  )
  
  # Initialize empty lists for both categories
  dependent_vars <- character(0)
  independent_vars <- character(0)
  
  # Categorize each indicator
  for (column in columns_to_group) {
    # If it's in the forced dependent list, categorize as dependent
    if (column %in% forced_dependent_vars) {
      dependent_vars <- c(dependent_vars, column)
      next
    }
    
    column_lower <- tolower(column)
    
    # Check if any outcome keywords are in the column name
    is_outcome <- any(sapply(outcome_keywords, function(kw) grepl(tolower(kw), column_lower)))
    
    # Check if any predictor keywords are in the column name
    is_predictor <- any(sapply(predictor_keywords, function(kw) grepl(tolower(kw), column_lower)))
    
    if (is_outcome && !is_predictor) {
      dependent_vars <- c(dependent_vars, column)
    } else if (is_predictor && !is_outcome) {
      independent_vars <- c(independent_vars, column)
    } else if (is_outcome && is_predictor) {
      # For ambiguous cases, default to independent variable
      independent_vars <- c(independent_vars, column)
    } else {
      # If neither clear, default to independent variable
      independent_vars <- c(independent_vars, column)
    }
  }
  
  return(list(dependent_vars = dependent_vars, independent_vars = independent_vars))
}




# ------------------------------------------------------
# MAIN EXECUTION OF CATEGORIZATION
# ------------------------------------------------------

# Save the names of the columns c(16:70, 77:99)
columns_to_group <- names(data_filtered)[c(16:70, 77:96)]

# Categorize indicators as dependent or independent
indicator_categories <- categorize_indicators(columns_to_group)
dependent_vars <- indicator_categories$dependent_vars
independent_vars <- indicator_categories$independent_vars

# Print categorization for review
cat("Dependent variables (outcomes):\n")
cat(paste(dependent_vars, collapse = "\n"), "\n\n")
cat("Independent variables (predictors):\n")
cat(paste(independent_vars, collapse = "\n"), "\n\n")






# ------------------------------------------------------
# REGRESSION ANALYISIS
# ------------------------------------------------------

income_results <- data.frame(
  income = character(),
  dependent_var = character(),
  independent_var = character(),
  r_squared = numeric(),
  adj_r_squared = numeric(),
  p_value = numeric(),
  estimate = numeric(),
  std_error = numeric(),
  t_value = numeric(),
  n_obs = numeric(),
  stringsAsFactors = FALSE
)

# Función para ejecutar las regresiones y almacenar resultados
run_regressions <- function(data_df, group_col, results_df, dependent_vars, independent_vars) {
  groups <- unique(data_df[[group_col]])
  
  for (group in groups) {
    # Filtrar datos para el grupo actual
    group_data <- data_df[data_df[[group_col]] == group, ]
    
    # Procesar variables dependientes
    for (dep_var in dependent_vars) {
      if (dep_var %in% colnames(group_data)) {
        # Modelo: indicador ~ publication_count
        model_formula <- as.formula(paste(dep_var, "~ publication_count"))
        
        # Intentar ajustar el modelo
        tryCatch({
          model <- lm(model_formula, data = group_data)
          summary_model <- summary(model)
          
          # Extraer estadísticas
          r_squared <- summary_model$r.squared
          adj_r_squared <- summary_model$adj.r.squared
          f_stat <- summary_model$fstatistic
          p_value <- if(length(f_stat) > 0) pf(f_stat[1], f_stat[2], f_stat[3], lower.tail = FALSE) else NA
          
          # Coeficientes para la variable independiente
          coef_ind <- summary_model$coefficients["publication_count", , drop = FALSE]
          estimate <- coef_ind[1, "Estimate"]
          std_error <- coef_ind[1, "Std. Error"]
          t_value <- coef_ind[1, "t value"]
          
          # Agregar resultados al dataframe
          new_row <- data.frame(
            group = group,
            dependent_var = dep_var,
            independent_var = "publication_count",
            r_squared = r_squared,
            adj_r_squared = adj_r_squared,
            p_value = p_value,
            estimate = estimate,
            std_error = std_error,
            t_value = t_value,
            n_obs = nobs(model),
            stringsAsFactors = FALSE
          )
          
          colnames(new_row)[1] <- group_col
          results_df <- rbind(results_df, new_row)
        }, error = function(e) {
          # En caso de error, agregar fila con valores NA para las estadísticas
          new_row <- data.frame(
            group = group,
            dependent_var = dep_var,
            independent_var = "publication_count",
            r_squared = NA,
            adj_r_squared = NA,
            p_value = NA,
            estimate = NA,
            std_error = NA,
            t_value = NA,
            n_obs = NA,
            stringsAsFactors = FALSE
          )
          colnames(new_row)[1] <- group_col
          results_df <<- rbind(results_df, new_row)
        })
      }
    }
    
    # Procesar variables independientes
    for (ind_var in independent_vars) {
      if (ind_var %in% colnames(group_data)) {
        # Modelo: publication_count ~ indicador
        model_formula <- as.formula(paste("publication_count", "~", ind_var))
        
        # Intentar ajustar el modelo
        tryCatch({
          model <- lm(model_formula, data = group_data)
          summary_model <- summary(model)
          
          # Extraer estadísticas
          r_squared <- summary_model$r.squared
          adj_r_squared <- summary_model$adj.r.squared
          f_stat <- summary_model$fstatistic
          p_value <- if(length(f_stat) > 0) pf(f_stat[1], f_stat[2], f_stat[3], lower.tail = FALSE) else NA
          
          # Coeficientes para la variable independiente
          coef_ind <- summary_model$coefficients[ind_var, , drop = FALSE]
          estimate <- coef_ind[1, "Estimate"]
          std_error <- coef_ind[1, "Std. Error"]
          t_value <- coef_ind[1, "t value"]
          
          # Agregar resultados al dataframe
          new_row <- data.frame(
            group = group,
            dependent_var = "publication_count",
            independent_var = ind_var,
            r_squared = r_squared,
            adj_r_squared = adj_r_squared,
            p_value = p_value,
            estimate = estimate,
            std_error = std_error,
            t_value = t_value,
            n_obs = nobs(model),
            stringsAsFactors = FALSE
          )
          colnames(new_row)[1] <- group_col
          results_df <- rbind(results_df, new_row)
        }, error = function(e) {
          # En caso de error, agregar fila con valores NA para las estadísticas
          new_row <- data.frame(
            group = group,
            dependent_var = "publication_count",
            independent_var = ind_var,
            r_squared = NA,
            adj_r_squared = NA,
            p_value = NA,
            estimate = NA,
            std_error = NA,
            t_value = NA,
            n_obs = NA,
            stringsAsFactors = FALSE
          )
          colnames(new_row)[1] <- group_col
          results_df <<- rbind(results_df, new_row)
        })
      }
    }
  }
  
  return(results_df)
}


# Ejecutar las regresiones para los grupos de ingresos
income_results <- run_regressions(
  income_data_filtered,
  "income",
  income_results,
  indicator_categories$dependent_vars,
  indicator_categories$independent_vars
)

# Ver una muestra de los resultados
head(income_results)

# Puedes guardar los resultados en archivos CSV si lo deseas
#write.csv(income_results, "~/income_regression_results.csv", row.names = FALSE)





################################################################################
#### 6. META-ANALYSIS
################################################################################

# Cargar las librerías necesarias
library(meta)
library(dplyr)
library(tidyr)

# Function to perform meta-analysis for each variable combination
perform_meta_analysis <- function(results_df, group_col) {
  # Identify all unique combinations of dependent and independent variables
  var_combinations <- results_df %>%
    select(dependent_var, independent_var) %>%
    distinct()
  
  # Create dataframe to store meta-analysis results
  meta_results <- data.frame(
    dependent_var = character(),
    independent_var = character(),
    combined_estimate = numeric(),
    ci_lower = numeric(),
    ci_upper = numeric(),
    z_value = numeric(),
    p_value = numeric(),
    i_squared = numeric(),
    tau_squared = numeric(),
    Q_statistic = numeric(),
    het_p_value = numeric(),
    n_studies = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Perform meta-analysis for each variable combination
  for(i in 1:nrow(var_combinations)) {
    dep_var <- var_combinations$dependent_var[i]
    ind_var <- var_combinations$independent_var[i]
    
    # Filter data for this variable combination
    subset_data <- results_df %>%
      filter(dependent_var == dep_var & independent_var == ind_var) %>%
      filter(!is.na(estimate) & !is.na(std_error))  # Remove missing data
    
    # Only proceed if there are at least 2 studies for meta-analysis
    if(nrow(subset_data) >= 2) {
      # Perform meta-analysis using random effects model
      tryCatch({
        # Convert standard errors to variances (se^2)
        subset_data$variance <- subset_data$std_error^2
        
        # Meta-analysis using meta::metagen
        meta_result <- metagen(
          TE = estimate,          # Effect size (coefficients)
          seTE = std_error,       # Standard error
          studlab = subset_data[[group_col]],  # Study labels
          data = subset_data,
          sm = "SMD",             # Standardized mean difference
          method.tau = "REML",    # REML method for tau^2 estimation
          hakn = TRUE,            # Hartung-Knapp-Sidik-Jonkman correction
          prediction = TRUE       # Include prediction interval
        )
        
        # Extract relevant results
        new_row <- data.frame(
          dependent_var = dep_var,
          independent_var = ind_var,
          combined_estimate = meta_result$TE.random,    # Combined estimate
          ci_lower = meta_result$lower.random,          # Lower CI limit
          ci_upper = meta_result$upper.random,          # Upper CI limit
          z_value = meta_result$zval.random,            # Z value
          p_value = meta_result$pval.random,            # P value
          i_squared = meta_result$I2,                   # I² statistic (heterogeneity)
          tau_squared = meta_result$tau2,               # Tau² (between-study variance)
          Q_statistic = meta_result$Q,                  # Q statistic
          het_p_value = meta_result$pval.Q,             # P value for heterogeneity
          n_studies = meta_result$k,                    # Number of included studies
          stringsAsFactors = FALSE
        )
        
        # Add results to dataframe
        meta_results <- rbind(meta_results, new_row)
        
      }, error = function(e) {
        # Handle errors (some meta-analyses may fail for various reasons)
        warning(paste("Error in meta-analysis for", dep_var, "~", ind_var, ":", e$message))
      })
    }
  }
  
  return(meta_results)
}

# Perform meta-analysis for regions
region_meta_results <- perform_meta_analysis(region_results, "region")

# Perform meta-analysis for income groups
income_meta_results <- perform_meta_analysis(income_results, "income")

# Add statistical significance and heterogeneity level as categorical variables
add_interpretation <- function(meta_df) {
  meta_df <- meta_df %>%
    mutate(
      significance = case_when(
        p_value < 0.001 ~ "p<0.001",
        p_value < 0.01 ~ "p<0.01",
        p_value < 0.05 ~ "p<0.05",
        p_value < 0.1 ~ "p<0.1",
        TRUE ~ "ns"
      ),
      heterogeneity = case_when(
        i_squared < 0.25 ~ "Low",
        i_squared < 0.50 ~ "Moderate",
        i_squared < 0.75 ~ "Substantial",
        TRUE ~ "High"
      )
    )
  
  return(meta_df)
}

# Add interpretations
income_meta_results <- add_interpretation(income_meta_results)

# Sort results by significance and effect size
income_meta_results <- income_meta_results %>%
  arrange(p_value, desc(abs(combined_estimate)))

# View results
head(income_meta_results)

# Save results
#write.csv(income_meta_results, "~/income_meta_analysis_results.csv", row.names = FALSE)







################################################################################
#### 7. META-REGRESSION
################################################################################


# Load required libraries
library(meta)
library(metafor)
library(dplyr)
library(tidyr)

# Function to calculate confidence intervals robustly
calculate_confidence_intervals <- function(estimate, se, level = 0.95) {
  # Critical value for desired confidence level
  z_crit <- qnorm((1 + level) / 2)
  
  # Calculate confidence intervals
  ci_lower <- estimate - z_crit * se
  ci_upper <- estimate + z_crit * se
  
  return(list(ci_lower = ci_lower, ci_upper = ci_upper))
}

# Function to perform meta-regression using pre-aggregated data
perform_meta_regression <- function(results_df, meta_results_df, group_col, moderator_vars, aggregated_data) {
  
  cat("Using data with temporal dimension, calculating moderator averages across time...\n")
  
  # Initialize dataframe to store meta-regression results
  metareg_results <- data.frame(
    dependent_var = character(),
    independent_var = character(),
    moderator = character(),
    # Meta-analysis estimates (before moderator)
    meta_estimate = numeric(),
    meta_ci_lower = numeric(),
    meta_ci_upper = numeric(),
    meta_p = numeric(),
    i_squared_original = numeric(),
    # Moderator effects
    moderator_estimate = numeric(),
    moderator_se = numeric(),
    moderator_z = numeric(),
    moderator_p = numeric(),
    moderator_ci_lower = numeric(),
    moderator_ci_upper = numeric(),
    # Heterogeneity measures
    i_squared_with_moderator = numeric(),
    tau_squared = numeric(),
    residual_tau_squared = numeric(),
    tau_squared_reduction = numeric(),
    i_squared_reduction = numeric(),
    # Model fit statistics
    QE = numeric(),
    QE_p = numeric(),
    QM = numeric(),
    QM_p = numeric(),
    k = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Get unique combinations from meta-analysis results
  meta_combinations <- meta_results_df %>%
    select(dependent_var, independent_var) %>%
    distinct()
  
  # For each meta-analysis result
  for(i in 1:nrow(meta_combinations)) {
    dep_var <- meta_combinations$dependent_var[i]
    ind_var <- meta_combinations$independent_var[i]
    
    cat(sprintf("Processing meta-regression for %s ~ %s\n", dep_var, ind_var))
    
    # Get the original meta-analysis results
    meta_result <- meta_results_df %>%
      filter(dependent_var == dep_var & independent_var == ind_var)
    
    # Filter the data for the current dependent and independent variables
    subset_data <- results_df %>%
      filter(dependent_var == dep_var & independent_var == ind_var) %>%
      filter(!is.na(estimate) & !is.na(std_error))
    
    # Only proceed if we have enough studies
    if(nrow(subset_data) < 4) {  # Need at least 4 studies for reliable meta-regression
      cat("  Skipping - insufficient studies (n < 4)\n")
      next
    }
    
    # Calculate variance
    subset_data$variance <- subset_data$std_error^2
    
    # First fit a meta-analysis model to use as reference
    rma_base <- try(rma(yi = estimate, 
                        vi = variance, 
                        data = subset_data,
                        method = "REML"), 
                    silent = TRUE)
    
    if(inherits(rma_base, "try-error")) {
      cat("  Skipping - error in base meta-analysis model\n")
      next
    }
    
    # Get base model statistics
    tau2_base <- rma_base$tau2
    i2_base <- rma_base$I2
    meta_est <- rma_base$b[1]
    meta_p <- rma_base$pval
    meta_se <- sqrt(rma_base$vb[1,1])  # Standard error for the intercept
    
    # Calculate confidence intervals for meta-estimate
    ci <- calculate_confidence_intervals(meta_est, meta_se)
    meta_ci_lower <- ci$ci_lower
    meta_ci_upper <- ci$ci_upper
    
    # For each moderator, fit a meta-regression model
    for(mod_var in moderator_vars) {
      
      # Skip if moderator is the same as independent variable
      if(mod_var == ind_var) {
        next
      }
      
      # Skip if moderator is not in the aggregated data
      if(!(mod_var %in% colnames(aggregated_data))) {
        cat(sprintf("  Skipping moderator %s - not found in aggregated data\n", mod_var))
        next
      }
      
      # Create a mapping of group to moderator value from aggregated data
      # Average across years to get one value per group
      mod_values <- aggregated_data %>%
        select(!!sym(group_col), !!sym(mod_var)) %>%
        group_by(!!sym(group_col)) %>%
        summarize(mod_value = mean(!!sym(mod_var), na.rm = TRUE), .groups = "drop") %>%
        filter(!is.na(mod_value) & is.finite(mod_value))
      
      # Merge with subset data
      analysis_data <- merge(subset_data, mod_values, by = group_col)
      
      # Only proceed if we have enough data after merging
      if(nrow(analysis_data) < 4) {
        cat(sprintf("  Skipping moderator %s - insufficient merged data (n=%d)\n", 
                    mod_var, nrow(analysis_data)))
        next
      }
      
      # Fit meta-regression model
      tryCatch({
        # Fit meta-regression
        rma_mod <- rma(yi = estimate, 
                       vi = variance, 
                       mods = ~ mod_value, 
                       data = analysis_data,
                       method = "REML")
        
        # Extract moderator results
        mod_summary <- summary(rma_mod)
        mod_coef_table <- coef(mod_summary)
        
        # Extract moderator coefficient and standard error
        if(nrow(mod_coef_table) >= 2) {
          mod_est <- mod_coef_table[2, "estimate"]
          mod_se <- mod_coef_table[2, "se"]
          mod_z <- mod_coef_table[2, "zval"]
          mod_p <- mod_coef_table[2, "pval"]
          
          # Calculate confidence intervals for moderator
          ci <- calculate_confidence_intervals(mod_est, mod_se)
          mod_ci_lower <- ci$ci_lower
          mod_ci_upper <- ci$ci_upper
          
          # Get I² after moderator
          i2_mod <- rma_mod$I2
          
          # Calculate reduction in heterogeneity
          tau2_reduction_pct <- ifelse(tau2_base > 0, 
                                       (tau2_base - rma_mod$tau2) / tau2_base * 100, 
                                       0)
          tau2_reduction_pct <- max(0, tau2_reduction_pct)  # Ensure non-negative
          
          i2_reduction_pct <- ifelse(i2_base > 0,
                                     (i2_base - i2_mod) / i2_base * 100,
                                     0)
          i2_reduction_pct <- max(0, i2_reduction_pct)  # Ensure non-negative
          
          # Add results to dataframe
          new_row <- data.frame(
            dependent_var = dep_var,
            independent_var = ind_var,
            moderator = mod_var,
            # Meta-analysis estimates (before moderator)
            meta_estimate = meta_est,
            meta_ci_lower = meta_ci_lower,
            meta_ci_upper = meta_ci_upper,
            meta_p = meta_p,
            i_squared_original = i2_base,
            # Moderator effects
            moderator_estimate = mod_est,
            moderator_se = mod_se,
            moderator_z = mod_z,
            moderator_p = mod_p,
            moderator_ci_lower = mod_ci_lower,
            moderator_ci_upper = mod_ci_upper,
            # Heterogeneity measures
            i_squared_with_moderator = i2_mod,
            tau_squared = tau2_base,
            residual_tau_squared = rma_mod$tau2,
            tau_squared_reduction = tau2_reduction_pct,
            i_squared_reduction = i2_reduction_pct,
            # Model fit statistics
            QE = rma_mod$QE,  # Test for residual heterogeneity
            QE_p = rma_mod$QEp,
            QM = rma_mod$QM,  # Test for moderators (omnibus test)
            QM_p = rma_mod$QMp,
            k = rma_mod$k,    # Number of studies included
            stringsAsFactors = FALSE
          )
          
          metareg_results <- rbind(metareg_results, new_row)
          cat(sprintf("  Success with moderator %s (p = %.4f, I² reduction = %.1f%%)\n", 
                      mod_var, mod_p, i2_reduction_pct))
        } else {
          cat(sprintf("  Warning: Could not extract moderator coefficient for %s\n", mod_var))
        }
      }, error = function(e) {
        cat(sprintf("  Error in meta-regression with moderator %s: %s\n", mod_var, e$message))
      })
    }
  }
  
  # Add significance levels and FDR-adjusted p-values
  if(nrow(metareg_results) > 0) {
    tryCatch({
      # Add significance indicators
      metareg_results <- metareg_results %>%
        mutate(
          meta_significance = case_when(
            meta_p < 0.001 ~ "p<0.001",
            meta_p < 0.01 ~ "p<0.01",
            meta_p < 0.05 ~ "p<0.05",
            meta_p < 0.10 ~ "p<0.10",
            TRUE ~ "ns"
          ),
          moderator_significance = case_when(
            moderator_p < 0.001 ~ "p<0.001",
            moderator_p < 0.01 ~ "p<0.01",
            moderator_p < 0.05 ~ "p<0.05",
            moderator_p < 0.10 ~ "p<0.10",
            TRUE ~ "ns"
          )
        )
      
      # Calculate FDR-adjusted p-values (Benjamini-Hochberg)
      metareg_results <- metareg_results %>%
        group_by(dependent_var, independent_var) %>%
        mutate(
          fdr_p = p.adjust(moderator_p, method = "BH"),
          fdr_significance = case_when(
            fdr_p < 0.001 ~ "p<0.001",
            fdr_p < 0.01 ~ "p<0.01",
            fdr_p < 0.05 ~ "p<0.05",
            fdr_p < 0.10 ~ "p<0.10",
            TRUE ~ "ns"
          )
        ) %>%
        ungroup()
      
      # Sort by p-value, I-squared reduction, and R-squared
      metareg_results <- metareg_results %>%
        arrange(moderator_p, desc(i_squared_reduction))
      
    }, error = function(e) {
      warning(paste("Error adding significance levels:", e$message))
      # Return the original results without the additional columns
    })
  } else {
    warning("No meta-regression results to process")
  }
  
  return(metareg_results)
}


# Perform meta-regression for income groups
cat("Starting meta-regression analysis for income groups...\n")
tryCatch({
  income_metareg_results <- perform_meta_regression(
    income_results, 
    income_meta_results,
    "income",
    indicator_categories$independent_vars,
    income_data_filtered  # Usando income_data_filtered
  )
  
  # Check if we got results
  if(nrow(income_metareg_results) > 0) {
    cat("Successfully completed meta-regression for income groups with", nrow(income_metareg_results), "results\n")
    # Save results to CSV file
    write.csv(income_metareg_results, "~/Desktop/neglectedDiseases/data/income_metaregression_results.csv", row.names = FALSE)
  } else {
    cat("No meta-regression results for income groups\n")
  }
}, error = function(e) {
  cat("Error in income meta-regression:", e$message, "\n")
})


# Save results to CSV files
write.csv(income_metareg_results, "~/income_metaregression_results.csv", row.names = FALSE)
