################################################################################
#### 0. LIBRARIES
################################################################################


library(readxl)
library(rworldmap)
library(dplyr)
library(stringdist)
library(RColorBrewer)
library(ggplot2)
library(dplyr)
library(scales)
library(cowplot)



################################################################################
#### 1. DATA INPUT
################################################################################


data = read_excel("~/Bibliometrix-Export-File-CONSOLIDADO VF.xlsx")



################################################################################
### 2. DEPURATION 
################################################################################

# Replace some regions in data$region
data_bibliometric = data
data_bibliometric$region[data_bibliometric$region == "East Asia & Pacific"] = "South-East Asia"
data_bibliometric$region[data_bibliometric$region == "Latin America & Caribbean"] = "Americas"
data_bibliometric$region[data_bibliometric$region == "Middle East & North Africa"] = "Eastern Mediterranean"
data_bibliometric$region[data_bibliometric$region == "NO"] = NA

# Replace some income groups in data$income
data_bibliometric$income[data_bibliometric$income == "NO"] = NA

# Replace some quartile values
data_bibliometric$Cuartil[data_bibliometric$Cuartil == "-"] = NA

# Eliminate rows with NA in the region column (it is the same as NAs in $income)
data_bibliometric = data_bibliometric[!is.na(data_bibliometric$region), ]

# Convert PY to numeric
data_bibliometric$PY = as.numeric(data_bibliometric$PY)



################################################################################
#### 3. MAPS - OPTIMIZED CODE
################################################################################

# Function to standardize country names and improve matching
standardize_country_names <- function(countries, map_countries) {
  # Dictionary for mapping between your dataset and rworldmap
  country_dict <- list(
    "UNITED STATES" = "United States of America",
    "UNITED KINGDOM" = "United Kingdom",
    "SOUTH KOREA" = "Republic of Korea",
    "RUSSIA" = "Russian Federation",
    "TURKIYE" = "Turkey",
    "COTE D'IVOIRE" = "Ivory Coast",
    "SYRIAN ARAB REPUBLIC" = "Syria",
    "CONGO" = "Republic of Congo",
    "CZECH REPUBLIC" = "Czech Republic",
    "YUGOSLAVIA" = "Serbia",
    "BURMA" = "Myanmar",
    "SWAZILAND" = "eSwatini",
    "PALESTINE" = "Palestine",
    "ZAIRE" = "Democratic Republic of the Congo",
    "BRUNEI DARUSSALAM" = "Brunei",
    "VIRGIN ISLANDS (U.S.)" = "United States Virgin Islands",
    "ST. LUCIA" = "Saint Lucia",
    "TAIWAN" = "Taiwan",
    "KOSOVO" = "Kosovo",
    "NORTH MACEDONIA" = "Macedonia",
    "MYANMAR" = "Myanmar",
    "VIETNAM" = "Viet Nam",
    "CURACAO" = "Curaçao"
    # Add more mappings as needed
  )
  
  standardized <- character(length(countries))
  
  for (i in seq_along(countries)) {
    country <- countries[i]
    
    # First check if it's in the dictionary
    if (country %in% names(country_dict)) {
      standardized[i] <- country_dict[[country]]
    } else {
      # Convert to title case for better comparison
      country_title <- tools::toTitleCase(tolower(country))
      
      # Try direct match first
      if (country_title %in% map_countries) {
        standardized[i] <- country_title
      } else {
        # If no direct match, use approximate matching
        distances <- stringdist(country_title, map_countries, method = "jw")
        closest_match <- which.min(distances)
        
        # Only use approximate match if reasonably close
        if (distances[closest_match] < 0.2) {
          standardized[i] <- map_countries[closest_match]
        } else {
          standardized[i] <- country_title # Keep original if no good match
        }
      }
    }
  }
  
  return(standardized)
}

# Unified function to create publication-quality choropleth maps for both publications and citations
create_lancet_choropleth <- function(data, country_col, value_col = NULL, year_col = NULL, 
                                     year_range = NULL, map_type = "publications",
                                     title = NULL, subtitle = NULL, resolution = "high") {
  
  # Set default title based on map_type if not provided
  if (is.null(title)) {
    if (map_type == "publications") {
      title <- "Global Distribution of Neglected Diseases Publications"
    } else {
      title <- "Global Distribution of Neglected Diseases Citations"
    }
  }
  
  # Filter by year range if specified
  if (!is.null(year_col) && !is.null(year_range)) {
    data <- data %>% 
      filter(!!sym(year_col) %in% year_range)
  }
  
  # Get the world map
  world_map <- getMap(resolution = resolution)
  
  # Extract country names from the map
  map_countries <- world_map@data$NAME
  
  # Standardize country names in your data
  data$standardized_country <- standardize_country_names(
    data[[country_col]], 
    map_countries
  )
  
  # Aggregate data by country based on map_type
  if (map_type == "publications") {
    # Count publications by country (each row is one publication)
    aggregated_data <- data %>%
      group_by(standardized_country) %>%
      summarise(total_value = n()) %>%
      as.data.frame()
  } else {
    # Sum citations by country
    aggregated_data <- data %>%
      group_by(standardized_country) %>%
      summarise(total_value = sum(!!sym(value_col), na.rm = TRUE)) %>%
      as.data.frame()
  }
  
  # Create dataframe to join with the map
  join_data <- data.frame(
    country = aggregated_data$standardized_country,
    value = aggregated_data$total_value,
    stringsAsFactors = FALSE
  )
  
  # Check for unmatched countries
  matched_countries <- join_data$country %in% map_countries
  if (sum(!matched_countries) > 0) {
    unmatched <- unique(join_data$country[!matched_countries])
    cat("The following countries could not be matched to the map:\n")
    cat(paste(unmatched, collapse = ", "), "\n")
  }
  
  # Join data to map
  joined_data <- joinCountryData2Map(
    join_data,
    joinCode = "NAME",
    nameJoinColumn = "country",
    verbose = TRUE
  )
  
  # Create custom breaks for 10 intervals
  data_values <- joined_data@data$value[!is.na(joined_data@data$value)]
  
  # If we have enough unique values, create 10 quantile breaks
  if (length(unique(data_values)) >= 10) {
    breaks <- unique(quantile(data_values, probs = seq(0, 1, by = 0.1)))
  } else {
    # If less than 10 unique values, use those as breaks
    breaks <- sort(unique(data_values))
  }
  
  # Select color palette based on map_type
  if (map_type == "publications") {
    lancet_palette <- brewer.pal(9, "YlGnBu")  # Blue-green for publications
    legend_title <- "Publication Count"
  } else {
    lancet_palette <- brewer.pal(9, "YlOrRd")  # Yellow-orange-red for citations
    legend_title <- "Citation Count"
  }
  
  # Extend to 10 colors if needed
  if (length(breaks) > 9) {
    lancet_palette <- colorRampPalette(lancet_palette)(length(breaks) - 1)
  }
  
  # Set up the plot parameters
  par(mar = c(0, 0, 2, 0), bg = "white")  # Adjust margins, white background
  
  # Create the map with customized appearance
  map_plot <- mapCountryData(
    joined_data,
    nameColumnToPlot = "value",
    catMethod = breaks,
    colourPalette = lancet_palette,
    missingCountryCol = "gray90",
    addLegend = FALSE,  # We'll add a custom legend
    mapTitle = "",  # Empty title for now, we'll add it separately
    oceanCol = NA,  # No ocean color (transparent)
    aspect = 1.3,
    borderCol = "white",  # White borders between countries
    lwd = 0.5  # Border width
  )
  
  # Add a custom title with proper formatting
  full_title <- title
  if (!is.null(subtitle)) {
    full_title <- paste0(title, "\n", subtitle)
  }
  
  title(main = full_title, cex.main = 1.2, font.main = 2, line = 0)
  
  # Add a custom legend
  legend_colors <- lancet_palette
  legend_labels <- character(length(breaks) - 1)
  
  for (i in 1:(length(breaks) - 1)) {
    if (breaks[i+1] - breaks[i] < 1) {
      # For decimal values
      legend_labels[i] <- sprintf("%.1f - %.1f", breaks[i], breaks[i+1])
    } else {
      # For integer values
      legend_labels[i] <- sprintf("%d - %d", ceiling(breaks[i]), floor(breaks[i+1]))
    }
  }
  
  # Position the legend in the bottom right
  legend("bottomright", 
         legend = legend_labels, 
         fill = legend_colors, 
         title = legend_title, 
         cex = 0.7, 
         bty = "n",  # No box around legend
         bg = "white")
  
  return(map_plot)
}

# Function to save maps in multiple formats
save_maps <- function(data, map_type, output_dir = "~/Desktop/neglectedDiseases/output/") {
  # Define file prefix based on map_type
  file_prefix <- ifelse(map_type == "publications", "publications", "citations")
  
  # Map parameters based on type
  if (map_type == "publications") {
    value_col <- NULL  # Not needed for publications
  } else {
    value_col <- "TC"  # Citation column
  }
  
  # 1. All-time total map
  # Save as PNG
  png_file <- paste0(output_dir, "lancet_total_", file_prefix, "_map.png")
  png(file = png_file, width = 3000, height = 1800, res = 300)
  create_lancet_choropleth(
    data = data,
    country_col = "country",
    value_col = value_col,
    map_type = map_type,
    subtitle = "Historical Total"
  )
  dev.off()
  
  # Save as PDF
  pdf_file <- paste0(output_dir, "lancet_total_", file_prefix, "_map.pdf")
  pdf(file = pdf_file, width = 10, height = 6)
  create_lancet_choropleth(
    data = data,
    country_col = "country",
    value_col = value_col,
    map_type = map_type,
    subtitle = "Historical Total"
  )
  dev.off()
  
  # 2. Recent 5-years map (2019-2023)
  # Save as PNG
  png_file <- paste0(output_dir, "lancet_recent_", file_prefix, "_map.png")
  png(file = png_file, width = 3000, height = 1800, res = 300)
  create_lancet_choropleth(
    data = data,
    country_col = "country",
    value_col = value_col,
    year_col = "PY",
    year_range = 2019:2023,
    map_type = map_type,
    subtitle = "2019-2023"
  )
  dev.off()
  
  # Save as PDF
  pdf_file <- paste0(output_dir, "lancet_recent_", file_prefix, "_map.pdf")
  pdf(file = pdf_file, width = 10, height = 6)
  create_lancet_choropleth(
    data = data,
    country_col = "country",
    value_col = value_col,
    year_col = "PY",
    year_range = 2019:2023,
    map_type = map_type,
    subtitle = "2019-2023"
  )
  dev.off()
  
  # Generate and print summary statistics
  if (map_type == "publications") {
    summary_data <- data %>%
      group_by(country) %>%
      summarise(
        total_count = n(),
        recent_count = sum(PY %in% 2019:2023)
      ) %>%
      arrange(desc(total_count))
    
    print(paste("Publication count summary (top 20 countries):"))
  } else {
    summary_data <- data %>%
      group_by(country) %>%
      summarise(
        total_count = sum(TC, na.rm = TRUE),
        recent_count = sum(ifelse(PY %in% 2019:2023, TC, 0), na.rm = TRUE)
      ) %>%
      arrange(desc(total_count))
    
    print(paste("Citation count summary (top 20 countries):"))
  }
  
  print(head(summary_data, 20))
}

################################################################################
# Generate maps
################################################################################

# 1. Publication Maps
save_maps(data_bibliometric, "publications")

# 2. Citation Maps
save_maps(data_bibliometric, "citations")


# Generate and print summary of publication by countrey
top_country = data_bibliometric %>%
  group_by(country) %>%
  summarise(
    total_publications = n(),
    total_citations = sum(TC, na.rm = TRUE)
  ) %>%
  arrange(desc(total_publications))

# Generate and print summary of recent publications by country
recent_country = data_bibliometric %>%
  filter(PY >= 2019) %>%
  group_by(country) %>%
  summarise(
    recent_publications = n(),
    recent_citations = sum(TC, na.rm = TRUE)
  ) %>%
  arrange(desc(recent_publications))


# save both dataframes in one excel file
output_file <- "~/Supplementary Material 1.xlsx"
write.xlsx(top_country, file = output_file, sheetName = "Top Countries", rowNames = FALSE)
write.xlsx(recent_country, file = output_file, sheetName = "Recent Countries", append = TRUE, rowNames = FALSE)






################################################################################
#### 4. Tables
################################################################################



# Average citations per paper per region
summaryIncome <- data_bibliometric %>%
  group_by(income) %>%
  summarise(
    total_citations = sum(TC, na.rm = TRUE),
    average_citations = mean(TC, na.rm = TRUE),
    total_papers = n(),
    median_h_index = median(`Índice H`, na.rm = TRUE),
    IQR_h_index = IQR(`Índice H`, na.rm = TRUE)
  ) %>%
  arrange(desc(average_citations))








################################################################################
#### 5. LINE PLOTS
################################################################################


# Define Lancet color palette - colorblind friendly
lancet_colors <- c(
  "Americas" = "#0072B2",              # Blue
  "South-East Asia" = "#E69F00",       # Orange
  "Africa" = "#009E73",                # Green
  "Europe" = "#CC79A7",                # Pink
  "Western Pacific" = "#56B4E9",       # Light blue
  "Eastern Mediterranean" = "#D55E00", # Red-brown
  
  # For income groups - will be mapped to abbreviations
  "HIC" = "#0072B2",           # Blue - High income
  "UMIC" = "#009E73",          # Green - Upper middle income
  "LMIC" = "#E69F00",          # Orange - Lower middle income
  "LIC" = "#D55E00"            # Red-brown - Low income
)

# Define income group abbreviations and their logical order
income_abbreviations <- c(
  "High income" = "HIC",
  "Upper middle income" = "UMIC",
  "Lower middle income" = "LMIC",
  "Low income" = "LIC"
)

income_order <- c("HIC", "UMIC", "LMIC", "LIC")

# Function to create minimalist line plots
create_lancet_lineplot <- function(data, group_var, 
                                   title = NULL, 
                                   ylabel = "Number of Publications",
                                   use_abbreviations = FALSE) {
  
  # Set default title if not provided
  if (is.null(title)) {
    group_label <- ifelse(group_var == "region", "Region", "Income Group")
    title <- paste("Publication Trends by", group_label, "(1904-2024)")
  }
  
  # Create a copy of the data to avoid modifying the original
  plot_data_prep <- data
  
  # Apply income group abbreviations if requested and if we're working with income data
  if (use_abbreviations && group_var == "income") {
    # Create a new column with abbreviated income groups
    plot_data_prep <- plot_data_prep %>%
      mutate(income_abbr = factor(income_abbreviations[income], 
                                  levels = income_order))
    
    # Switch to using the abbreviated column
    group_var <- "income_abbr"
  }
  
  # Aggregate data: count publications by year and group
  plot_data <- plot_data_prep %>%
    group_by(PY, !!sym(group_var)) %>%
    summarise(count = n(), .groups = "drop") %>%
    filter(!is.na(!!sym(group_var))) # Remove any NA categories
  
  # Get unique groups for color mapping
  unique_groups <- unique(plot_data[[group_var]])
  
  # Subset the colors to match the groups in the data
  if (use_abbreviations && group_var == "income_abbr") {
    # Use abbreviation-based colors directly
    color_subset <- lancet_colors[as.character(unique_groups)]
  } else {
    color_subset <- lancet_colors[unique_groups]
  }
  
  # Create the base plot
  p <- ggplot(plot_data, aes(x = PY, y = count, color = !!sym(group_var), group = !!sym(group_var))) +
    geom_line(size = 0.8) +
    scale_color_manual(values = color_subset) +
    scale_x_continuous(breaks = seq(1900, 2025, by = 20), 
                       limits = c(min(plot_data$PY), max(plot_data$PY)),
                       expand = c(0.01, 0.01)) +
    scale_y_continuous(labels = comma_format(), expand = c(0.01, 0.05)) +
    labs(
      title = title,
      x = "Year",
      y = ylabel,
      color = ifelse(group_var == "region", "WHO Region", 
                     ifelse(group_var == "income_abbr", "Income Group", "Income Group"))
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0),
      plot.subtitle = element_text(size = 12, hjust = 0, margin = margin(b = 20)),
      axis.title = element_text(size = 12, face = "plain"),
      axis.text = element_text(size = 10, color = "black"),
      legend.position = "bottom",
      legend.title = element_text(size = 11, face = "plain"),
      legend.text = element_text(size = 10),
      legend.key.size = unit(0.8, "lines"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "gray90", size = 0.3),
      panel.border = element_blank(),
      plot.margin = margin(15, 15, 15, 15)
    )
  
  return(p)
}

# Create the line plot by region
plot_by_region <- create_lancet_lineplot(
  data_bibliometric,
  group_var = "region",
  title = "Publication Trends by WHO Region (1904-2024)"
)

# Create the line plot by income group with abbreviations
plot_by_income <- create_lancet_lineplot(
  data_bibliometric,
  group_var = "income",
  title = "Publication Trends by Income Group (1904-2024)",
  use_abbreviations = TRUE  # Use abbreviations for this plot
)

# Display the plots in RStudio
plot_by_region
plot_by_income

# Create a combined figure with both plots
combined_plot <- plot_grid(
  plot_by_region + theme(legend.position = "none"), 
  plot_by_income + theme(legend.position = "none"),
  labels = c("A", "B"),
  ncol = 1,
  align = "v"
)

# Add shared legends at the bottom
legend_region <- get_legend(plot_by_region + theme(legend.position = "bottom"))
legend_income <- get_legend(plot_by_income + theme(legend.position = "bottom"))

# Combined plot with legends
final_plot <- plot_grid(
  combined_plot, 
  legend_region, 
  legend_income,
  ncol = 1, 
  rel_heights = c(4, 0.3, 0.3)
)

# Display the combined plot in RStudio
final_plot









# Function to create filtered plots from 1970 onwards
create_filtered_plot <- function(data, group_var, title_prefix = "Publication Trends by") {
  # Handle income group abbreviations if needed
  if(group_var == "income") {
    # Define income abbreviations and order
    income_abbreviations <- c(
      "High income" = "HIC",
      "Upper middle income" = "UMIC",
      "Lower middle income" = "LMIC",
      "Low income" = "LIC"
    )
    
    income_colors <- c(
      "HIC" = "#0072B2",    # Blue
      "UMIC" = "#009E73",   # Green
      "LMIC" = "#E69F00",   # Orange
      "LIC" = "#D55E00"     # Red-brown
    )
    
    # Create plot with abbreviated income groups
    plot <- data %>%
      filter(PY >= 1970) %>%  # Filter from 1970 onwards
      mutate(group = factor(income_abbreviations[income], 
                            levels = c("HIC", "UMIC", "LMIC", "LIC"))) %>%
      group_by(PY, group) %>%
      summarise(count = n(), .groups = "drop") %>%
      filter(!is.na(group)) %>%
      ggplot(aes(x = PY, y = count, color = group, group = group)) +
      geom_line(size = 0.8) +
      scale_color_manual(values = income_colors) +
      labs(
        title = paste(title_prefix, "Income Group (1970-2024)"),
        color = "Income Group"
      )
  } else {
    # Region colors
    region_colors <- c(
      "Americas" = "#0072B2",              # Blue
      "South-East Asia" = "#E69F00",       # Orange
      "Africa" = "#009E73",                # Green
      "Europe" = "#CC79A7",                # Pink
      "Western Pacific" = "#56B4E9",       # Light blue
      "Eastern Mediterranean" = "#D55E00"  # Red-brown
    )
    
    # Create plot for regions
    plot <- data %>%
      filter(PY >= 1970) %>%  # Filter from 1970 onwards
      group_by(PY, region) %>%
      summarise(count = n(), .groups = "drop") %>%
      filter(!is.na(region)) %>%
      ggplot(aes(x = PY, y = count, color = region, group = region)) +
      geom_line(size = 0.8) +
      scale_color_manual(values = region_colors) +
      labs(
        title = paste(title_prefix, "WHO Region (1970-2024)"),
        color = "WHO Region"
      )
  }
  
  # Add common formatting
  plot <- plot +
    scale_x_continuous(breaks = seq(1970, 2025, by = 10),
                       limits = c(1970, max(data$PY, na.rm = TRUE)),
                       expand = c(0.01, 0.01)) +
    scale_y_continuous(labels = comma_format(), expand = c(0.01, 0.05)) +
    labs(x = "Year", y = "Number of Publications") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0),
      axis.text = element_text(size = 10, color = "black"),
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "gray90", size = 0.3)
    )
  
  return(plot)
}

# Create both plots filtered from 1970 onwards
region_plot_1970 <- create_filtered_plot(data_bibliometric, "region")
income_plot_1970 <- create_filtered_plot(data_bibliometric, "income")

# Display both plots
region_plot_1970
income_plot_1970

# Optional: Create a combined plot
library(cowplot)
combined_1970_plot <- plot_grid(
  region_plot_1970 + theme(legend.position = "none"),
  income_plot_1970 + theme(legend.position = "none"),
  labels = c("A", "B"),
  ncol = 1,
  align = "v"
)

# Add legends at the bottom
legend_region <- get_legend(region_plot_1970 + theme(legend.position = "bottom"))
legend_income <- get_legend(income_plot_1970 + theme(legend.position = "bottom"))

final_1970_plot <- plot_grid(
  combined_1970_plot,
  legend_region,
  legend_income,
  ncol = 1,
  rel_heights = c(4, 0.3, 0.3)
)

# Display the combined plot
final_1970_plot