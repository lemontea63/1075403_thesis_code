---
title: "chromatogram_production"
author: "Oliver Eyre"
date: "2026-02-25"
output: html_document
---

This R script has been made to help create chromatogram graphs from data for use in a Master's thesis.

```{r Installing packages}
library(ggplot2)
library(dplyr)
library(scales)
library(readr)
library(tidyverse)
library(here)

```

```{r Cleaning data}
raw_data <- read.csv(here("His", "PpPCO_16.10.2024_OE_001.csv"))
colnames(raw_data) <- c("Conc_B_ml", "Conc_B_pct", "Fraction_ml", "Fraction_ID", "UV_ml", "UV_mAU", "unused")

summary(raw_data)

clean_data <- raw_data 

  clean_data$Conc_B_ml <- as.numeric(as.character(clean_data$Conc_B_ml))
  clean_data$Conc_B_pct <- as.numeric(as.character(clean_data$Conc_B_pct)) 
  clean_data$Fraction_ml <- as.numeric(as.character(clean_data$Fraction_ml)) 
  clean_data$Fraction_ID <- as.numeric(as.character(clean_data$Fraction_ID)) 
  clean_data$UV_ml <- as.numeric(as.character(clean_data$UV_ml)) 
  clean_data$UV_mAU <- as.numeric(as.character(clean_data$UV_mAU)) 
  
summary(clean_data)

tidy_data <- clean_data[, -which(names(clean_data) == "unused")]
```

```{r}
uv_data <- tidy_data %>%
  filter(!is.na(UV_ml) & !is.na(UV_mAU)) %>%
  distinct()

conc_b_data <- tidy_data %>%
  filter(!is.na(Conc_B_ml) & !is.na(Conc_B_pct)) %>%
  distinct()

fraction_lines <- tidy_data %>%
  filter(!is.na(Fraction_ml) & !is.na(Fraction_ID)) %>%
  distinct(Fraction_ID, Fraction_ml)
```

```{r}
x_min <- min(conc_b_data$Conc_B_ml)
x_max <- max(conc_b_data$Conc_B_ml)

uv_max_fixed <- 2000 

conc_b_data <- conc_b_data %>%
  mutate(Conc_B_scaled = rescale(Conc_B_pct, to = c(0, uv_max_fixed)))

```

```{r}
p <- ggplot() +
  # UV trace
  geom_line(data = uv_data, aes(x = UV_ml, y = UV_mAU), color = "blue", linewidth = 1.2) +
  
  # Conc B trace
  geom_line(data = conc_b_data, aes(x = Conc_B_ml, y = Conc_B_scaled), color = "green4", linewidth = 1.2) +
  
  # Vertical fraction markers
  geom_vline(data = fraction_lines, aes(xintercept = Fraction_ml), linetype = "dotted", color = "black") +
  
  # Define continuous x-axis with fraction labels as ticks
  scale_x_continuous(
    limits = c(x_min, x_max),
    breaks = fraction_lines$Fraction_ml,
    labels = fraction_lines$Fraction_ID,
    name = "Elution Fraction"
  ) +
  
  # Y-axis scales
  scale_y_continuous(
    name = "UV Absorbance (mAU)",
    limits = c(0, uv_max_fixed),
    breaks = seq(0, uv_max_fixed, by = 500),
    sec.axis = sec_axis(
      ~ rescale(., from = c(0, uv_max_fixed), to = range(conc_b_data$Conc_B_pct)),
      name = "Concentration B (%)"
    )
  ) +
  
  # Theme styling
  theme(
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    
    # Titles
    axis.title.x = element_text(size = 22, margin = margin(t = 15)),
    axis.title.y.left = element_text(color = "blue", size = 22),
    axis.title.y.right = element_text(color = "green4", size = 22),
    
    # Tick labels
    axis.text.x = element_text(angle = 90, vjust = 0.5, size = 19),
    axis.text.y.left = element_text(color = "blue", size = 19),
    axis.text.y.right = element_text(color = "green4", size = 19),
    
    plot.margin = margin(10, 10, 10, 10)
  )

print(p)

```