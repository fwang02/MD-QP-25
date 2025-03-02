### BIVARIATE ANALYSIS (cat. vs cat.) ###

# Reading the data
data <- read.csv2("data_with10NA.csv",header=T, sep=",")
data


data$Daytime.evening.attendance <- factor(data$Daytime.evening.attendance,
                              levels = c(1, 0),
                              labels = c("Daytime", "Evening"))

data$Displaced <- factor(data$Displaced,
                              levels = c(1, 0),
                              labels = c("Yes", "No"))

data$Educational.special.needs <- factor(data$Educational.special.needs,
                         levels = c(1, 0),
                         labels = c("Yes", "No"))

data$Debtor <- factor(data$Debtor,
                         levels = c(1, 0),
                         labels = c("Yes", "No"))

data$Scholarship.holder <- factor(data$Scholarship.holder,
                         levels = c(1, 0),
                         labels = c("Yes", "No"))

data$Gender <- factor(data$Gender,
                         levels = c(1, 0),
                         labels = c("Male", "Female"))

data$Marital.status <- factor(data$Marital.status,
                              levels = c(1, 2, 3, 4, 5, 6),
                              labels = c("Single", "Married", "Widower", "Divorced", 
                                         "Facto union", "Legally separated"))

data$Application.order <- factor(data$Application.order,
                              levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
                              labels = c("1st", "2nd", "3rd", "4th", "5th", 
                                         "6th","7th", "8th", "9th"))

data$Course <- factor(data$Course,
                      levels = c(33, 171, 8014, 9003, 9070, 9085, 9119, 9130, 9147,
                                 9238, 9254, 9500, 9556, 9670, 9773, 9853, 9991),
                      labels = c("Biofuel Prod. Tech.", "Animation and Multim. Design",
                                 "Social Service (ev.)", "Agronomy", "Comm. Design", 
                                  "Vet. Nursing","Inf. Eng", "Equinculture", "Manag.",
                                 "Social Service", "Toursim", "Nursing", "Oral Hygiene",
                                 "Adv. and Mark. Manag.", "Journalism and Comm.",
                                 "Basic Education", "Manag. (ev.)"))

data$Previous.qualification <- factor(data$Previous.qualification,
                                      levels = c(1, 2, 3, 4, 5, 6, 9, 10, 12,
                                                 14, 15, 19, 38, 39, 40, 42, 43),
                                      labels = c("Secondary Education", "Bachelor's",
                                                 "Degree", "Master's", "PhD", 
                                                 "Freq. Higher Ed.","12th year (n.c.)", "11th year (n.c.)",
                                                 "Other","10th year", "10th (n.c.)", "3rd cycle", "2nd cycle",
                                                 "Tech. course", "Degree-1st cycle",
                                                 "Profess. Course", "Master-2nd cycle"))

data$Nacionality <- factor(data$Nacionality,
                                      levels = c(1, 2, 6, 11, 13, 14, 17, 21, 22,
                                                 24, 25, 26, 32, 41, 62, 100, 101,
                                                 103, 105, 108, 109),
                                      labels = c("Portuguese", "German",
                                                 "Spanish", "Italian", "Dutch", 
                                                 "English","Lithuanian", "Angolan",
                                                 "Cape Verdean","Guinean", "Mozambican", "Santomean", "Turkish",
                                                 "Brazilian", "Romanian",
                                                 "Moldovan", "Mexican", "Ukrainian",
                                                 "Russian", "Cuban", "Colombian"))

data

# CONTINGENCY TABLES
variables <- c("Marital.status", "Daytime.evening.attendance", "Displaced",
               "Application.order", "Course", "Educational.special.needs",
               "Previous.qualification", "Nacionality", "Target", "Debtor",
               "Gender", "Scholarship.holder")

library(gridExtra)
library(grid)

# PDF with all tables
pdf("contingency_tables.pdf", width = 11, height = 8.5)

for (i in 1:length(variables)) {
  for (j in 1:length(variables)) {
    tab <- table(data[[variables[i]]], data[[variables[j]]])
    
    
    grid_table <- tableGrob(tab, theme = ttheme_default(
      core = list(fg_params = list(cex = 0.6)),
      colhead = list(fg_params = list(cex = 0.8)),
      rowhead = list(fg_params = list(cex = 0.8))
    ))
    
    # Title
    title <- textGrob(paste("Contingency Table for", variables[i], "and", variables[j]), 
                      gp = gpar(fontsize = 14, fontface = "bold"))
    
    # Arrange elements
    grid.arrange(title, grid_table, ncol = 1, heights = c(0.2, 1))
  }
}

dev.off()


## MULTIPLE BARPLOTS (NORMALIZED)
library(dplyr)
library(ggplot2)
library(tidyr)

pdf("normalized_barplots.pdf", width = 8.5, height = 11)

library(dplyr)
library(ggplot2)
library(tidyr)

pdf("normalized_barplots.pdf", width = 8.5, height = 11)

for (i in 1:length(variables)) {
  for (j in 1:length(variables)) {
    if (i != j) {  # Avoid self-pairing
      
      # Computing normalized frequencies
      data_prop <- data %>%
        count(.data[[variables[i]]], .data[[variables[j]]]) %>%
        group_by(.data[[variables[i]]]) %>%
        mutate(percentage = n / sum(n) * 100) %>%  # Convert to percentages
        ungroup()
      
      # Creating a complete grid of all possible combinations
      all_combinations <- expand.grid(
        setNames(list(unique(data[[variables[i]]]), unique(data[[variables[j]]])), 
                 c(variables[i], variables[j]))
      )
      
      data_prop_complete <- all_combinations %>%
        left_join(data_prop, by = c(variables[i], variables[j])) %>%
        replace_na(list(percentage = 0, n = 0))
      
      # Creating the normalized bar plot
      p <- ggplot(data_prop_complete, aes(x = .data[[variables[i]]], y = percentage, fill = .data[[variables[j]]])) +
        geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.9) +  # Grouped bars with percentages
        labs(
          title = paste("Normalized Bar Plot of", variables[i], "vs", variables[j]),
          x = variables[i],
          y = "Percentage (%)",
          fill = variables[j]
        ) +
        theme_minimal(base_size = 14) + 
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x labels if needed
        scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20))  # Set y-axis from 0 to 100
      
      print(p)
    }
  }
}

dev.off()

