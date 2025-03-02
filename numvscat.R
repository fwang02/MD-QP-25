data <- read.table("data_with10NA.csv",header=T, sep=",");
attach(data)


# declare the categorical variables
category_mappings <- list(
  Marital.status = list(levels = c(1,2,3,4,5,6), labels = c("single", "married", "widowed", "divorced", "facto union", "legally separated")),
  Application.order = list(levels = c(0,1,2,3,4,5,6,9), labels = c("first choice", "second choice", "third choice", "fourth choice", "fifth choice", "sixth choice", "seventh chioce","last choice")),
  Course = list(levels = c(33,171,8014,9003,9070, 9085, 9119, 9130, 9147, 9238, 9254, 9500, 9556, 9670, 9773, 9853, 9991), labels = c("Biofuel Production Technologies", "Animation and Multimedia Design", "Social Service (evening attendance)", "Agronomy", "Communication Design", "Veterinary Nursing", "Informatics Engineering", "Equinculture", "Management", "Social Service", "Tourism", "Nursing", "Oral Hygiene", "Advertising and Marketing Management", "Journalism and Communication", "Basic Education", "Management (evening attendance)")),
  Daytime.evening.attendance = list(levels = c(0,1), labels = c("evening", "daytime")),
  Previous.qualification = list(levels = c(1,2,3,4,5,6,9,10,12,14,15,19,38,39,40,42,43), labels = c("Secondary education", "Higher education - bachelor's degree", "Higher education - degree", "Higher education - master's", "Higher education - doctorate", "Frequency of higher education", "12th year of schooling - not completed", "11th year of schooling - not completed", "Other - 11th year of schooling", "10th year of schooling", "10th year of schooling - not completed", "Basic education 3rd cycle (9th/10th/11th year) or equiv.", "Basic education 2nd cycle (6th/7th/8th year) or equiv.", "Technological specialization course", "Higher education - degree (1st cycle)", "Professional higher technical course", "Higher education - master (2nd cycle)")),
  Nacionality = list(levels = c(1,2,6,11,13,14,17,21,22,24,25,26,32,41,62,100,101,103,105,108,109), labels = c("Portuguese", "German", "Spanish", "Italian", "Dutch", "English", "Lithuanian", "Angolan", "Cape Verdean", "Guinean", "Mozambican", "Santomean", "Turkish", "Brazilian", "Romanian", "Moldova (Republic of)", "Mexican", "Ukrainian", "Russian", "Cuban", "Colombian")),
  Displaced = list(levels = c(0,1), labels = c("no", "yes")),
  Educational.special.needs = list(levels = c(0,1), labels = c("no", "yes")),
  Debtor = list(levels = c(0,1), labels = c("no", "yes")),
  Gender = list(levels = c(0,1), labels = c("female","male")),
  Scholarship.holder = list(levels = c(0,1), labels = c("no", "yes"))
)

# apply the mappings

for (col in names(category_mappings)) {
  if (class(data[[col]]) == "integer") {
    data[[col]] <- factor(data[[col]], levels = category_mappings[[col]]$levels, labels = category_mappings[[col]]$labels)
    print(paste("Column", col, "has been converted to factor"))
  }
}

data_num <- data[, c( 6, 8, 14, 15, 16, 17, 18)]
data_cat <- data[, c(1,2,3,4,5,7,9,10,11,12,13,19)]
data_num <- na.omit(data_num)
data_cat <- na.omit(data_cat)


# calculating all the summaries by modality and placing them in a .txt

sink("outputs/numvscatsummary.txt")
for (i in 1 : 12){
  for(j in 1 : 7){
    summary_df <- aggregate(data_num[,j] ~ data_cat[,i], data = data, summary)
    cat ("Categorical variable:" ,names(data_cat[i]), " \nNumerical variable:" ,names(data_num[j]),"\n")
    colnames(summary_df) <- c("Category", " ")
    print(summary_df)
    cat("------------------------------------------------------------------------------------\n")
    
  }
}
sink()



# Identificar variables numéricas y categóricas
num_vars <- names(data)[sapply(data, is.numeric)]
cat_vars <- names(data)[sapply(data, is.factor) | sapply(data, is.character)]

# Crear boxplots para cada variable numérica segmentados por cada variable categórica
for (num_var in num_vars) {
  for (cat_var in cat_vars) {
    p <- ggplot(data, aes_string(x = cat_var, y = num_var, fill = cat_var)) +
      geom_boxplot(alpha = 0.7) +
      theme_minimal() +
      labs(title = paste("Boxplot"),
           x = cat_var, y = num_var, fill = cat_var) +
      theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1))
    
    # Guardar gráfico
    ggsave(filename = paste0("boxplot_", num_var, "_por_", cat_var, ".png"), plot = p, width = 10, height = 6)
  }
}

# Creating all the histograms and placing them in a pdf


detach(data)