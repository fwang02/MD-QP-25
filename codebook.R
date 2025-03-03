library(haven)
dataset <- read.csv("data_with10NA.csv")
str(dataset)

  generate_codebook <- function(df) {
    codebook <- data.frame(
      Variable = character(),
      Type = character(),
      Range = character(),
      Modalities = character(),
      Missing_Code = character(),
      Missing_Percentage = numeric(),
      Unique_Values = character(),
      Meaning = character(),
      stringsAsFactors = FALSE
    )
    
    for (var_name in names(df)) {
      var_data <- df[[var_name]]
      
      # Calcular Missing Code y Missing Percentage
      missing_code <- ifelse(any(is.na(var_data)), "NA", "None")
      missing_percentage <- round(mean(is.na(var_data)) * 100, 2)
      
      # Obtener valores únicos y tipo de variable
      unique_values <- paste(unique(var_data), collapse = ", ")
      
      # Cambiar "class" por "Type" y asignar "Numerical" o "Categorical"
      if (is.numeric(var_data)) {
        type_var <- "Numerical"
        range_val <- paste(min(var_data, na.rm = TRUE), "-", max(var_data, na.rm = TRUE))
        modalities <- "N/A"
      } else if (is.factor(var_data) || is.character(var_data)) {
        type_var <- "Categorical"
        range_val <- "N/A"
        modalities <- paste(levels(factor(var_data)), collapse = ", ")
      }
      
      # Rellenar la tabla del codebook
      codebook <- rbind(codebook, data.frame(
        Variable = var_name,
        Type = type_var,
        Range = range_val,
        Modalities = modalities,
        Missing_Code = missing_code,
        Missing_Percentage = missing_percentage,
        Unique_Values = unique_values,
        Meaning = "Describir variable aquí"  # Puedes añadir una descripción personalizada
      ))
    }
    
    return(codebook)  # Asegúrate de devolver el codebook
  }

# Cargar datos (asegúrate de que el archivo CSV esté en el directorio correcto)
dataset <- read.csv("data_with10NA.csv")

# Ver la estructura de los datos
str(dataset)

# Llamar a la función para generar el codebook
codebook <- generate_codebook(dataset)

# Mostrar el codebook generado
print(codebook)

install.packages("DT")

# Hacer la tabla interactiva donde puedes editar las celdas
datatable(codebook, editable = TRUE)  # editable = TRUE permite editar las celdas

# Al editar, puedes guardar el resultado:
edited_codebook <- as.data.frame(codebook)

codebook$Variable[codebook$Variable == "Unemployment.rate"] <- "Unemployment rate"
codebook$Type[codebook$Variable == "Scholarship.holder"] <- "Binary"
codebook$Range[codebook$Variable == "Target"] <- "-"
codebook$Modalities[codebook$Variable == "Unemployment.rate"] <- "-"
codebook$Meaning[codebook$Variable == "Target"] <- "Student's classification at the end of the academic year"

for (i in seq_len(nrow(codebook))) {
  if (codebook$Type[i] == "Binary") {
    codebook$Unique_Values[i] <- "-"
  }
}
if (!"Type" %in% names(codebook) | !"Unique_Values" %in% names(codebook)) {
  stop("El data.frame 'codebook' debe contener las columnas 'type' y 'unique_values'")
}
install.packages("rmarkdown")
install.packages("knitr")

tinytex::install_tinytex()

rmarkdown::render("Metadata-file.Rmd", output_format = "pdf_document")

install.packages("kableExtra")

