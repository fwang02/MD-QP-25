
dd <- read.table("data_with10NA.csv",header=T, sep=",");

attach(dd)

# Crear un nuevo dataframe solo con las variables numericas
dd_subset <- dd[, c( 6, 8, 14, 15, 16, 17, 18)]
names(dd_subset)
# Eliminar filas con NA
dd_subset <- na.omit(dd_subset)

#Create all the scatterplots graphs and place them in a pdf
pdf("outputs/numvsnumscatterplot2.pdf")
k <- 1
for (i in 1:6) {
  k = k + 1
  for(j in k:7){
    dd_2 <- dd_subset[, c(i,j)]
    X <- names(dd_subset)[i]
    Y <- names(dd_subset)[j]
    p <- ggplot(dd_2, aes(x = dd_2[,1], y = dd_2[,2])) +
      geom_point(color = "grey") +
      geom_smooth(method = "lm", color = "grey") +  
      labs(
        title = paste("Scatter Plot:", X, "vs", Y),
        x = X,  # Set dynamic x-axis label
        y = Y   # Set dynamic y-axis label 
          )
    print(p)
  }
}
dev.off()



# Obtener los nombres de las variables
var_names <- names(dd_subset)

sink("outputs/numvsnumcorrelacio2.txt")
# Recorrer todas las combinaciones de variables
for (i in 1:(length(var_names) - 1)) {
  for (j in (i + 1):length(var_names)) {
    var1 <- var_names[i]
    var2 <- var_names[j]
    
    # Realizar la prueba de correlación
    resultado <- cor.test(dd_subset[[var1]], dd_subset[[var2]])
    
    # Imprimir los resultados
    cat("\nCorrelation between", var1, "i", var2, "\n")
    cat("Correlation coefficient:", resultado$estimate, "\n")
    cat("p-value:", resultado$p.value, "\n")
    cat("------------\n")
  }
}
sink()

detach(dd)
