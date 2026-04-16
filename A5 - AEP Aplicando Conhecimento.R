# Criando os dados
C <- c(500,550,600,620,660,700,750,800,850,900)
M <- c(2,3,3,4,4,5,5,6,6,7)
E <- c(5,5,6,6,7,8,9,10,11,12)

#Criando o data frame
df <- data.frame(C, M, E)

# a) Correlação C e M
corr_CM <- cor(df$C, df$M)

# b) Correlação C e E
corr_CE <- cor(df$C, df$E)

# c) Regressão linear múltipla
modelo <- lm(C ~ M + E, data = df)

# d) Equação
coeficientes <- coef(modelo)

# e) R²
resumo <- summary(modelo)
r2 <- resumo$r.squared

# Resultados
print(paste("Correlação C e M:", corr_CM))
print(paste("Correlação C e E:", corr_CE))


print("Coeficientes:")
print(coeficientes)

print(paste("R²:", r2))
