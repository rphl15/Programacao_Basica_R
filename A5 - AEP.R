# Criando os vetores
V <- c(20, 30, 60, 70, 80, 90, 100, 120, 140, 150)
E <- c(13, 15, 18, 17, 20, 22, 19, 24, 26, 26)
P <- c(18, 22, 19, 26, 27, 30, 27, 35, 39, 40)

# Criando o dataframe
dados <- data.frame(V, E, P)

# Visualizar
dados

#Analise de correlação
cor(dados)

#Regressão Linear
modelo <- lm(V ~ E + P, data = dados)

summary(modelo)
