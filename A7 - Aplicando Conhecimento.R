# =========================================
# MÉDIA MÓVEL DE 5 DIAS - TEMPERATURAS
# =========================================

# Carregar bibliotecas
library(zoo)
library(ggplot2)

# 1) Simular dados de temperatura máxima diária
temperaturas <- c(
  29, 30, 31, 33, 32,
  34, 35, 36, 34, 33,
  32, 31, 30, 29, 28
)

# Criar sequência de dias
dias <- 1:15

# 2) Organizar os dados em um data frame
dados <- data.frame(
  Dia = dias,
  Temperatura = temperaturas
)

# 3) Calcular média móvel de 5 dias
dados$Media_Movel <- rollmean(
  dados$Temperatura,
  k = 5,
  fill = NA,
  align = "right"
)

# Visualizar tabela
print(dados)

# 4) Gerar gráfico
ggplot(dados, aes(x = Dia)) +
  
  # Série original
  geom_line(aes(y = Temperatura, color = "Temperatura Original"),
            linewidth = 1) +
  
  geom_point(aes(y = Temperatura, color = "Temperatura Original"),
             size = 2) +
  
  # Média móvel
  geom_line(aes(y = Media_Movel, color = "Média Móvel (5 dias)"),
            linewidth = 1.2) +
  
  geom_point(aes(y = Media_Movel, color = "Média Móvel (5 dias)"),
             size = 2) +
  
  labs(
    title = "Temperaturas Máximas e Média Móvel de 5 Dias",
    x = "Dias",
    y = "Temperatura (°C)",
    color = "Séries"
  ) +
  
  theme_minimal()

# =========================================
# INTERPRETAÇÃO
# =========================================

# A série original apresenta oscilações diárias nas temperaturas máximas.
# Já a média móvel de 5 dias suaviza essas variações, permitindo visualizar
# melhor a tendência geral da temperatura ao longo do período.
#
# O gráfico mostra que a linha da média móvel é mais estável e reduz os
# efeitos de variações abruptas presentes na série original.