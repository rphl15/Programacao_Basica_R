# -----------------------------
# Base de dados
# -----------------------------
dados <- data.frame(
  Dia = 1:12,
  Visitas = c(188, 272, 170, 256, 204, 248,
              256, 233, 178, 175, 168, 166)
)

# -----------------------------
# Média móvel de 3 dias
# -----------------------------
dados$Media_Movel_3 <- stats::filter(
  dados$Visitas,
  rep(1/3, 3),
  sides = 1
)

# -----------------------------
# Exibir tabela
# -----------------------------
print(dados)

# -----------------------------
# Configuração do gráfico
# -----------------------------
par(bg = "white")

# -----------------------------
# Criar gráfico no RStudio
# -----------------------------
plot(
  dados$Dia,
  dados$Visitas,
  type = "o",
  lwd = 2,
  pch = 16,
  col = "blue",
  ylim = c(150, 300),
  xlab = "Dia",
  ylab = "Visitas (milhares)",
  main = "Visitas Diárias e Média Móvel de 3 Dias"
)

# Linha da média móvel
lines(
  dados$Dia,
  dados$Media_Movel_3,
  type = "o",
  lwd = 2,
  pch = 17,
  col = "red"
)

# Grade
grid()

# Legenda
legend(
  "topright",
  legend = c("Visitas Diárias", "Média Móvel (3 dias)"),
  col = c("blue", "red"),
  lty = 1,
  lwd = 2,
  pch = c(16, 17)
)