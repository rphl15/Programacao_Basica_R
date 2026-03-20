# ==========================================
# TESTE DE HIPÓTESE - PREÇO DE VEÍCULOS
# ==========================================

# Dados da amostra
dados <- c(
  167240, 149300, 220000, 159500, 128500,
  120000, 159895, 79500, 79900, 312500,
  141000, 136000, 178000, 119750, 139500,
  96500, 191000, 176000, 122500, 186700
)

# Valor de referência (hipótese nula)
mu0 <- 160000

# Nível de significância
alpha <- 0.05

# Estatísticas básicas
n <- length(dados)
media <- mean(dados)
desvio <- sd(dados)

# Exibindo estatísticas
cat("=====================================\n")
cat("ESTATÍSTICAS DA AMOSTRA\n")
cat("=====================================\n")
cat("Tamanho da amostra:", n, "\n")
cat("Média amostral:", round(media,2), "\n")
cat("Desvio padrão:", round(desvio,2), "\n\n")

# ==========================================
# TESTE t (unilateral à esquerda)
# H0: mu = 160000
# H1: mu < 160000
# ==========================================

teste <- t.test(dados, mu = mu0, alternative = "less")

# Resultado do teste
cat("=====================================\n")
cat("RESULTADO DO TESTE t\n")
cat("=====================================\n")
print(teste)

# Decisão
if (teste$p.value < alpha) {
  cat("\nDecisão: Rejeitamos H0.\n")
  cat("Conclusão: Há evidências de que a média dos preços é menor que 160.000.\n")
} else {
  cat("\nDecisão: Não rejeitamos H0.\n")
  cat("Conclusão: A diferença observada pode ser explicada por variação normal dos dados.\n")
}

