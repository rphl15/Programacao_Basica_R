# ==========================================
# Exercício 1 - Teste de hipótese para média
# ==========================================

# Dados do problema
n <- 30
media_amostral <- 480
desvio_amostral <- 50
mu0 <- 500
alpha <- 0.05

# Hipóteses
# H0: mu = 500
# H1: mu < 500

# Estatística do teste t
t_calc <- (media_amostral - mu0) / (desvio_amostral / sqrt(n))

# Graus de liberdade
gl <- n - 1

# p-valor para teste unilateral à esquerda
p_valor <- pt(t_calc, df = gl)

# Valor crítico
t_critico <- qt(alpha, df = gl)

# Exibindo resultados
cat("=====================================\n")
cat("TESTE t PARA UMA AMOSTRA\n")
cat("=====================================\n")
cat("Hipótese nula (H0): mu = ", mu0, "\n")
cat("Hipótese alternativa (H1): mu < ", mu0, "\n\n")

cat("Média amostral =", media_amostral, "\n")
cat("Desvio padrão amostral =", desvio_amostral, "\n")
cat("Tamanho da amostra =", n, "\n")
cat("Nível de significância =", alpha, "\n\n")

cat("Estatística t calculada =", round(t_calc, 4), "\n")
cat("Graus de liberdade =", gl, "\n")
cat("Valor crítico =", round(t_critico, 4), "\n")
cat("p-valor =", round(p_valor, 4), "\n\n")


# Decisão
if (p_valor < alpha) {
  cat("Decisão: Rejeitamos H0.\n")
  cat("Conclusão: Há evidências estatísticas de que a vida útil média das baterias é significativamente menor que 500 ciclos.\n")
} else {
  cat("Decisão: Não rejeitamos H0.\n")
  cat("Conclusão: Não há evidências estatísticas suficientes para afirmar que a vida útil média das baterias é menor que 500 ciclos.\n")
}

