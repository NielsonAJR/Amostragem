# ==============================================================================
# SCRIPT DE SIMULAÇÃO: AAS COM E SEM REPOSIÇÃO (Estimador Expansão)
# ==============================================================================

# Limpar ambiente
rm(list = ls())

# Pacotes necessários
library(ggplot2)

# --- PASSO 1: Criar a População ---
# Instrução: Tamanho N grande (ex: 5000), fixa, com semente.

set.seed(123) # Semente fixada para reprodutibilidade

N <- 5000 # Tamanho da população

# Gerando uma variável de interesse (y)
# Vamos usar uma distribuição Gamma para simular algo como renda (assimétrica)
# mas poderia ser qualquer distribuição.
y_pop <- rgamma(n = N, shape = 2, scale = 10)

# Parâmetro verdadeiro (Total Populacional - Tau)
tau_verdadeiro <- sum(y_pop)

cat("=== CARACTERÍSTICAS DA POPULAÇÃO ===\n")
cat("Tamanho (N):", N, "\n")
cat("Total Verdadeiro (Tau):", round(tau_verdadeiro, 2), "\n\n")


# --- FUNÇÃO DE SIMULAÇÃO (Engloba Passos 2 e 3) ---
# Criamos uma função para facilitar a repetição no Passo 5 (mudar n)

realizar_simulacao <- function(n_amostra, R_replicas, populacao, N_pop) {
  
  # Vetores para armazenar as estimativas
  estimativas_com_reposicao <- numeric(R_replicas)
  estimativas_sem_reposicao <- numeric(R_replicas)
  
  # Loop das R repetições
  for(r in 1:R_replicas) {
    
    # --- AAS Com Reposição (AASc) ---
    amostra_c <- sample(populacao, size = n_amostra, replace = TRUE)
    # Estimador Expansão: N * média_amostral
    est_c <- N_pop * mean(amostra_c) 
    estimativas_com_reposicao[r] <- est_c
    
    # --- AAS Sem Reposição (AASs) ---
    amostra_s <- sample(populacao, size = n_amostra, replace = FALSE)
    # Estimador Expansão: N * média_amostral
    est_s <- N_pop * mean(amostra_s)
    estimativas_sem_reposicao[r] <- est_s
  }
  
  return(list(
    com_reposicao = estimativas_com_reposicao,
    sem_reposicao = estimativas_sem_reposicao
  ))
}

# --- FUNÇÃO PARA CALCULAR MÉTRICAS (Passo 4) ---
calcular_metricas <- function(estimativas, valor_verdadeiro) {
  viés <- mean(estimativas) - valor_verdadeiro
  viés_relativo <- (viés / valor_verdadeiro) * 100
  variancia <- var(estimativas)
  desvio_padrao <- sd(estimativas)
  eqm <- viés^2 + variancia # Erro Quadrático Médio
  
  return(c(Vies = viés, 
           Vies_Rel_Perc = viés_relativo, 
           Variancia = variancia, 
           DP = desvio_padrao, 
           EQM = eqm))
}

# ==============================================================================
# EXECUÇÃO DO ESTUDO (Passos 2, 3, 4 e 5)
# ==============================================================================

# Configurações
R <- 5000        # Número de réplicas
tamanhos_n <- c(50, 200, 1000) # Variando n (Passo 5: pequeno, médio, grande)

# Loop para rodar a simulação com diferentes tamanhos de amostra
for (n in tamanhos_n) {
  
  cat("========================================\n")
  cat("ANALISANDO PARA TAMANHO DE AMOSTRA n =", n, "\n")
  cat("========================================\n")
  
  # Rodar simulação
  resultados <- realizar_simulacao(n, R, y_pop, N)
  
  # Calcular métricas AAS Com Reposição
  metricas_c <- calcular_metricas(resultados$com_reposicao, tau_verdadeiro)
  
  # Calcular métricas AAS Sem Reposição
  metricas_s <- calcular_metricas(resultados$sem_reposicao, tau_verdadeiro)
  
  # Exibir Tabela Comparativa
  tabela <- rbind(AAS_Com_Rep = metricas_c, AAS_Sem_Rep = metricas_s)
  print(round(tabela, 4))
  
  # --- Gráficos (Histogramas) ---
  # Criando data frame para o ggplot
  df_plot <- data.frame(
    Estimativa = c(resultados$com_reposicao, resultados$sem_reposicao),
    Tipo = c(rep("Com Reposição", R), rep("Sem Reposição", R))
  )
  
  p <- ggplot(df_plot, aes(x = Estimativa, fill = Tipo)) +
    geom_histogram(alpha = 0.5, position = "identity", bins = 50, color="black") +
    geom_vline(xintercept = tau_verdadeiro, linetype="dashed", color = "red", linewidth=1) +
    labs(title = paste("Distribuição das Estimativas (n =", n, ")"),
         subtitle = "Linha vermelha = Total Verdadeiro",
         y = "Frequência", x = "Estimativa do Total") +
    theme_minimal()
  
  print(p)
  
  cat("\n")
}


