# Carregando pacotes necessários
pacman::p_load("tidyverse", "lubridate", "readr", "scales", "infer")

# Definindo o diretório de trabalho
setwd("~/Lista4/tarefa5/tarefa5")

# Lendo e processando o arquivo CSV
calculo_rotatividade_b <- read_csv("calculo_rotatividade_b.csv") %>%
  mutate(mes = ymd(mes))

# Adicionando colunas 'ano' e 'pos_imp'
calculo_rotatividade_b <- calculo_rotatividade_b %>%
  mutate(ano = year(mes),
         pos_imp = if_else(mes > ymd("2016-08-31"), 1, 0))

# Realizando o teste t para comparar as médias de percent_rotat entre os grupos
t_test_result <- t.test(percent_rotat ~ pos_imp, data = calculo_rotatividade_b)

# Exibindo os resultados do teste t
print(t_test_result)

# Calculando a média e o intervalo de confiança para percent_rotat por pos_imp
summary_stats <- calculo_rotatividade_b %>%
  group_by(pos_imp) %>%
  summarize(
    mean_percent_rotat = mean(percent_rotat, na.rm = TRUE),
    ci_lower = mean_percent_rotat - qt(0.975, df=n()-1) * sd(percent_rotat, na.rm = TRUE)/sqrt(n()),
    ci_upper = mean_percent_rotat + qt(0.975, df=n()-1) * sd(percent_rotat, na.rm = TRUE)/sqrt(n())
  )

# Criando o gráfico com erro bars horizontais e cores diferentes para cada grupo
ggplot(summary_stats, aes(y = factor(pos_imp, labels = c("Pré-Afastamento", "Pós-Afastamento")), x = mean_percent_rotat, color = factor(pos_imp))) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper), height = 0.2) +
  scale_color_manual(values = c("#1F77B4", "#FF7F0E")) + # cores mais escuras
  labs(
    title = "Comparação de médias de rotatividade antes e\ndepois do afastamento de Dilma",
    x = "Média de Percentual de Rotatividade Mensal",
    y = "Período",
    color = "Grupo"
  ) +
  theme_minimal()

# Testando correlação 

# Criando o gráfico com pontos individuais e linha de ajuste linear (eixos invertidos)
ggplot(calculo_rotatividade_b, aes(y = percent_rotat, x = factor(pos_imp, labels = c("Pré-Afastamento", "Pós-Afastamento")), color = factor(pos_imp))) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "black") +
  scale_color_manual(values = c("#1F77B4", "#FF7F0E")) + # cores mais escuras
  labs(
    title = "Rotatividade mensal antes e depois do afastamento de Dilma",
    y = "Percentual de Rotatividade",
    x = "Período",
    color = "Grupo"
  ) +
  theme_minimal()

# Criando o gráfico de densidade

ggplot(calculo_rotatividade_b, aes(x = percent_rotat, fill = factor(pos_imp, labels = c("Pré-Afastamento", "Pós-Afastamento")))) +
  geom_density(alpha = 0.6, color = NA) +
  geom_vline(data = summary_stats, aes(xintercept = mean_percent_rotat, color = factor(pos_imp, labels = c("Pré-Afastamento", "Pós-Afastamento"))), linetype = "dashed", size = 1) +
  scale_fill_manual(values = c("#1F77B4", "#FF7F0E")) +
  scale_color_manual(values = c("#1F77B4", "#FF7F0E")) + 
  labs(
    title = "Distribuição de densidade da rotatividade antes e depois do afastamento de Dilma",
    x = "Percentual de Rotatividade",
    y = "Densidade",
    fill = "Período",
    color = "Período"
  ) +
  theme_minimal()

