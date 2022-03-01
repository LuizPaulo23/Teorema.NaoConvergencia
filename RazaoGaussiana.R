# Para leitura do texto com o código no meu blog: https://medium.com/p/ac6b8a3a237b

rm(list = ls()) # limpando a memória 

# Bibliotecas 

library(tidyverse) # Manipulação dataset 
library(DataExplorer) # Análise Exploratória 
library(GGally) # Matriz de correlação 
library(cowplot) # Junção de Plotes 
library(nortest) # Hipótese de normalidade 
library(aplpack) # BagPlot
library(plotly) # Plotes interativos 


# Plotando a representação da distribuição normal 

x <- pretty(c(-4,4), 100) # intervalo gerado [-4, 4] 
y <- dnorm(x, mean = 0, sd = 1) # Y~N(0,1)
n <- data.frame(y, x) # passando para um data.frame 

# Plotando em um gráfico 

ggplot(n, aes(x = x, y = y)) + 
  geom_line(color = "red", lwd = 1.5, alpha = 0.5) + 
  labs(y = "Probabilidade", x = "", title = "Normal Padrão", 
       caption = "Elaboração de Luiz Paulo T. Gonçalves")+
  theme_bw()

# Gerando duas variáveis com números pseudo-aleatórios 
# Ambas normal padrão Y~N(0,1)

n = 1000 # amostra 

set.seed(123) # Travando a semente geradora de números 
y <- rnorm(n, mean = 0, sd = 1)
set.seed(321) # Semente geradora diferente para x 
x <- rnorm(n, mean = 0, sd = 1)

data.base <- data.frame(y, x) # Passando para um data.frame 

# Visualizando e teste a normalidade de ambas variáveis 

for (feature in data.base) {
  sw <- shapiro.test(feature)
  print(sw) # jogando o teste de normalidade numa repetição 
}


# Plotando em um histograma as densidades 

var.y <-ggplot(aes(x= y),
               data = data.base)+
  geom_histogram(aes(y=..density..), bins = 30L, fill = "blue", alpha = 0.5)+
  geom_density(color="red")+
  labs(title = "Variável Y", y = "Densidade", x = "Y",
       subtitle = "P-valor do teste de Shapiro-Wilk = 0.4765", 
       caption = "n = 1000")+
  theme_bw()

var.x <- ggplot(aes(x= x),
                data = data.base)+
  geom_histogram(aes(y=..density..), bins = 30L, fill = "blue", alpha = 0.5)+
  geom_density(color="red")+
  labs(title = "Variável X", y = "", x = "X",
       subtitle = "P-valor do teste de Shapiro-Wilk = 0.3052", 
       caption = "n = 1000")+
  theme_bw()

plot_grid(var.y, var.x)

# Bagplot com ggplot e plotly

data_bag = compute.bagplot(data.base) # Computando o bagplot 

hull.loop <- data.frame(x = data_bag$hull.loop[,2], y = data_bag$hull.loop[,1])
hull.bag <- data.frame(x = data_bag$hull.bag[,2], y = data_bag$hull.bag[,1])
pxy.outlier <- data.frame(x = data_bag$pxy.outlier[,2], y = data_bag$pxy.outlier[,1])
center <- data.frame(x = data_bag$center[2], y = data_bag$center[1])

# Plotando com o ggplot2

ggplot(data.base) +
  aes(x = y, y = x)+
  geom_polygon(data = hull.loop, fill = "red", alpha = 0.2) +
  geom_polygon(data = hull.bag, fill = "blue", alpha = 0.7) +
  geom_point(data = pxy.outlier, col = "black", pch = 19, alpha = 0.5)+
  geom_point(data = hull.loop, col = "red", size = 2.0)+
  geom_point(data = center, col = "orange", size = 5.0)+
  labs(x = "Variável X", y = "Variável Y",
       title = "BagPlot entre as variáveis X e Y")+
  theme_bw()

# Observando a relação bivariada com correlação de Pearson 

ggpairs(data = data.base, 
        lower = list(continuos = "cor"))+
  ggtitle("Correlação de Pearson entre as variáveis")+
  theme_bw()

# Criando a variável Z (variável razão)

data.base <- data.base %>% 
  mutate(z = y/x) # Variável razão 

shapiro.test(data.base$z) # Teste de normalidade 

ggplot2::ggplot(data = data.base)+
  aes(x = z)+
  geom_histogram(bins = 30L, fill = "blue", alpha = 0.5)+
  labs(y = "Contagem", x = "Z", title = "Z = Y/X", 
       subtitle = "P-valor do teste de Shapiro-Wilk = 2.2e-16")+
  ylim(0, 1000)+
  theme_bw()

# Convergência de Z para distribuição Gaussiana 
# Simulação 

r = 100 # Replicações

y <- data.frame(replicate(r, rnorm(n, mean = 0, sd = 1)))
x <- data.frame(replicate(r, rnorm(n, mean = 0, sd = 1)))
z = (y/x) # Razao entre as normais padrão 

tiragem <- data.frame(apply(z, 2, mean)) # Tirando a media da razao Z 

tiragem <- tiragem %>% 
  rename("simulation" = "apply.z..2..mean.") # Renomeando 

a <- ggplot() +
  aes(x = tiragem$simulation) +
  geom_density(adjust = 1L, 
               fill = "blue", alpha = 0.5, size= 0.1, col="blue")+
  labs(x="", y = "Densidade",
       title = "Distribuição de 100 médias retiradas da variável razão Z", 
       subtitle = "Elaboração de Luiz Paulo Tavares Gonçalves", 
       caption = "P-valor do teste de Shapiro-Wilk = 2.2e-16") +
  theme_bw()

print(a)

r = 1000 # Replicações

y <- data.frame(replicate(r, rnorm(n, mean = 0, sd = 1)))
x <- data.frame(replicate(r, rnorm(n, mean = 0, sd = 1)))
z = (y/x) # Razao entre as normais padrão 

tiragem <- data.frame(apply(z, 2, mean)) # Tirando a media da razao Z 

tiragem <- tiragem %>% 
  rename("simulation" = "apply.z..2..mean.") # Renomeando 

b <- ggplot() +
  aes(x = tiragem$simulation) +
  geom_density(adjust = 1L, 
               fill = "blue", alpha = 0.5, size= 0.1, col="blue")+
  labs(x="", y = "Densidade",
       title = "Distribuição de 1000 médias retiradas da variável razão Z", 
       subtitle = "Elaboração de Luiz Paulo Tavares Gonçalves", 
       caption = "P-valor do teste de Shapiro-Wilk = 2.2e-16") +
  theme_bw()

print(b)

# Média ao longo das replicações 

ggplot(mapping=aes(x= seq_along(1:1000), y = tiragem$simulation)) +
  geom_line(col="red") +
  labs(title= "1000 médias ao longo de 1000 replicações",
       x='Simulações', y='Média', 
       subtitle = "Elaboração de Luiz Paulo Tavares Gonçalves")+
  theme_bw()

# Algumas estatísticas descritivas das médias 

summary(tiragem$simulation)
sd(tiragem$simulation)
var(tiragem$simulation)

# Teste de Shapiro-Wilk para todas as vaiáveis Z 

for (feature in z) {
  sw = shapiro.test(feature)
  print(sw)
}





