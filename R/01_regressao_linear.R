##### Testando diferenca de N de lamelas entre machos e femeas
## codigo por Sara Mortara, 1a versao em 20.01.2019
## abrir o arquivo .Rproj e seguir este codigo

# 1. arregando os pacotes ------------------------------------------------------
# se nao tiver os pacotes usar:
# install.packages("wesanderson")
library(bbmle) # para AICctab
# pacotes graficos
library(ggplot2)
library(gridExtra)
library(ggpubr)
library(wesanderson) # para paleta de cores

# 2. Lendo e ajustando os dados ------------------------------------------------
## lendo os dados
dados <- read.csv("data/Dados.csv", sep = ';', dec = ",")
names(dados)[names(dados) == "sexo"] <- "Sex"

# sumario dos dados
summary(dados)

# checando as dimensoes
dim(dados) # 89 linhas e 7 colunas

# transformando a coluna maturacao em fator
dados$maturacao <- as.factor(dados$maturacao)

# mudando o PMF27 de femea para macho
dados[dados$exemplar == "PMF27", 4] <- "M"
# checando
dados[dados$exemplar == "PMF27", 4]

# Criando a coluna com o numero medio de lamelas
## criando objeto com as colunas das lamelas
lamelas <- dados[, c(2, 3)]

## criando coluna com a media de N de lamelas
dados$lamelas.med <- rowMeans(lamelas, na.rm = TRUE)

# olhando para o padrao dos daods
plot(lamelas.med ~ Cpmm, data = dados, 
     col = as.factor(dados$Sex))

# 3. Ajuste dos modelos --------------------------------------------------------
# y ~ a + bx
# lamelas ~ Cpmm + Sex
# y = a + bx1 + cx2
# y = variavel resposta, N de lamelas
# x = variavel preditora
# x1 = comprimento
# x2 = Sex
## fazendo os modelos 

# transformando variável Sexo em fator
dados$Sex <- as.factor(dados$Sex)

# Ajuste dos modelos:
# 0. hipotese nula
m0 <- lm(lamelas.med ~ 1, data = dados) 
# 1. hipotese de que apenas Sex afeta N lamelas
m1 <- lm(lamelas.med ~ Sex, data = dados)
# 2. hipotese do comprimento
m2 <- lm(lamelas.med ~ Cpmm, data = dados)
# 3. hipotese do comprimento e Sex
m3 <- lm(lamelas.med ~ Sex + Cpmm, data = dados)
# 4. hipotese da interação entre comprimento e Sex
m4 <- lm(lamelas.med ~ Sex * Cpmm, data = dados)

# Selecao de modelos usando o AICc (AIC corrido para pequenas amostras)
AICctab(m0, m1, m2, m3, m4, weights = TRUE, base = TRUE)


# 4. Graficos ------------------------------------------------------------------
## fazendo o grafico do resultado dos modelos com o pacote ggplot2

## definindo as cores
cores <-  wesanderson::wes_palette("Darjeeling1", 2)[2:1]
cores # femea e macho

## tamanho do texto e numeros
tamt <- 18
## tamanho dos pontos
tamp <- 3

## 4.1. grafico do modelo ----
r <- ggplot(dados, aes(x = Cpmm, y = lamelas.med, color = Sex)) +
  geom_smooth(method = lm, fill = "grey80") +
  geom_point(shape = 19,
             size = tamp,
             alpha = 0.5) + # size=3
  labs(x = "Length (mm)", y = "Number of lamellae (mean)") +
  scale_color_manual(values = cores, name = "", labels = c("Female", "Male")) +
  theme_classic(base_size = tamt) +
  theme(legend.position = "top")

# Salvando o gráfico, para salvar descomentar L 99
r
# ggsave("figures/regressao_comprimento_lamela.tiff")

## 4.2. boxplot das lamelas ----

yplot <- ggplot(dados, aes(x = Sex, y = lamelas.med, fill = Sex)) +
  geom_boxplot() +
  labs(y = "Number of lamellae (mean)", x = "Sex") +
  scale_fill_manual(values = cores) +
  theme_classic(base_size = tamt) +
  theme(legend.position = "none")

# Salvando o gráfico, para salvar descomentar L 112
yplot
#ggsave("figures/boxplot_lamela.tiff")


## 4.3. boxplot do comprimento ----

xplot <- ggplot(dados, aes(x = Sex, y = Cpmm, fill = Sex)) +
  labs(y = "Length (mm)", x = "Sex") +
  geom_boxplot() +
  #geom_density(alpha = 0.8) +
  scale_fill_manual(values = cores) +
  theme_classic(base_size = tamt)

# Removendo os eixos, legendas e virando o boxplot do comprimento
xplot <- xplot + theme_void() + coord_flip()
yplot <- yplot + theme_void()

# Função acessória para extrair a legenda de outro plot
get_legend <- function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

# Guardando a legenda em um objeto
legend <- get_legend(r)

# 4.4. Juntando os graficos em um mesmo painel 
ggarrange(xplot, NULL, r, yplot,  
          #r, yplot,
          ncol = 2, nrow = 2, #align = "h", 
          widths = c(2.2, .8), heights = c(.8, 2.2),
          common.legend = TRUE, 
          legend.grob = legend)

# Salvando o gráfico, para salvar descomentar L 148
#ggsave("figures/all_graphics.tiff")

### testando com a maturacao ####

m <- ggplot(dados, aes(x = maturacao, y = lamelas.med, color = Sex)) +
  #geom_smooth(method=lm, fill="grey80") + #, se=FALSE) +
  geom_boxplot() + # size=3
  labs(x = "Maturacao", y = "N lamelas (média)") +
  #scale_color_manual(values=cor) +
  theme_classic(base_size = tamt)

table(dados$maturacao, dados$Sex)
