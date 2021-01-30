##### Testando diferenca de N de lamelas entre machos e femeas
## codigo por Sara Mortara, 1a versao em 20.01.2019
## atualizado em 17.03
 
# # checando o diretorio de trabalho
# getwd()
# # deve estar na pasta de analise de dados, se nao estiver ai sim?
# setwd("C:/Users/morei/OneDrive/Documentos/narinas/analise_dados/R")

## carregando os pacotes
# se nao tiver os pacotes usar:
# install.packages("wesanderson")
library(wesanderson) # para paleta de cores
library(bbmle) # para AICctab
# pacotes graficos
library(ggplot2)
library(gridExtra)
library(ggpubr)

## lendo os dados
dados <- read.csv("data/Dados.csv", sep = ';', dec = ",")
names(dados)[names(dados) == "sexo"] <- "Sex"

dim(dados)
head(dados)

length(unique(dados$exemplar))

# inspecao dos dados
head(dados) # seis primeiras linhas

names(dados)[1] <- "exemplar"

# sumario dos dados
summary(dados)

# checando as dimensoes
dim(dados) # 89 linhas e 7 colunas

# transformando a coluna maturacao em fator
dados$maturacao <- as.factor(dados$maturacao)

summary(dados$maturacao)

table(dados$maturacao, dados$sexo)

# graficos para olhar a distribuicao dos dados
hist(dados$Cpmm)
plot(density(dados$Cpmm))
boxplot(dados$Cpmm)

head(dados)

# mudando o PMF27 de femea para macho
dados[dados$exemplar == "PMF27", 4] <- "M"
# checando
dados[dados$exemplar == "PMF27", 4]

# selecionando colunas
# criando objeto com as colunas das lamelas
lamelas <- dados[, c(2, 3)]

## criando coluna com a media de N de lamelas
dados$lamelas.med <- rowMeans(lamelas, na.rm = TRUE)

head(dados)

# olhando para o padrao dos daods
plot(lamelas.med ~ Cpmm, data = dados, 
     col = dados$sexo)

# y ~ x1 + x2
# lamelas ~ Cpmm + sexo
# y= a + bx1 + cx2
# y = variavel resposta, N de lamelas
# x = variavel preditora
# x1 = comprimento
# x2 = sexo
## fazendo os modelos 

# hipotese nula
m0 <- lm(lamelas.med ~ 1, data = dados) 
# hipotese de que apenas sexo afeta N lamelas
m1 <- lm(lamelas.med ~ sexo, data = dados)
# hipotese do comprimento
m2 <- lm(lamelas.med ~ Cpmm, data = dados)
# hipotese do comprimento e sexo
m3 <- lm(lamelas.med ~ sexo + Cpmm, data = dados)

m0
m1
m2
m3

AICctab(m0, m1, m2, m3, weights = TRUE, base = TRUE)

### fazendo o grafico do resultado dos modelos com ggplot2

## definindo as cores
cores <-  wesanderson::wes_palette("Darjeeling1", 2)[2:1]
cores # femea e macho

## tamanho do texto e numeros
tamt <- 18
## tamanho dos pontos
tamp <- 3

### grafico do modelo
#X11()

r <- ggplot(dados, aes(x = Cpmm, y = lamelas.med, color = Sex)) +
  geom_smooth(method = lm, fill = "grey80") + #, se=FALSE) +
  geom_point(shape = 19,
             size = tamp,
             alpha = 0.5) + # size=3
  labs(x = "Length (mm)", y = "Number of lamellae (mean)") +
  scale_color_manual(values = cores) +
  theme_classic(base_size = tamt) +
  theme(legend.position = "top")

r
#ggsave("figures/regressao_comprimento_lamela.png")

## boxplot

## boxplot das lamelas
bp <- ggplot(dados, aes(x = Sex, y = lamelas.med, fill = Sex)) +
  geom_boxplot() +
  labs(y = "Number of lamellae (mean)", x = "Sex") +
  scale_fill_manual(values = cores) +
  theme_classic(base_size = tamt)

bp
#ggsave("figures/boxplot_lamela.png")


### graficos de densidade
#X11()
yplot <- ggplot(dados, aes(x = lamelas.med, fill = Sex)) +
  labs(x = "Number of lamellae", y = "Density") +
  geom_density(alpha = 0.8) +
  scale_fill_manual(values = cores) +
  theme_classic(base_size = tamt)
#savePlot("lamelaxsexo_densidade.tiff", type="tiff")

xplot <- ggplot(dados, aes(x = Cpmm, fill = Sex)) +
  labs(x = "Length (mm)", y = "Density") +
  geom_density(alpha = 0.8) +
  scale_fill_manual(values = cores) +
  theme_classic(base_size = tamt)

xplot <- xplot + theme_void()
yplot <- yplot + theme_void() + coord_flip()


ggarrange(#xplot, NULL, r, yplot,  
  r, yplot,
  ncol = 2, nrow = 1, align = "hv", 
  widths = c(2, 1), #heights = c(1, 2),
  common.legend = TRUE)

#ggsave("figures/all_graphics.png")

#### testando com a maturacao ####

m <- ggplot(dados, aes(x = maturacao, y = lamelas.med, color = sexo)) +
  #geom_smooth(method=lm, fill="grey80") + #, se=FALSE) +
  geom_boxplot() + # size=3
  labs(x = "Maturacao", y = "N lamelas (média)") +
  #scale_color_manual(values=cor) +
  theme_classic(base_size = tamt)

m



