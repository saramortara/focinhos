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
dados <- read.csv("data/dados.csv", sep=';')

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
dados[dados$exemplar=="PMF27",4] <- "M"
# checando
dados[dados$exemplar=="PMF27",4]

# selecionando colunas
# criando objeto com as colunas das lamelas
lamelas <- dados[,c(2,3)]

## criando coluna com a media de N de lamelas
dados$lamelas.med <- rowMeans(lamelas, na.rm=TRUE)

head(dados)

# olhando para o padrao dos daods
plot(lamelas.med ~ Cpmm, data=dados, 
     col=dados$sexo)

# y ~ x1 + x2
# lamelas ~ Cpmm + sexo
# y= a + bx1 + cx2
# y = variavel resposta, N de lamelas
# x = variavel preditora
# x1 = comprimento
# x2 = sexo
## fazendo os modelos 

# hipotese nula
m0 <- lm(lamelas.med ~ 1, data=dados) 
# hipotese de que apenas sexo afeta N lamelas
m1 <- lm(lamelas.med ~ sexo, data=dados)
# hipotese do comprimento
m2 <- lm(lamelas.med ~ Cpmm, data=dados)
# hipotese do comprimento e sexo
m3 <- lm(lamelas.med ~ sexo + Cpmm, data=dados)

m0
m1
m2
m3

AICctab(m0, m1, m2, m3, weights=TRUE, base=TRUE)

### fazendo o grafico do resultado dos modelos com ggplot2

## definindo as cores
cor <-  wes_palette("Darjeeling1", 2)[2:1]
## tamanho do texto e numeros
tamt <- 18
## tamanho dos pontos
tamp <- 3

### grafico do modelo
#X11()
r <- ggplot(dados, aes(x=Cpmm, y=lamelas.med, color=sexo)) +
  geom_smooth(method=lm, fill="grey80") + #, se=FALSE) +
  geom_point(shape=19, size=tamp, alpha=0.5) + # size=3
  labs(x="Comprimento (mm)", y="N lamelas (média)") +
  scale_color_manual(values=cor) +
  theme_classic(base_size = tamt)

r


#savePlot("lamelaxcomp.tiff", type="tiff")

  ## boxplot
#X11()
ggplot(dados, aes(x=sexo, y=lamelas.med, fill=sexo)) + 
geom_boxplot() +
labs(y="N lamelas", x= "Sexo") +
scale_fill_manual(values=cor) +
theme_classic(base_size = tamt)
#savePlot("../resultados/lamelaxsexo.tiff", type="tiff")
#dev.off()

ggplot(dados, aes(x=sexo, y=Cpmm, fill=sexo)) + 
  geom_boxplot() +
  labs(y="Comprimento", x= "Sexo") +
  scale_fill_manual(values=cor) +
  theme_classic(base_size = tamt)

### graficos de densidade
#X11()
yplot <- ggplot(dados, aes(x=lamelas.med, fill=sexo)) +
  labs(x="N lamelas", y= "Densidade") +
  geom_density(alpha=0.8) +
  scale_fill_manual(values=cor) +
  theme_classic(base_size = tamt)
#savePlot("lamelaxsexo_densidade.tiff", type="tiff")

xplot <- ggplot(dados, aes(x=Cpmm, fill=sexo)) +
  labs(x="Comprimento (mm)", y= "Densidade") +
  geom_density(alpha=0.8) +
  scale_fill_manual(values=cor) +
  theme_classic(base_size = tamt)

xplot <- xplot + clean_theme()
yplot <- yplot + clean_theme() + rotate()

#X11()
#png("figures/all_graphics.png")
ggarrange(xplot, NULL, r, yplot,  
             ncol=2, nrow=2, align = "hv", 
          widths = c(2, 1), heights = c(1, 2),
          common.legend = TRUE)
#dev.off()

boxplot(dados$lamelas.med[dados$sexo=="M"])
boxplot(dados$lamelas.med[dados$sexo=="F"])


dados[dados$sexo=="M",]
#### testando com a maturacao ####

m <- ggplot(dados, aes(x=maturacao, y=lamelas.med, color=sexo)) +
  #geom_smooth(method=lm, fill="grey80") + #, se=FALSE) +
  geom_boxplot() + # size=3
  labs(x="Maturacao", y="N lamelas (média)") +
  #scale_color_manual(values=cor) +
  theme_classic(base_size = tamt)

m



