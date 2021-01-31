##### Padronizando graficos de regressao 

# loading packages
library(ggplot2)


#### 1. Regressions ####
# dados de centroid morfometria
d.centr <- read.delim("data/Centroid_dorsal_total_2, averaged by ind.txt", 
                    stringsAsFactors = FALSE)

l.centr <- read.delim("data/LATERAL_CS_Combined dataset lateral TOTAL, averaged by ind.txt", 
                      stringsAsFactors = FALSE)
head(d.centr)

# excluir um macho com erro
remover <- which(d.centr$Centroid.Size > 24)

# dados de regressao
d.reg <- read.delim("data/Regression alometria, results.txt", 
                  stringsAsFactors = FALSE)


l.reg <- read.delim("data/LATERAL_Regression alometria lateral total, results.txt", 
                    stringsAsFactors = FALSE)

head(d.reg)

names(d.reg)

d.df <- data.frame(sexo = ifelse(d.reg$sexo == "f", "female", "male"),
                   y = d.reg$RegScore1, 
                   x = d.centr$Centroid.Size)

l.df <- data.frame(sexo = ifelse(l.reg$sexo == "f", "female", "male"),
                   y = l.reg$RegScore1, 
                   x = l.centr$Centroid.Size)


# exclundo o macho com erro
d.df <- d.df[-remover, ]

## tamanho do texto e numeros
tamt <- 18
## tamanho dos pontos
tamp <- 3

d.plotreg <- ggplot(d.df, aes(x = x, y = y, color = sexo)) + 
  geom_point(size = tamp, alpha = 0.5, show.legend = FALSE) +
  labs(x = "Centroid size", y = "Regression Score") +
  scale_color_manual(values = cores) +
  #ylim(-0.06, 0.04) + 
  #xlim(14, 24) +
  theme_classic(base_size = tamt)

d.plotreg


l.plotreg <- ggplot(l.df, aes(x = x, y = y, color = sexo)) + 
  geom_point(size = tamp, alpha = 0.5, show.legend = FALSE) +
  labs(x = "Centroid size", y = "Regression Score") +
  scale_color_manual(values = cores) +
  #ylim(-0.06, 0.04) + 
  #xlim(14, 24) +
  theme_classic(base_size = tamt)


l.plotreg

d.plotreg
ggsave("figures/dorsal_regression.tiff")

l.plotreg
ggsave("figures/lateral_regression.tiff")


#### 2. PCA ####
pca.d <- read.delim("data/PCA_dorsal.txt")
pca.l <- read.delim("data/PCA_Lateral2.txt")

head(pca.d)
head(pca.l)

dim(pca.d)
dim(pca.l)
summary(pca.d)

plot.pca.d <- ggplot(pca.d, aes(x = PC1, y = PC2, color = SEXO)) + 
  geom_point(size = tamp, alpha = 0.5, show.legend = FALSE) +
  labs(x = "PC1", y = "PC2") +
  scale_color_manual(values = cores) +
  #ylim(-0.06, 0.04) + 
  #xlim(14, 24) +
  theme_classic(base_size = tamt)

plot.pca.l <- ggplot(pca.l, aes(x = PC1, y = PC2, color = sexo)) + 
  geom_point(size = tamp, alpha = 0.5, show.legend = FALSE) +
  labs(x = "PC1", y = "PC2") +
  scale_color_manual(values = cores) +
  ylim(-0.09, 0.09) + 
  #xlim(14, 24) +
  theme_classic(base_size = tamt)


plot.pca.d
plot.pca.l

# pca.l[pca.l$Id == "m19", ]
# pca.l[38, ]

plot.pca.d
ggsave("figures/dorsal_pca.tiff")

plot.pca.l
ggsave("figures/lateral_pca.tiff")

