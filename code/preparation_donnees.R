# 0) Importation des données

library(readr)
df <- read.csv("data/raw/base.csv",
               sep = ",",
               header = TRUE,
               stringsAsFactors = FALSE,
               na.strings = c("", "NA"))

# 1) Nettoyage de la base de données

# 1.1 Traduction / recodage des modalités

df$While.working <- factor(df$While.working,
                           levels = c("Yes", "No"),
                           labels = c("Oui", "Non"))

freq_vars <- grep("^Frequency\\.", names(df), value = TRUE)

df[freq_vars] <- lapply(df[freq_vars], function(x)
  factor(x,
         levels = c("Never", "Rarely", "Sometimes", "Very frequently"),
         labels = c("Jamais", "Rarement", "Parfois", "Très fréquemment"),
         ordered = TRUE))

df$Music.effects <- factor(df$Music.effects,
                           levels = c("Improve", "No effect", "Worsen"),
                           labels = c("Améliore", "Aucun effet", "Aggrave"))

df$Fav.genre <- factor(
  df$Fav.genre,
  levels = c("Classical","Country","EDM","Folk","Gospel","Hip hop","Jazz","K pop",
             "Latin","Lofi","Metal","Pop","R&B","Rap","Rock","Video game music"),
  labels = c("Classique","Country","EDM","Folk","Gospel","Hip-hop","Jazz","K-pop",
             "Latin","Lo-fi","Metal","Pop","R&B","Rap","Rock","Musique de jeu vidéo"))

# 1.2 Suppression des colonnes inutiles

df$Timestamp <- NULL
df$Permissions <- NULL

# 1.3 Conversion des variables numériques

df$Age           <- as.numeric(df$Age)
df$Hours.per.day <- as.numeric(df$Hours.per.day)
df$Anxiety       <- as.numeric(df$Anxiety)
df$Depression    <- as.numeric(df$Depression)
df$Insomnia      <- as.numeric(df$Insomnia)
df$OCD           <- as.numeric(df$OCD)
df$BPM           <- as.numeric(df$BPM)

# 1.4 Conversion des variables qualitatives (facteurs)

df$Primary.streaming.service <- as.factor(df$Primary.streaming.service)
df$While.working             <- as.factor(df$While.working)
df$Instrumentalist           <- as.factor(df$Instrumentalist)
df$Composer                  <- as.factor(df$Composer)
df$Exploratory               <- as.factor(df$Exploratory)
df$Foreign.languages         <- as.factor(df$Foreign.languages)
df$Fav.genre                 <- as.factor(df$Fav.genre)
df$Music.effects             <- as.factor(df$Music.effects)

# 1.5 Conversion des fréquences musicales (facteurs ordonnés)

freq_levels <- c("Jamais", "Rarement", "Parfois", "Très fréquemment")

df$Frequency..Classical.         <- factor(df$Frequency..Classical., levels = freq_levels, ordered = TRUE)
df$Frequency..Country.           <- factor(df$Frequency..Country., levels = freq_levels, ordered = TRUE)
df$Frequency..EDM.               <- factor(df$Frequency..EDM., levels = freq_levels, ordered = TRUE)
df$Frequency..Folk.              <- factor(df$Frequency..Folk., levels = freq_levels, ordered = TRUE)
df$Frequency..Gospel.            <- factor(df$Frequency..Gospel., levels = freq_levels, ordered = TRUE)
df$Frequency..Hip.hop.           <- factor(df$Frequency..Hip.hop., levels = freq_levels, ordered = TRUE)
df$Frequency..Jazz.              <- factor(df$Frequency..Jazz., levels = freq_levels, ordered = TRUE)
df$Frequency..K.pop.             <- factor(df$Frequency..K.pop., levels = freq_levels, ordered = TRUE)
df$Frequency..Latin.             <- factor(df$Frequency..Latin., levels = freq_levels, ordered = TRUE)
df$Frequency..Lofi.              <- factor(df$Frequency..Lofi., levels = freq_levels, ordered = TRUE)
df$Frequency..Metal.             <- factor(df$Frequency..Metal., levels = freq_levels, ordered = TRUE)
df$Frequency..Pop.               <- factor(df$Frequency..Pop., levels = freq_levels, ordered = TRUE)
df$Frequency..R.B.               <- factor(df$Frequency..R.B., levels = freq_levels, ordered = TRUE)
df$Frequency..Rap.               <- factor(df$Frequency..Rap., levels = freq_levels, ordered = TRUE)
df$Frequency..Rock.              <- factor(df$Frequency..Rock., levels = freq_levels, ordered = TRUE)
df$Frequency..Video.game.music.  <- factor(df$Frequency..Video.game.music., levels = freq_levels, ordered = TRUE)

# 1.6 Vérifications (structure + NA)

str(df)

anyNA(df)
sum(is.na(df))
colSums(is.na(df))
rowSums(is.na(df))
summary(df)
unique(df)

# 1.7 Suppression de la variable BPM

df$BPM <- NULL

# 1.8 Imputation MICE

df$Age[is.na(df$Age)] <- median(df$Age, na.rm = TRUE)

library(mice)

ini  <- mice(df, maxit = 0, printFlag = FALSE)
pred <- ini$predictorMatrix
meth <- ini$method

meth["Age"] <- ""
pred["Age", ] <- 0
pred[, "Age"] <- 0

pred[, c("Fav.genre", "Primary.streaming.service")] <- 0
diag(pred) <- 0

meth[c("Age","Hours.per.day","Anxiety","Depression","Insomnia","OCD")] <- "pmm"
meth[c("While.working","Instrumentalist","Composer","Exploratory","Foreign.languages",
       "Primary.streaming.service","Fav.genre","Music.effects")] <- "cart"
meth[freq_vars] <- "cart"

imp <- mice(df,
            m = 5, maxit = 20, seed = 123,
            predictorMatrix = pred, method = meth,
            remove.constant = TRUE, remove.collinear = TRUE,
            printFlag = TRUE)

dff <- complete(imp, 1)

anyNA(dff)
str(dff)

# 1.9 Valeurs extrêmes

vars_quant <- dff[, sapply(dff, is.numeric)]
names(vars_quant)
par(mfrow = c(2, 3))
for (v in names(vars_quant)) {
  boxplot(vars_quant[[v]],
          main = v,
          col = "lightgrey")
}
par(mfrow = c(1, 1))

outliers_summary <- data.frame(
  variable = character(),
  Q1 = numeric(),
  Q3 = numeric(),
  IQR = numeric(),
  seuil_bas = numeric(),
  seuil_haut = numeric(),
  nb_outliers = numeric()
)

for (v in names(vars_quant)) {
  x <- vars_quant[[v]]
  
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR_val <- IQR(x, na.rm = TRUE)
  seuil_bas <- Q1 - 1.5 * IQR_val
  seuil_haut <- Q3 + 1.5 * IQR_val
  
  nb_out <- sum(x < seuil_bas | x > seuil_haut, na.rm = TRUE)
  
  outliers_summary <- rbind(
    outliers_summary,
    data.frame(variable = v,
               Q1 = Q1,
               Q3 = Q3,
               IQR = IQR_val,
               seuil_bas = seuil_bas,
               seuil_haut = seuil_haut,
               nb_outliers = nb_out)
  )
}

outliers_summary

dff$log_heures <- log1p(dff$Hours.per.day)

# 1.10 Changement de nom des variables

library(dplyr)
dff <- dff %>%
  rename(
    age = Age,
    heures_ecoute_par_jour = Hours.per.day,
    anxiete = Anxiety,
    depression = Depression,
    insomnie = Insomnia,
    toc = OCD,
    plateforme_streaming = Primary.streaming.service,
    ecoute_en_travaillant = While.working,
    instrumentiste = Instrumentalist,
    compositeur = Composer,
    ecoute_exploratoire = Exploratory,
    langues_etrangeres = Foreign.languages,
    genre_prefere = Fav.genre,
    effet_musique = Music.effects,
    freq_classique = Frequency..Classical.,
    freq_country = Frequency..Country.,
    freq_edm = Frequency..EDM.,
    freq_folk = Frequency..Folk.,
    freq_gospel = Frequency..Gospel.,
    freq_hip_hop = Frequency..Hip.hop.,
    freq_jazz = Frequency..Jazz.,
    freq_k_pop = Frequency..K.pop.,
    freq_latin = Frequency..Latin.,
    freq_lofi = Frequency..Lofi.,
    freq_metal = Frequency..Metal.,
    freq_pop = Frequency..Pop.,
    freq_rnb = Frequency..R.B.,
    freq_rap = Frequency..Rap.,
    freq_rock = Frequency..Rock.,
    freq_jeu_video = Frequency..Video.game.music.
  )

names(dff)

# 1.11 Exportation

write.csv(dff, "data/processed/base_n.csv", row.names = FALSE)
                        
# 2) Statistiques descriptives

# 2.1 Analyse univariée : variables qualitatives

tab_stream <- sort(table(dff$plateforme_streaming), decreasing = TRUE)
tab_stream
round(prop.table(tab_stream), 3)

barplot(tab_stream, las = 2,
        main = "Plateforme de streaming utilisée",
        ylab = "Effectifs")

tab_work <- table(dff$ecoute_en_travaillant)
tab_work
round(prop.table(tab_work), 3)

barplot(tab_work,
        main = "Écoute de musique en travaillant",
        ylab = "Effectifs")

tab_effet <- sort(table(dff$effet_musique), decreasing = TRUE)
tab_effet
round(prop.table(tab_effet), 3)

barplot(tab_effet,
        main = "Effet perçu de la musique",
        ylab = "Effectifs")

# 2.2 Analyse univariée : préférences musicales

tab_genre <- sort(table(dff$genre_prefere), decreasing = TRUE)
tab_genre
round(prop.table(tab_genre), 3)

barplot(tab_genre, las = 2,
        main = "Répartition des genres musicaux préférés",
        ylab = "Effectifs")

freq_tables <- lapply(freq_vars, function(v) table(dff[[v]]))
names(freq_tables) <- freq_vars
freq_tables

freq_props <- lapply(freq_vars, function(v) round(prop.table(table(dff[[v]])), 3))
names(freq_props) <- freq_vars
freq_props

par(mfrow = c(1,3))
barplot(table(dff$freq_pop),   main = "Fréq. Pop",   ylab = "Effectifs")
barplot(table(dff$freq_rock),  main = "Fréq. Rock",  ylab = "Effectifs")
barplot(table(dff$freq_hip_hop), main = "Fréq. Hip-hop", ylab = "Effectifs")
par(mfrow = c(1,1))

# 2.3 Analyse univariée : santé mentale

vars_sante <- c("anxiete", "depression", "insomnie", "toc")

summary(dff[vars_sante])
sapply(dff[vars_sante], sd)

par(mfrow = c(2,2))
hist(dff$anxiete,    main = "Distribution de l'anxiété",    xlab = "Score", breaks = 10)
hist(dff$depression, main = "Distribution de la dépression", xlab = "Score", breaks = 10)
hist(dff$insomnie,   main = "Distribution de l'insomnie",   xlab = "Score", breaks = 10)
hist(dff$toc,        main = "Distribution des TOC",         xlab = "Score", breaks = 10)
par(mfrow = c(1,1))

boxplot(dff[vars_sante],
        main = "Scores de santé mentale (boxplots)",
        ylab = "Score")

# 2.4 Analyse univariée : intensité d'écoute et âge

summary(dff$heures_ecoute_par_jour)
sd(dff$heures_ecoute_par_jour)

hist(dff$heures_ecoute_par_jour,
     main = "Temps d'écoute quotidien",
     xlab = "Heures par jour",
     col = "lightgreen")

boxplot(dff$heures_ecoute_par_jour,
        main = "Temps d'écoute quotidien (boxplot)",
        ylab = "Heures/jour")

summary(dff$age)
sd(dff$age)

hist(dff$age,
     main = "Distribution de l'âge",
     xlab = "Âge",
     breaks = 12)

boxplot(dff$age,
        main = "Âge (boxplot)",
        ylab = "Âge")

# 3) Analyse bivariée

# 3.1 Quali-quali

tab_genre_effet <- table(dff$genre_prefere, dff$effet_musique)
tab_genre_effet

round(prop.table(tab_genre_effet, 1) * 100, 1)

barplot(tab_genre_effet,
        beside = FALSE,
        legend = TRUE,
        las = 2,
        main = "Effet perçu de la musique selon le genre préféré",
        ylab = "Effectifs")

tab_travail_effet <- table(dff$ecoute_en_travaillant, dff$effet_musique)
tab_travail_effet

round(prop.table(tab_travail_effet, 1) * 100, 1)

barplot(tab_travail_effet,
        beside = FALSE,
        legend = TRUE,
        col = c("darkseagreen3", "indianred2"),
        main = "Effet perçu de la musique selon l'écoute en travaillant",
        ylab = "Effectifs")

tab_stream_genre <- table(dff$plateforme_streaming, dff$genre_prefere)
tab_stream_genre

round(prop.table(tab_stream_genre, 1) * 100, 1)

barplot(tab_stream_genre,
        beside = FALSE,
        legend = FALSE,
        las = 2,
        main = "Genres préférés selon la plateforme de streaming",
        ylab = "Effectifs")

chisq.test(tab_genre_effet)
chisq.test(tab_travail_effet)
chisq.test(tab_stream_genre)

# 3.2 Quanti-quanti

cor.test(dff$log_heures, dff$anxiete, method="pearson")
cor.test(dff$log_heures, dff$depression, method="pearson")
cor.test(dff$log_heures, dff$insomnie, method="pearson")
cor.test(dff$log_heures, dff$toc, method="pearson")

library(corrplot)

num_vars <- dff[, c("heures_ecoute_par_jour",
                    "age",
                    "anxiete",
                    "depression",
                    "insomnie",
                    "toc")]

mat_spear <- cor(num_vars,
                 method = "spearman",
                 use = "complete.obs")

corrplot(mat_spear,
         method = "number",
         type = "upper",
         order = "hclust",
         tl.col = "black",
         tl.cex = 0.9,
         number.cex = 0.75,
         number.font = 2,
         col = colorRampPalette(c("darkblue", "grey", "red"))(200),
         cl.pos = "r",
         cl.cex = 0.8,
         mar = c(0,0,2,0))

title("Corrélations entre variables quantitatives (Spearman)", line = 3)

plot(
  dff$heures_ecoute_par_jour,
  dff$anxiete,
  pch = 19,
  col = rgb(0.2, 0.4, 0.6, 0.5),
  xlab = "Temps d'écoute quotidien (heures)",
  ylab = "Score d'anxiété",
  main = "Relation entre le temps d'écoute et l'anxiété"
)

plot(
  dff$anxiete,
  dff$depression,
  pch = 19,
  col = rgb(0.6, 0.2, 0.2, 0.5),
  xlab = "Score d'anxiété",
  ylab = "Score de dépression",
  main = "Relation entre l'anxiété et la dépression"
)

plot(
  dff$insomnie,
  dff$depression,
  pch = 19,
  col = rgb(0.3, 0.3, 0.3, 0.5),
  xlab = "Score d'insomnie",
  ylab = "Score de dépression",
  main = "Relation entre l'insomnie et la dépression"
)

# 3.3 Quali-quanti

tapply(dff$anxiete, dff$effet_musique, summary)

boxplot(anxiete ~ effet_musique,
        data = dff,
        main = "Anxiété selon l'effet perçu de la musique",
        xlab = "Effet perçu de la musique",
        ylab = "Score d'anxiété",
        col = "lightgrey")

tapply(dff$depression, dff$genre_prefere, summary)

boxplot(depression ~ genre_prefere,
        data = dff,
        las = 2,
        main = "Dépression selon le genre musical préféré",
        xlab = "Genre préféré",
        ylab = "Score de dépression",
        col = "lightgrey")

tapply(dff$heures_ecoute_par_jour, dff$ecoute_en_travaillant, summary)

boxplot(heures_ecoute_par_jour ~ ecoute_en_travaillant,
        data = dff,
        main = "Temps d'écoute selon l'écoute en travaillant",
        xlab = "Écoute de musique en travaillant",
        ylab = "Heures d'écoute par jour",
        col = "lightgrey")

t.test(heures_ecoute_par_jour ~ ecoute_en_travaillant, data = dff)
t.test(anxiete ~ ecoute_en_travaillant, data = dff)

# 4) Analyse multivariée : ACP

# 4.1 Normalité

vars_quant <- c("heures_ecoute_par_jour",
                "age",
                "anxiete",
                "depression",
                "insomnie",
                "toc")

df_quant <- dff[, vars_quant]

shapiro_results <- sapply(df_quant, function(x) {
  shapiro.test(x)$p.value
})

shapiro_results

# 4.2 Déterminant

mat_corr <- cor(df_quant,
                method = "spearman",
                use = "complete.obs")

det_corr <- det(mat_corr)
det_corr

# 4.2.1 KMO et Bartlett

library(psych)
KMO(mat_corr)
cortest.bartlett(mat_corr, n = nrow(df_quant))

# 4.3 ACP globale

library(FactoMineR)
library(factoextra)
library(patchwork)

df_quant <- dff[, sapply(dff, is.numeric)]
ACP <- PCA(df_quant, scale.unit = TRUE, graph = FALSE)

fviz_eig(ACP, addlabels = TRUE, ylim = c(0, 80))

d12_global <- fviz_pca_var(ACP, axes = c(1,2), col.var="contrib", repel=TRUE,
                           title="Cercle (Dim 1–2)")
d34_global <- fviz_pca_var(ACP, axes = c(3,4), col.var="contrib", repel=TRUE,
                           title="Cercle (Dim 3–4)")
d12_global + d34_global

# 4.4 ACP principale : log_heures + santé mentale

dff$log_heures <- log1p(dff$heures_ecoute_par_jour)

df_acp_main <- dff[, c("log_heures","anxiete","depression","insomnie","toc")]
ACP1 <- PCA(df_acp_main, scale.unit = TRUE, graph = FALSE)

fviz_eig(ACP1, addlabels = TRUE, ylim = c(0, 80))

d12_1 <- fviz_pca_var(ACP1, axes = c(1,2),
                      select.var = list(contrib = 5),
                      col.var = "contrib", repel = TRUE,
                      title = "ACP1 - Cercle (Dim 1–2)")

d34_1 <- fviz_pca_var(ACP1, axes = c(3,4),
                      select.var = list(contrib = 10),
                      col.var = "contrib", repel = TRUE,
                      title = "ACP1 - Cercle (Dim 3–4)")

d12_1 + d34_1

fviz_pca_ind(ACP1, axes = c(1,2), label="none", geom="point", alpha.ind=0.6)

fviz_pca_ind(ACP1, habillage=dff$ecoute_en_travaillant, addEllipses=TRUE,
             ellipse.level=0.95, label="none")

fviz_pca_ind(ACP1, habillage=dff$effet_musique, addEllipses=TRUE,
             ellipse.level=0.95, label="none")

fviz_pca_ind(ACP1,
             habillage = dff$genre_prefere,
             addEllipses = FALSE,
             label = "none")

# 4.5 Âge en variable supplémentaire

df_acp_age <- dff[, c("log_heures","anxiete","depression","insomnie","toc","age")]
ACP2 <- PCA(df_acp_age, scale.unit = TRUE, quanti.sup = 6, graph = FALSE)

fviz_pca_var(ACP2, axes = c(1,2),
             repel = TRUE,
             title = "ACP2 - Cercle (Dim 1–2) avec âge en suppl")

# 4.6 Score de santé globale + ACP3

m_anx <- 1
m_dep <- 1
m_ins <- 1
m_toc <- 1
dff$sante_globale <- (m_anx*dff$anxiete +
                        m_dep*dff$depression +
                        m_ins*dff$insomnie +
                        m_toc*dff$toc) / (m_anx + m_dep + m_ins + m_toc)

df_acp_sante <- dff[, c("log_heures", "sante_globale")]
ACP3 <- PCA(df_acp_sante, scale.unit = TRUE, graph = FALSE)

fviz_eig(ACP3, addlabels = TRUE, ylim = c(0, 100))

fviz_pca_var(ACP3, repel = TRUE,
             title = "ACP3 - ACP sur log_heures et santé mentale pondérée")

fviz_pca_ind(ACP3, axes = c(1,2), label = "none",
             geom = "point", alpha.ind = 0.6,
             title = "ACP3 - Individus (log_heures vs santé pondérée)")

# 4.7 Contributions / cos2 + profils atypiques

get_pca_var (ACP1) $contrib
get_pca_var (ACP1) $cos2

head(get_pca_ind(ACP1)$contrib[order(get_pca_ind(ACP1)$contrib[,1], decreasing=TRUE), ], 10)

library(dplyr)
var_contrib <- as.data.frame(get_pca_var(ACP1)$contrib)
var_cos2    <- as.data.frame(get_pca_var(ACP1)$cos2)
top_var_axis <- function(axis = 1, k = 5){
  ax <- paste0("Dim.", axis)
  tibble(
    variable = rownames(var_contrib),
    contrib  = var_contrib[[ax]],
    cos2     = var_cos2[[ax]]
  ) %>%
    arrange(desc(contrib)) %>%
    head(k)
}

top_var_axis(1, 5)
top_var_axis(2, 5)
top_var_axis(3, 5)
top_var_axis(4, 5)

ind_coord <- as.data.frame(get_pca_ind(ACP1)$coord)
ind_coord$id <- as.integer(rownames(ind_coord))

top_dim1_pos <- ind_coord %>% arrange(desc(Dim.1)) %>% head(5)
top_dim1_neg <- ind_coord %>% arrange(Dim.1) %>% head(5)

top_dim1_pos
top_dim1_neg
vars_profil <- c("log_heures","anxiete","depression","insomnie","toc",
                 "ecoute_en_travaillant","effet_musique")

dff[top_dim1_pos$id, vars_profil]
dff[top_dim1_neg$id, vars_profil]

ind_contrib <- as.data.frame(get_pca_ind(ACP1)$contrib)
ind_contrib$id <- as.integer(rownames(ind_contrib))

top_contrib_dim1 <- ind_contrib %>% arrange(desc(Dim.1)) %>% head(5)
top_contrib_dim1

dff[top_contrib_dim1$id, vars_profil]

# 4.8 Régression (test en plus)

modele <- lm(sante_globale ~ log_heures, data = dff)
summary(modele)
