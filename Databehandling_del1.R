setwd("C:/Users/alhan/projects")
#setwd("~/projects")
source("help_func_datainspec.R")
source("help_func_standardization.R")

train <- readRDS("anonym_data_kursus.rds")
#train <- readRDS("~/1_A_Machine_learning_kursus_data/anonym_data_kursus.rds")
train <- imputerer_og_samler_geodata(train) # tilbage imputerer og fjerner irrelevante attributter
# Laver et subset af salgene så det kun er de almindelige parcelhuse som optræder i data
# Herefter bliver der lavet en oversigt over de variable som findes i data 
# samt hvor mange missing values der er.
tabel_attribut_typer <- undersoeger_attributter(train) # Laver oversigt over de attributter som er tilgængelige
# Missing values i data skyldes flere forskellige ting
# Der er mange missing værdier som fordi felterne er valgfrie felter når de udfyldes i bbr. 
# For afstandsvariablene er der missing værdier afstandene ud over en bestemt trunkeringsafstand ikke
# er blevet målt.
tabel_manglende_vaerdier <- missing_table(df = train)
train <- missing_values_pct90(tabel_manglende_vaerdier, train)
tabel_manglende_vaerdier <- missing_table(df = train)

train <- trimmer_og_uniformiserer_data(train)

# One-out-of-k encoding:
# Find variabel og lave 1-out-of-k encoding. 
# hvor en faktor variabel laves om til mange variable med 1/0.
# Normalization:rescales your data into a range of [0;1]
# One-out-of-k er at normalisere data:
train <- one_out_of_k(train)
train <- binarisere_afstande(train)
# Plot af den fremskrevne handelspris mod de fremskrevne salgspriser for 
# de naermeste 15 naboer.
# plot(train$fremskreven_pris_M2, train$EV_NN_M2)

####################
# exercise 1 & 2
####################
# Ud over at PCA'en kan hjælpe med at udvælge de variable som 
# forklarer det meste variationen i data, vil det
# også være rigtig godt hvis man ud fra sin forretningsviden kan reducere
# antallet af attributter. 
train <- readRDS("train.rds")
train <- train %>% dplyr::select(-fremskreven_pris_M2)
# Standardization: rescales your data to have a mean of 0 and a standard deviation of 1
# Standardiseringen bliver lavet inden 
 # Bruger deres funktion til at standardisere
train <- train[train$ejendomsgruppe %in% c("pcl_byzone",
                                             "pcl_landzone",
                                             "pcl_uzone",
                                             "pcl_somzone",
                                             "rkh_byzone",
                                             "rkh_somzone",
                                             "rkh_uzone",
                                             "rkh_landzone"),]
train <- standardize(train)

# Principal component analysen skal foretages uden variablen 
# med den fremskrevne handelspris. 
train_pca <- prcomp(train)
# Plotter den første og den anden principal component mod hinanden.
# PC1 på x aksen da det er den principal component som forklarer mest af
# variationen i data. 
plot(train_pca$x[,1], train_pca$x[,2])

pca_data <- data.frame(Sample = rownames(train_pca$x),
                       X= train_pca$x[, 1],
                       Y = train_pca$x[, 2])

pca_var <- train_pca$sdev^2
pca_var_round <- round(pca_var/sum(pca_var)*100, 1)

library(ggplot2)
ggplot(data = pca_data,
       aes(x=X, y = Y)) +
  geom_point() +
  xlab(paste("PC1 - ", pca_var_round[[1]], "%", sep ="")) +
  ylab(paste("PC2 - ", pca_var_round[[2]], "%", sep ="")) +
  theme_bw() +
  ggtitle("Principal Component Analysis")

# Scree plot? 
plot(pca_var_round,
     type='o',
     main="Varians forklaret med principal components",
     xlab="Principal components", 
     ylab="Forklaret varians",
     col='deepskyblue')
par(new = TRUE)
plot(cumsum(pca_var_round),
     type='o',
     col='darkorchid',
     axes = FALSE, 
     ylab = "", 
     xlab = "")
legend("topleft",
       c("varians","kommulativ varians"),
       cex=.8,
       bty = 'n',
       fill=c("deepskyblue","darkorchid")
)
axis(side=4)
mtext("Kummulativ varians", side=4, line=3)

# 
# barplot(pca_var_round,
#         main = "Varians blandt Principal Components",
#         xlab = "Principal Components",
#         ylab = "Procent variation")

loading_scores <- train_pca$rotation[, 1]
loading_scores_abs <- abs(loading_scores)
loading_scores_abs <- sort(loading_scores_abs, decreasing = TRUE)
# Top 10 som har en inflydelse på variationen 
# i den første princip component
loading_scores_abs[1:10]
names(loading_scores_abs[1:10])

summary(train_pca)
str(train_pca)

par(mar = c(5, 4, 4, 4) + 0.3)
barplot(cumsum(pca_var_round),
        col = 'darkorchid',
        axes = FALSE, 
        ylab = "", 
        xlab = "")
axis(side=4)
mtext("Kummulativ varians", side=4, line=3)
par(new = TRUE)
barplot(pca_var_round,
        main = "Varians blandt Principal Components",
        xlab = "Principal Components",
        ylab = "Procent variation",
        col='deepskyblue')

legend("topleft",
       c("varians","kommulativ varians"),
       cex=.8,
       bty = 'n',
       fill=c("deepskyblue","darkorchid")
)
# De foeste tre principal components kan man forklare 90% af variationen i data.
# For at kommme over 95% skal man have de 18 foerste komponenter. 
# Der er i alt 23 mulige komponenter.
# Det som det umiddelbart synes at betyde er at der ikke kan reduceres mange attibutter vaek
# Uden at det betyder at man mister variation.
# PCA analysen kan bruges til at visualiserer data, feature extraction og compression. 
# Principal component analyse kan bruges til at reducerer dementionerne i data. 
# PCA bliver udregnet ved bl.a. at udregne 'singular value decomposition'. Ud fra PCA resultaterne kan man 
# finde ud af hvor meget af variationen i data der kan forklares ud fra hver enkelt PCA komponent. 
# Prøv at se om det er muligt at lave en enkelt variabel ud fra de to koordinater - Eller - kan de indgå 
# som to selvstændige variable?

library(ggfortify)
autoplot(train_pca, loadings = TRUE)

####################
# exercise 3
####################
# Measures of similarity, summary statistics and
# probabilities with R
mean_values <- sapply(colnames(train), function(x){
  mean(train[[x]])
})

sd_values <- sapply(colnames(train), function(x){
  sd(train[[x]])
}) # Er alle sammen 1 - fordi der er blevet standardiseret! 
# Hvis det skal give mening er det måske en god ide at gøre inden
# standardisering.

meadian_values <- sapply(colnames(train), function(x){
  median(train[[x]])
})

range_values <- sapply(colnames(train), function(x){
  diff(range(train[[x]]))
}) # range returns the minimum and maximum of the vector x. 
#To get the range, we must take the maximum minus the minimum.
#We do this using the function diff, which finds differences between 
#consecutive elements in a vector.

library(corrplot)
correlation <- cor(train)
print(colnames(correlation))
rownames(correlation) <- 1:nrow(correlation)
colnames(correlation) <- 1:nrow(correlation)
corrplot(correlation)
## Similarity measures mangler: og Et ordenligt correlation plot mangler
# også !
