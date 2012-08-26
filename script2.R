## Initialisation
setwd("/media/Data/Dropbox/Thèse/évolution")  # à modifier en fonction de la config locale...
data <- read.csv("data.csv", colClasses = c("factor", "character", "character", rep("integer",72)))

change <- read.csv("changements.csv", colClasses = c("character", "character", "factor", "character", "character", "character")) # un tableau récapitulant toutes les modifications survenues au découpage en communes depuis 1968
change[,4] <- as.numeric(substr(change[,4],nchar(change[,4])-3,nchar(change[,4])))

source("consolid.R")   #attention, un peu long
  
resultat <- consolid(data, change)
resultat[,4:75] <- apply(resultat[,4:75], 2, function(x) {x[which(is.na(x))] <- 0 ; x})

save(resultat, file="données consolidées.Rdata")

load("/media/Data/Dropbox/Thèse/données propres/identification/communes_ident.Rdata") # à configurer localement...
data <- merge(communes_ident, resultat[,-2], by.x="CodeInsee", by.y="Code.Insee")

rct <- c(2008,1999,1990,1982,1975,1968)
variables <- names(data)

for (i in 1:5) {
  data[,paste(unlist(strsplit(names(data)[grep(as.character(rct[i]),variables)], paste(".RP",as.character(rct[i]), sep=""))), substr(rct[i+1],3,4), substr(rct[i],3,4), sep="")] <- (data[,grep(as.character(rct[1]),variables)]/data[,grep(as.character(rct[i+1]),variables)]-1)*100 # calcule les taux d'évolution
  data[,paste(unlist(strsplit(names(data)[grep(as.character(rct[i]),variables)], paste(".RP",as.character(rct[i]), sep=""))), substr(rct[i+1],3,4), substr(rct[i],3,4), sep="")] <- apply(data[,paste(unlist(strsplit(names(data)[grep(as.character(rct[i]),variables)], paste(".RP",as.character(rct[i]), sep=""))), substr(rct[i+1],3,4), substr(rct[i],3,4), sep="")], 2, function(x) {x[which(x %in% Inf)] <- 100 ; x}) # transforme les Inf en 100 % -- arbitraire !
  data[,paste(unlist(strsplit(names(data)[grep(as.character(rct[i]),variables)], paste(".RP",as.character(rct[i]), sep=""))), substr(rct[i+1],3,4), substr(rct[i],3,4), sep="")] <- apply(data[,paste(unlist(strsplit(names(data)[grep(as.character(rct[i]),variables)], paste(".RP",as.character(rct[i]), sep=""))), substr(rct[i+1],3,4), substr(rct[i],3,4), sep="")], 2, function(x) {x[which(x %in% NaN)] <- 0 ; x}) # transforme les NaN (0/0) en 0 %
}

row.names(data) <- data$CodeInsee
save(data, file="data.Rdata")

groupes_depart <- kmeans(data[,113:172], centers=1000, iter.max=50) # on simplifie le dataset de départ pour pouvoir faire une ACP plus aisément dessus. Problème : pas déterministe donc pas réplicable...
groupes <- data.frame(cbind(groupes=1:1000, groupes_depart$centers))


# attention, c'est *long* (près de deux heures sur une instance AWS...)
groupes_def2 <- pam(data[,113:172], k=10, )
save(groupes_def2, file="groupes_def_pam.Rdata")
# attention, ce n'est pas un algorithme déterministe, donc vous n'obtiendrez pas les mêmes groupes que moi. Beaucoup plus robuste qu'un K-means classique, ceci dit.

groupes2 <- cbind(groupes=1001:1010,as.data.frame(groupes_def2$medoids)) 
names(groupes)[1] <- "groupes"
data_acp <- rbind(groupes, groupes2) 
row.names(data_acp) <- data_acp$groupes
data_acp <- data_acp[,-1] # on a construit un dataset pour mener l'ACP avec les 1000 groupes homogènes et les 10 groupes issus de la PAM (en individus supplémentaires)

library(FactoMineR)
acp <- PCA(data_acp, ind.sup=1001:1010, graph=F)
plot.PCA(acp, axes=1:2, choix="ind", invisible="ind")
plot.PCA(acp, axes=c(1,3), choix="ind", invisible="ind")

int <- as.data.frame(acp$var$coord[acp$var$cos2[,1] > 0.2,])
int[order(int$Dim.1),] # pour faire des tableaux des variables les plus associées avec la 1e composante principale

int <- as.data.frame(acp$var$coord[acp$var$cos2[,2] > 0.2,])
int[order(int$Dim.2),]

int <- as.data.frame(acp$var$coord[acp$var$cos2[,3] > 0.2,])
int[order(int$Dim.3),]


source("graphe.R")

data$cluster_med <- as.factor(groupes_def2$clustering)

## on prépare maintenant le dataset à fournir à la fonction graphe()
groupes3 <- aggregate(data[,41:112], by=list(data$cluster_med), FUN=sum)
groupes3$effectif <- aggregate(data[,1], by=list(data$cluster_med), FUN= length)[,2]

for (i in c(1968, 1975, 1982, 1990, 1999, 2008)) {
  groupes3[,paste0("total", i)] <- aggregate(apply(data[,grep(i, names(data))], MARGIN=1, FUN=sum), by=list(data$cluster_med), FUN=sum)[2] # on calcule l'effectif total par groupe par année
  groupes3[,paste0("qu1", names(data)[grep(i, names(data))])] <- aggregate(data[,grep(i, names(data))]/apply(data[,grep(i, names(data))], MARGIN=1, FUN=sum), by=list(data$cluster_med),FUN=quantile, 0.25, na.rm=T)[-1] # le 1er quartile 
  groupes3[,paste0("qu3", names(data)[grep(i, names(data))])] <- aggregate(data[,grep(i, names(data))]/apply(data[,grep(i, names(data))], MARGIN=1, FUN=sum), by=list(data$cluster_med),FUN=quantile, 0.75, na.rm=T)[-1] # le 3e quartile
}

pdf(file="graphique_evolution.pdf", title = "L'évolution socio-démographique des communes françaises depuis 1968", width=20, height=12)
g2 <- graphe(groupes3, choix="statique")
dev.off()

