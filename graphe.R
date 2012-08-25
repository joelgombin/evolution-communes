graphe <- function(data, lignes=1:dim(data)[1], choix="evol", totaux=T, dataframe=T) { 
  ## fonction pour représenter le dataset
  ## lignes permet de traiter toutes (défaut) ou seulement certaines lignes (à indiquer par leur numéro)
  ## choix = "evol" permet de représenter les taux de croissance
  ## choix = "statique" permet de représenter les valeurs absolues
  ## l'argument totaux indique si les totaux figurent déjà dans le dataset
  ## l'argument dataframe indique si la fonction doit retourner le dataframe transformé
  if (!(choix %in% c("evol", "statique"))) stop("Le paramètre choix doit prendre la valeur \'evol\' ou \'statique\'")
  if (choix %in% "statique") {
    for (i in c("1968", "1975", "1982", "1990", "1999", "2008")) {
      
      if (totaux %in% T) {
        data[,paste0("total", i)] <- apply(data[,grep(i, names(data))], 1, FUN = "sum")
      }
      data[,paste0(names(data)[setdiff(grep(i, names(data)),grep("^qu", names(data)))],".pc")] <- data[,setdiff(grep(i, names(data)),grep("^qu", names(data)))]/data[,paste0("total",i)]
      ## on transforme les valeurs absolues en pourcentage, en prenant soin de laisser de côté les 1ers et 3èmes quartiles
    } 
  }
  require(ggplot2)
  m <- length(lignes)
  if (choix %in% "statique") {
    periode = as.integer(rep(c("1968", "1975", "1982", "1990", "1999", "2008"), times = 2*6*m))
    periodes = c("1968.pc", "1975.pc", "1982.pc", "1990.pc", "1999.pc", "2008.pc")
    k_int <- 1:6
  }  
  else {
    periode <- rep(c("68-75", "75-82", "82-90", "90-99", "99-08"), times = 2*6*m)    
    periodes <- c("6875", "7582", "8290", "9099","9908")
    k_int <- 1:5
  }
  ## on construit un dataframe dont chaque ligne sera un point de donnée à passer à ggplot
  d <- data.frame(CS = rep(c("CS1", "CS2", "CS3", "CS4", "CS5", "CS6"), each=2 * length(periodes), times=m), actif = rep(c("occupé", "chômeur"), each=length(periodes), times=6*m), periode)
  noms <- names(data)
  CSP <- c("Agriculteurs", "Artisans..commerçants..chefs.d.entreprise", "Cadres.et.professions.intellectuelles.supérieures", "Professions.intermédiaires", "Employés", "Ouvriers")
  actifs <- c("Actifs.ayant.un.emploi", "Chômeurs")
  
  ## grosse boucle permetttant de saisir tous les cas pour mettre les données au bon endroit. On passe en revue chaque combinaison CS + statut d'activité + année + groupe
  for (n in 1:m) {
    for (i in 1:6) {
      for (j in 1:2) {
        for (k in k_int) {  
          d[length(periodes)*6*2*(n-1)+2*length(periodes)*(i-1)+length(periodes)*(j-1)+k,"valeur"] <- data[lignes[n],intersect(intersect(grep(CSP[i],noms),grep(actifs[j],noms)),grep(periodes[k], noms))] # la moyenne 
          d[length(periodes)*6*2*(n-1)+2*length(periodes)*(i-1)+length(periodes)*(j-1)+k,"qu1"] <- data[lignes[n],intersect(intersect(intersect(grep(CSP[i],noms),grep(actifs[j],noms)),grep(substr(periodes[k], 1,4),noms)),grep("qu1", noms))] # le 1er quartile
          d[length(periodes)*6*2*(n-1)+2*length(periodes)*(i-1)+length(periodes)*(j-1)+k,"qu3"] <- data[lignes[n],intersect(intersect(intersect(grep(CSP[i],noms),grep(actifs[j],noms)),grep(substr(periodes[k],1,4), noms)),grep("qu3", noms))] # le 3e quartile
          d[length(periodes)*6*2*(n-1)+2*length(periodes)*(i-1)+length(periodes)*(j-1)+k,"groupe"] <- paste0(n, " (N = ", data[data[,1] %in% n,"effectif"], ")") # le groupe
        }
      }
    }
  }
  
  d$groupe <- factor(d$groupe, levels=levels(as.factor(d$groupe))[c(2:10,1)]) # pour avoir les groupes dans le bon ordre sur le graphique
  d$var <- paste(d$CS, d$actif, sep="") # ce qu'on représentera ce sera des combinaisons de la CS et du statut d'activité
  library(scales)
  print(
    ggplot(d, aes(x=periode, group=var)) + geom_line(aes(y=valeur, linetype=actif, size="moyenne", alpha="moyenne")) + geom_line(aes(y=qu1, linetype=actif, size="1er quartile", alpha="1er quartile")) + geom_line(aes(y=qu3, linetype=actif, size="3e quartile", alpha="3e quartile")) + scale_size_manual("", breaks=c("1er quartile", "moyenne", "3e quartile"), values=c("1er quartile" = 0.5, "moyenne" = 1, "3e quartile" = 0.5)) + scale_alpha_manual("", values=c("1er quartile" = 0.5, "moyenne" = 1, "3e quartile" = 0.5), breaks=c("1er quartile", "moyenne", "3e quartile")) + scale_linetype_manual("", values=c("chômeur" = 2, "occupé" = 1)) + facet_grid(CS ~ groupe) + scale_y_continuous(labels = percent) + scale_x_date(breaks= c(1968, 1975, 1982, 1990, 1999, 2008), labels = as.character(c(1968, 1975, 1982, 1990, 1999, 2008))) + opts(axis.text.x = theme_text(angle = 45, hjust = 1, vjust = 1), legend.position = "bottom") + xlab("Année") + ylab("Proportion") + guides(size=guide_legend(""), alpha=guide_legend(""), linetype=guide_legend(""))
  ) 
  if (dataframe)  return(d)
}
