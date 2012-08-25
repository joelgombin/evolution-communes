consolid <- function(df, table) {
  # prend en entrée les données brutes et retourne les données consolidées
  result <- df
  rct <- c(2008,1999,1990,1982,1975,1968)
  variables <- names(df)
  # on part de la table. On identifie les lignes à traiter ensemble - même commune concernée, même modif, même date
  
  index <- unique(table[,1:4])
  
  for (i in 1:dim(index)[1]) {
    print(i)
    indices <- which(index[i,1] == table[,1] & index[i,2] == table[,2] & index[i,3] == table[,3] & index[i,4] == table[,4])
    l <- length(indices)
    if (index[i,"Nature.de.la.modification"] %in% "Changement de département") {
      for (j in rct) {
        if (table[indices[1],"date.d.effet"] < j) {
          # browser()
          result[which(index[i,1] == result[,3]),grep(as.character(j),variables)] <- colSums(df[sapply(indices, function(x) ifelse(any(which(table[x,"Ancien.code.de.la.commune"] == df$Code.Insee)), which(table[x,"Ancien.code.de.la.commune"] == df$Code.Insee),0)), grep(as.character(j),variables)])
        }
        else break
      }
    }
    else {
      if (index[i,"Nature.de.la.modification"] %in% "Commune se séparant") {
        for (j in rev(rct)) {
          if (table[indices[1],"date.d.effet"] > j) {
            #  browser()
            result[which(index[i,1] == result[,2]),grep(as.character(j),variables)] <- round(df[sapply(indices, function(x) which(table[x,"Code.commune.associée"] == df$Code.Insee)),grep(as.character(j),variables)]/(l+1))
          }
          else break
        }
      }
      else {
        if (index[i,"Nature.de.la.modification"] %in% "Création") {
          for (j in rev(rct)) {
            if (table[indices[1],"date.d.effet"] > j) {
              #   browser()
              result[which(index[i,1] == result[,2]),grep(as.character(j),variables)] <- round(colMeans(df[sapply(indices, function(x) which(table[x,"Code.commune.associée"] == df$Code.Insee)),grep(as.character(j),variables)]))
            }
            else break
          }
        }
        else {
          if (index[i,"Nature.de.la.modification"] %in% "Fusion (commune absorbante)") {
            for (j in rev(rct)) {
              if (table[indices[1],"date.d.effet"] > j) {
                #    browser()
                
                result[which(index[i,1] == result[,2]),grep(as.character(j),variables)] <- colSums(df[sapply(indices, function(x) ifelse(any(which(table[x,"Code.commune.associée"] == df$Code.Insee)),which(table[x,"Code.commune.associée"] == df$Code.Insee),0)),grep(as.character(j),variables)])
              }
              else break
            }
          }
          else {
            if (index[i,"Nature.de.la.modification"] %in% "Fusion (commune absorbée)") {
              for (j in rct) {
                if (table[indices[1],"date.d.effet"] < j) {
                  #  browser()
                  result[which(index[i,1] == result[,2]),grep(as.character(j),variables)] <- round(colMeans(df[sapply(indices, function(x) which(table[x,"Code.commune.associée"] == df$Code.Insee)),grep(as.character(j),variables)]))
                }
                else break
              }
            }
            else {
              if (index[i,"Nature.de.la.modification"] %in% "Rétablissement") {
                for (j in rev(rct)) {
                  if (table[indices[1],"date.d.effet"] > j) {
                    # browser()
                    result[which(index[i,1] == result[,2]),grep(as.character(j),variables)] <- colSums(df[sapply(indices, function(x) which(table[x,"Code.commune.associée"] == df$Code.Insee)),grep(as.character(j),variables)])/(l+1)
                  }
                  else break
                }
              }
              else { # cas de transfert de chef-lieu de commune
                # traiter à la main dans les données
              }
            }
          }
        }
      }
    }
  }
  return(result)
}
