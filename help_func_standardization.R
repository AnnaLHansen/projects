standardize <- function(df){

var <- sapply(1:ncol(df), function(x) class(df[[x]]))
var_num_integer <- df[var %in% c("numeric", "integer")]
myVar <- colnames(var_num_integer)

#scale(df$aux.ice_info.adresse.afstand_kyst)
for( i in myVar){
  df[[i]] <- (df[[i]] - mean(df[[i]])) / sd(df[[i]]) 
}
    
}



