one_out_of_k <- function(train){
  train[["ydervaegsmateriale_mursten"]] <- ifelse(train$bygning.ydervaeggensmateriale == 1,
                                                  1,0) 
  train[["ydervaegsmateriale_gasbeton"]] <- ifelse(train$bygning.ydervaeggensmateriale == 2,
                                                   1,0) 
  train[["ydervaegsmateriale_bindingsvaerk"]] <- ifelse(train$bygning.ydervaeggensmateriale == 4,
                                                        1,0) 
  train[["ydervaegsmateriale_traebeklaedning"]] <- ifelse(train$bygning.ydervaeggensmateriale == 5,
                                                          1,0) 
  train[["ydervaegsmateriale_andet"]] <- ifelse(train$bygning.ydervaeggensmateriale %in% 
                                                  c(3, 6, 8, 10, 11, 12, 80, 90),
                                                1, 0)
  train$bygning.ydervaeggensmateriale <- NULL

  train[["tagtype_builtup"]] <- ifelse(train$bygning.tagdaekningsmateriale == 1,1, 0)
  train[["tagtype_tagpap"]] <- ifelse(train$bygning.tagdaekningsmateriale == 2, 1,0)
  train[["tagtype_fibercement"]] <- ifelse(train$bygning.tagdaekningsmateriale %in% c(3, 10), 1, 0)
  train[["tagtype_cementsten"]] <- ifelse(train$bygning.tagdaekningsmateriale == 4, 1, 0)
  train[["tagtype_tegl"]] <- ifelse(train$bygning.tagdaekningsmateriale == 5, 1,0)
  train[["tagtype_straatag"]] <- ifelse(train$bygning.tagdaekningsmateriale ==  7, 1, 0)
  train[["tagtype_andet"]] <- ifelse(train$bygning.tagdaekningsmateriale %in% 
                                       c(11, 12, 20, 6,80, 90), 1, 0)
  train$bygning.tagdaekningsmateriale <- NULL
  # train[["opvarmning_el"]] <- ifelse(train$bygning.opvarmningsmiddel == 1, 1, 0)
  # train[["opvarmning_olie"]] <- ifelse(train$bygning.opvarmningsmiddel == 3, 1, 0)
  # train[["opvarmning_naturgas"]] <- ifelse(train$bygning.opvarmningsmiddel == 7, 1,0)
  # train[["opvarmning_andet"]] <- ifelse(train$bygning.opvarmningsmiddel %in% c(2,4,6,9), 1, 0)
  train$bygning.opvarmningsmiddel <- NULL
  
  return(train)
}

binarisere_afstande <- function(train){
  train[["taet_paa_kyst"]] <- ifelse(train$aux.ice_info.adresse.afstand_kyst < 300, 1, 0)
  train[["taet_paa_motorvej"]] <- ifelse(train$aux.ice_info.adresse.afstand_motorvej_motortrafikvej < 100,
                                         1, 0)
  train$aux.ice_info.adresse.afstand_kyst <- NULL
  train$aux.ice_info.adresse.afstand_motorvej_motortrafikvej <- NULL
  return(train)
}

standardize <- function(X)
{
  # Standardize the matrix X, that is, subtract mean and divide by standard deviation to yield matrix of zero mean and variance one.
  #
  # Author: Laura Frølich, lff@imm.dtu.dk
  Xmean <- colMeans(X, na.rm = TRUE)
  Xnomean <- X-matrix(rep(Xmean, times=nrow(X)), byrow=TRUE, nrow=nrow(X))
  
  Xsd <- apply(Xnomean,2,sd)
  Xstandardized <- Xnomean/matrix(rep(Xsd, times=nrow(X)), byrow=TRUE, nrow=nrow(X))
  Xstandardized
}

