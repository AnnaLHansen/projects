klargoer_salg <- function(df) {
  df <- df[, !grepl("^NN", colnames(df))]
  df <- df[df$ejendomsgruppe %in% c("pcl_byzone", "pcl_landzone"),]
  df <- df[, !grepl("ejendomsgruppe", colnames(df))]
  
  df$aux.ice_info.adresse.afstand_kyst <- ifelse(is.na(df$aux.ice_info.adresse.afstand_kyst), 
                                                 1501,
                                                 df$aux.ice_info.adresse.afstand_kyst)

  var <-
    c(
      "aux.ice_info.adresse.afstand_trafikvej_fordeling",
      "aux.ice_info.adresse.afstand_lokalvej",
      "aux.ice_info.adresse.afstand_trafikvej_gennemfart",
      "aux.ice_info.adresse.afstand_motorvej_motortrafikvej",
      "aux.ice_info.adresse.afstand_station_metro",
      "aux.ice_info.adresse.afstand_station_s_tog",
      "aux.ice_info.adresse.afstand_station_tog"

    )
  for (i in var) {
    df[[i]] <- ifelse(is.na(df[[i]]), 2001, df[[i]])
  }

  var <- c(
    "aux.ice_info.adresse.afstand_jernbane_any",
    "aux.ice_info.adresse.afstand_jernbane_s_bane",
    "aux.ice_info.adresse.afstand_jernbane_hovedbane",
    "aux.ice_info.adresse.afstand_jernbane_lokalbane",
    "aux.ice_info.adresse.afstand_jernbane_metro",
    "aux.ice_info.adresse.afstand_jernbane_regionalbane",
    "aux.ice_info.adresse.afstand_jernbane_region_privat",
    "aux.ice_info.adresse.afstand_stor_skov",
    "aux.ice_info.adresse.afstand_lille_skov",
    "aux.ice_info.adresse.afstand_lokalnet_hoejspaending",
    "aux.ice_info.adresse.afstand_regionalnet_hoejspaending",
    "aux.ice_info.adresse.afstand_transmissionsnet_hoejspaending",
    "aux.ice_info.adresse.afstand_stort_vandloeb",
    "aux.ice_info.adresse.afstand_lille_vandloeb",
    "aux.ice_info.adresse.afstand_mellem_vandloeb",
    "aux.ice_info.adresse.afstand_ukendt_vandloeb"
  )
  for (i in var) {
    df[[i]] <- ifelse(is.na(df[[i]]), 1001, df[[i]])
  }

  var <- c("aux.ice_info.adresse.afstand_stor_soe")
  for (i in var) {
    df[[i]] <- ifelse(is.na(df[[i]]), 501, df[[i]])
  }

  var <- c(
    "aux.ice_info.adresse.afstand_lille_vindmoelle",
    "aux.ice_info.adresse.afstand_mellem_vindmoelle",
    "aux.ice_info.adresse.afstand_stor_vindmoelle"

  )
  for (i in var) {
    df[[i]] <- ifelse(is.na(df[[i]]), 3001, df[[i]])
  }
  
  return(df)
}


undersoeger_attributter <- function(df){
  a <- data.frame("attributes" = colnames(df),
                  "descrete_continous" = "",
                  "attribute_type" = "",
                  "attribute_class" = "")
  a[["missing_values"]] <- sapply(1:nrow(a), function(x) sum(is.na(df[, x])))
  a[["pct_missing_values"]] <- sapply(1:nrow(a), function(x) (a[["missing_values"]][x] / nrow(train)) *100
  )
  
  a[["descrete_continous"]] <-
    ifelse(
      grepl("^aux.ice_info.adresse.afstand", a$attributes) |
        grepl("^aux.ice_info.adresse.areal", a$attributes) |
        grepl("^aux.ice_info.adresse.udsigt", a$attributes) |
        grepl("^aux.adresse.etrs", a$attributes) |
        grepl("^aux.ice_info.bygning", a$attributes) |
        grepl("^aux.ice_info.*opsummeret_delj", a$attributes) |
        grepl("^aux.ice_info.jordstykker.", a$attributes) |
        grepl("^bolig", a$attributes) |
        grepl("^bygning.areal", a$attributes) |
        grepl("bygning.*areal$", a$attributes) |
        grepl("^aux.adresse.etrs", a$attributes) |
        grepl("^bygning.*aar$", a$attributes) |
        grepl("^enhed.antal*", a$attributes) |
        grepl("^enhed.*areal", a$attributes) |
        grepl("^etage.*areal$", a$attributes) |
        grepl("^etage.areal", a$attributes) |
        grepl("^aux.ice_info.bb_anvendelse*", a$attributes) |
        grepl("^ice_info.*delgrund$", a$attributes) |
        grepl("enhed.*varme*", a$attributes) |
        grepl("bygning.*varme*", a$attributes) |
        grepl("forhold$", a$attributes) |
        grepl("bygning.antaletager", a$attributes) |
        grepl("ombyg_alder", a$attributes) |
        grepl("aux.adresse.regionskode", a$attributes) |
        grepl("aux.adresse.etage", a$attributes) |
        grepl("bygning.bygningensanvendelse", a$attributes) |
        grepl("bygning.tagdaekningsmateriale", a$attributes) |
        grepl("bygning.vandforsyning", a$attributes) |
        grepl("bygning.ydervaeggensmateriale", a$attributes) |
        grepl("ejendomsgruppe", a$attributes) |
        grepl("enhed.energiforsyning", a$attributes) |
        grepl("enhed.enhedensanvendelse", a$attributes) |
        grepl("etage.bygningensetagebetegnelse", a$attributes) |
        grepl("region_nr", a$attributes) |
        grepl("aux.vurbenyttelseskode", a$attributes) |
        grepl("^EV", a$attributes),
      "descrete",
      ifelse(
        grepl("ice_info.min_koteletratiobuff250", a$attributes) |
          grepl("fremskreven_pris", a$attributes) |
          grepl("fremskreven_pris_M2", a$attributes),
        "continous",
        
        ""
      )
    )
  
  
  
  a[["attribute_type"]] <-
    ifelse(
      grepl("^aux.ice_info.adresse.afstand", a$attributes) |
        grepl("^aux.ice_info.adresse.areal", a$attributes) |
        grepl("^aux.ice_info.adresse.udsigt", a$attributes) |
        grepl("^aux.ice_info.bygning", a$attributes) |
        grepl("^aux.ice_info.*opsummeret_delj", a$attributes) |
        grepl("^aux.ice_info.jordstykker.", a$attributes) |
        grepl("^bolig", a$attributes) |
        grepl("^bygning.areal", a$attributes) |
        grepl("bygning.*areal$", a$attributes) |
        grepl("^enhed.antal*", a$attributes) |
        grepl("^enhed.*areal", a$attributes) |
        grepl("^etage.*areal$", a$attributes) |
        grepl("^etage.areal", a$attributes) |
        grepl("^aux.ice_info.bb_anvendelse*", a$attributes) |
        grepl("^ice_info.*delgrund$", a$attributes),
      "ratio",
      ifelse(
        grepl("^aux.adresse.etrs", a$attributes) |
          grepl("^bygning.*aar$", a$attributes) |
          grepl("ice_info.min_koteletratiobuff250", a$attributes) |
          grepl("bygning.antaletager", a$attributes) |
          grepl("ombyg_alder", a$attributes)|
          grepl("fremskreven_pris", a$attributes) |
          grepl("fremskreven_pris_M2", a$attributes) |
          grepl("^EV", a$attributes),
        "interval",
        ifelse(
          grepl("enhed.*varme*", a$attributes) |
            grepl("bygning.*varme*", a$attributes) |
            grepl("forhold$", a$attributes) |
            grepl("aux.adresse.regionskode", a$attributes) |
            grepl("aux.adresse.etage", a$attributes) |
            grepl("bygning.bygningensanvendelse", a$attributes) |
            grepl("bygning.tagdaekningsmateriale", a$attributes) |
            grepl("bygning.vandforsyning", a$attributes) |
            grepl("bygning.ydervaeggensmateriale", a$attributes) |
            grepl("ejendomsgruppe", a$attributes) |
            grepl("enhed.energiforsyning", a$attributes) |
            grepl("enhed.enhedensanvendelse", a$attributes) |
            grepl("etage.bygningensetagebetegnelse", a$attributes) |
            grepl("region_nr", a$attributes) |
            grepl("aux.vurbenyttelseskode", a$attributes),
          "nomial",
          ""
        )
      )
    )
  
  attr <- as.character(a$attributes)
  a$attribute_class <- sapply(attr, function(x) class(df[[x]]))
  
  return(a)
}


samler_variable <- function(df){
  a <- colnames(df)[grepl("station", colnames(df))]
  
  df[["aux.ice_info.adresse.afstand_station_any"]] <- sapply(1:nrow(df), function(x){
    pmin(df[x,a[[1]]],
        df[x,a[[2]]],
        df[x,a[[3]]], na.rm = TRUE)
  })

  a <- colnames(df)[grepl("^aux.ice_info.adresse.afstand.*soe$", colnames(df))]
  df[["aux.ice_info.adresse.afstand_soe_any"]] <- sapply(1:nrow(df), function(x){
    pmin(df[x,a[[1]]],
        df[x,a[[2]]],
        df[x,a[[3]]], na.rm = TRUE)
  })
  
  a <- colnames(df)[grepl("^aux.ice_info.adresse.afstand.*vandloeb$", colnames(df))]
  df[["aux.ice_info.adresse.afstand_vandloeb_any"]] <- sapply(1:nrow(df), function(x){
    pmin(df[x,a[[1]]],
        df[x,a[[2]]],
        df[x,a[[3]]], na.rm = TRUE)
  })
  
  a <- colnames(df)[grepl("^aux.ice_info.adresse.afstand.*hoejspaending$", colnames(df))]
  df[["aux.ice_info.adresse.afstand_hoejspaending_any"]] <- sapply(1:nrow(df), function(x){
    pmin(df[x,a[[1]]],
        df[x,a[[2]]],
        df[x,a[[3]]], na.rm = TRUE)
  })
  
  return(df)
}


missing_values_pct95 <- function(attribut, df){
  attributter_som_fjernes <- as.character(attribut[attribut$pct_missing_values > 95, "attributes"])
  df <- df[, !colnames(df) %in% attributter_som_fjernes]
  return(df)
}


trimmer_og_uniformiserer_data <- function(df){
  df <- subset(df, enhed.antalvaerelser > 1 & enhed.antalvaerelser < 10)
  df <- subset(df, bolig_areal < 500 & bolig_areal > 50)
  df <- subset(df, bolig_alder > 0 & bolig_alder < 100)
  df <- subset(df, bygning.antaletager > 0 & bygning.antaletager < 4)
  df <- subset(df, enhed.antalbadevaerelser > 0 & enhed.antalbadevaerelser < 4)
  df <- subset(df, enhed.antalvandskylledetoiletter > 0 & enhed.antalvandskylledetoiletter < 4)
  df <- subset(df, aux.vurbenyttelseskode == "01")
  df <- df[, c(#"aux.ice_info.adresse.afstand_hoejspaending_any",
                # "aux.ice_info.adresse.afstand_vandloeb_any",
                # "aux.ice_info.adresse.afstand_soe_any",
                # "aux.ice_info.adresse.afstand_station_any",
                "fremskreven_pris_M2",
                # "fremskreven_pris", 
                "bygning.ydervaeggensmateriale",
                "bygning.tagdaekningsmateriale",
                #"bygning.varmeinstallation",
                #"bygning.opvarmningsmiddel",
                "ombyg_alder",
                "enhed.antalvaerelser",
                "enhed.antalbadevaerelser",
                "enhed.antalvandskylledetoiletter",
                "bolig_areal",
                "bolig_alder",
                "aux.ice_info.jordstykker.registreretareal_fratrukket_vejareal",
                #"aux.ice_info.adresse.afstand_jernbane_any",
                "aux.ice_info.adresse.afstand_motorvej_motortrafikvej",
                #"aux.ice_info.adresse.afstand_trafikvej_gennemfart",
                "aux.ice_info.adresse.afstand_kyst",
                "EV_NN_M2"
                # "aux.adresse.etrs89koordinatoest",
                # "aux.adresse.etrs89koordinatnord"
                )]
  
  df
  
  }

