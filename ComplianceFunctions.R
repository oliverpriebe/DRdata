# dateIndex =  9 UIDindex = 1 for DR_ADV_wEyeID

#INPUTS: df: data frame of interest. Should be distinct advised laser treatment enteries. Must have UID, eyepartID and entereddatetime
#             columns
#       dateIndex: Coumn index for "entereddatetime" in df
#       UID_Treatment: Pass in UID_Treatment from global env (should be a way to call from env automatically)

#OUTPUT: vector object with number of treatments completed within 45 days of the df entereddatetime entry.
SimpleTreats <- function(df, dateIndex, UID_Treatment) {
    
    UIDIndex <- grep("UID", colnames(df))
    eyeIndex <- grep("eyepartid", colnames(df))
    out <- vector(mode = "numeric", length(df))
    
    for (i in seq_along(df$UID)) {
      AdvisedDay <- df[[i, dateIndex]]
      timeDif <- difftime(UID_Treatment$entereddatetime, AdvisedDay, units = "days")
      corUID <- filter(UID_Treatment, UID == df[[i, UIDIndex]], eyepartid == df[[i, eyeIndex]],
                       timeDif >= 0, timeDif <= 45)
      out[[i]] <- nrow(corUID)
    }
    print(out)
}


#INPUTS: df: data frame of interest. Should be distinct advised laser treatment enteries. Must have UID, eyepartID and entereddatetime
#             columns
#       dateIndex: Coumn index for "entereddatetime" in df
#       UID_Treatment: Pass in UID_Treatment from global env (should be a way to call from env automatically)

#OUTPUT: vector object with number of treatments reccommended for each df entry. Basically just takes noofsittings, but also
#        removes nulls and replaces with 1s.
RecSittings_withEyeID <- function(df, dateIndex, UID_Treatment) {
  #get indicies
  noSittingsIndex <- grep("noofsitting", colnames(df))
  
  out <- vector(mode = "numeric", length(df))
  for (i in seq_along(df$UID)) {

    
    #multiply factor by the number of sittings reccommended (with Null = 1) to get numTreatments
    noSittings <- df[[i, noSittingsIndex]]
    RecNumSittings <- if (noSittings == "NULL"){ 1 } else {as.numeric(noSittings)}
    
    out[[i]] <- RecNumSittings
  }
  print(out)
}

#INPUTS: df: data frame of interest. Should be distinct advised laser treatment enteries. Must have UID, eyepartID and entereddatetime
#             columns
#       dateIndex: Coumn index for "entereddatetime" in df
#       UID_Treatment: Pass in UID_Treatment from global env (should be a way to call from env automatically)
#       ScNum_ofTreats: Toggle for whether to return numberical vector of rec sittings and number of treatment sittings within 
#                       15 * recnumsittings of advisory or to return logical vector with evaluations of whether number of sittings 
#                       is equal or greater than advised. (TRUE for Numerical, FALSE or OMIT for Logical)
#
#OUTPUT: See ScNum_ofTreats
ScaledCompliance <- function(df, dateIndex, UID_Treatment, ScNum_ofTreats) {
  #get indicies
  if(missing(ScNum_ofTreats)) {ScNumTreat = FALSE} else {ScNum_ofTreats = TRUE}
  
  UIDIndex <- grep("UID", colnames(df))
  eyeIndex <- grep("eyepartid", colnames(df))
  noSittingsIndex <- grep("noofsitting", colnames(df))
  
  if(ScNum_ofTreats) {out <- vector(mode = "logical", length(df))} else {
    out <- vector(mode = "numerical", length(df))
  }
  for (i in seq_along(df$UID)) {
    
    noSittings <- df[[i, noSittingsIndex]]
    RecNumSittings <- if (noSittings == "NULL"){ 1 } else {as.numeric(noSittings)}
    
    AdvisedDay <- df[[i, dateIndex]]
    timeDif <- difftime(UID_Treatment$entereddatetime, AdvisedDay, units = "days")
    corUID <- filter(UID_Treatment, UID == df[[i, UIDIndex]], eyepartid == df[[i, eyeIndex]],
                     timeDif >= 0, timeDif <= 15 * RecNumSittings) 
    
    if(ScNum_ofTreats) {out[[i]] <- nrow(corUID) } else {
    out[[i]] <- (nrow(corUID) >= RecNumSittings) }
  }
  print(out)
}




--------------------------------------------------------------------------------------------------------------
  ##DEPRICATED
  
  #finds number of rec sittings for each patient eye advisory
  ## DEPRECATED WITH EYEID ADVICE SHEET******************
  RecSittings_NoAdviceEyeID <- function(df, dateIndex, UID_Treatment) {
    #get indicies
    UIDIndex <- grep("UID", colnames(df))
    eyeIndex <- grep("eyepartid", colnames(df))
    noSittingsIndex <- grep("noofsitting", colnames(df))
    
    out <- vector(mode = "numeric", length(df))
    for (i in seq_along(df$UID)) {
      
      if(is.null(df[[i, eyeIndex]]) || df[[i, eyeIndex]] == '') { fac = 1} else { 
        if(df[[i, eyeIndex]] == 3) {fac = 2} else {fac = 1} }
      
      #multiply factor by the number of sittings reccommended (with Null = 1) to get numTreatments
      noSittings <- df[[i, noSittingsIndex]]
      RecNumSittings <- if (noSittings == "NULL"){ 1 * fac} else {as.numeric(noSittings) * fac}
      
      out[[i]] <- RecNumSittings
    }
    print(out)
  }

#finds number of rec sittings and number of treatment sittings within 15 * recnumsittings of advisory
#outputs whether number of sittins is equal or greater than advised
ComplexTreats <- function(df, dateIndex, UID_Treatment) {
  #get indicies
  UIDIndex <- grep("UID", colnames(df))
  eyeID <- grep("eyepartid", colnames(df))
  noSittingsIndex <- grep("noofsitting", colnames(df))
  
  out <- vector(mode = "numeric", length(df))
  for (i in seq_along(df$UID)) {
    
    if(is.null(df[[i, eyeID]]) || df[[i, eyeID]] == '') { fac = 1} else { 
      if(df[[i, eyeID]] == 3) {fac = 2} else {fac = 1} }
    
    #multiply factor by the number of sittings reccommended (with Null = 1) to get numTreatments
    noSittings <- df[[i, noSittingsIndex]]
    RecNumSittings <- if (noSittings == "NULL"){ 1 * fac} else {as.numeric(noSittings) * fac}
    
    AdvisedDay <- df[[i, dateIndex]]
    corUID <- filter(UID_Treatment, UID == df[[i, UIDIndex]], 
                     (difftime(UID_Treatment$entereddatetime, AdvisedDay, units = "days") <= (RecNumSittings * 15))) 
    
    out[[i]] <- (nrow(corUID) >= RecNumSittings)
  }
  print(out)
}