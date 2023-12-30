#
##'replace "N/A" by NA in the dataframe df
##' 14
rmNA<- function(df) {
  for(i in 1:ncol(df)){
    for(ii in 1:nrow(df)){

      if ((grepl("N/A", df[ii,i], ignore.case=TRUE))||(df[ii,i]=="")) {   #
        df[ii,i]<-NA
      }

    }
  }
  return(df)
}

##'Extracts year from the date
##' 15
##' @param str       date string
##' @return newdf
##' @export
##' @author Miguel Sautié  PIAAS
##' @examples
##'  df <-year(strDate)
year<- function(str, sep="-") {
 # sep<-"/"
  cp<-unlist(gregexpr(sep,str))
  if (sep=="/"){
    if (length(cp)!=1){
      s<-substring(str,cp[2]+1,nchar(str))
    }
  }
  else {
  if (length(cp)!=1){
    s<-substring(str,1,cp[1]-1)
  }
    }
  return(s)
}
# pathotype #1 NTEC2
##' Boolean function to define the NTEC2 pathotype
##' 16
##' @param str     string
##' @return boolean value
##' @export
##' @author Miguel Sautié PIAAS
##' @examples
##'  b <-NTEC2(str)
NTEC2<- function(str) {
  B<-FALSE
  if (grepl("CNF2", str, ignore.case = TRUE))
  {
    B<-TRUE
  }
  return (B)
}

# pathotype #2 EPEC
##' Boolean function to define the EPEC pathotype
##' 17
##' @param str     string
##' @return boolean value
##' @export
##' @author Miguel Sautié PIAAS
##' @examples
##'  b <-EPEC(str)
EPEC<- function(str) {
  B<-FALSE
  if (grepl("Eae", str, ignore.case = TRUE))
  {
    B<-TRUE
  }
  return (B)
}

# pathotype #2 EXPEC
##' Boolean function to define the EXPEC pathotype
##' 18
##' @param str     string
##' @return boolean value
##' @export
##' @author Miguel Sautié PIAAS
##' @examples
##'  b <-EXPEC(str)
EXPEC<- function(str) {
  B<-FALSE
  if (grepl("Aero", str, ignore.case = TRUE)||grepl("TSH", str, ignore.case = TRUE)||grepl("CNF", str, ignore.case = TRUE)||grepl("P", str, ignore.case = TRUE))
  {
    B<-TRUE
  }
  return (B)
}

# pathotype #3 EIEC
##' Boolean function to define the EIEC pathotype
##' 19
##' @param str     string
##' @return boolean value
##' @export
##' @author Miguel Sautié PIAAS
##' @examples
##'  b <-EIEC(str)
EIEC<- function(str) {
  B<-FALSE
  if (xor(grepl("ipaH", str, ignore.case = TRUE),grepl("ial", str, ignore.case = TRUE)))
  {
    B<-TRUE
  }
  return (B)
}

# pathotype #4 EAEC
##' Boolean function to define the EAEC pathotype
##' 20
##' @param str     string
##' @return boolean value
##' @export
##' @author Miguel Sautié PIAAS
##' @examples
##'  b <-EAEC(str)
EAEC<- function(str) {
  B<-FALSE
  if (xor(xor(grepl("aggR", str, ignore.case = TRUE),grepl("aatA", str, ignore.case = TRUE)), grepl("aaiC", str, ignore.case = TRUE)))
  {
    B<-TRUE
  }
  return (B)
}

# pathotype #5 ETEC
##' Boolean function to define the ETEC pathotype
##' 21
##' @param str     string
##' @return boolean value
##' @export
##' @author Miguel Sautié PIAAS
##' @examples
##'  b <-ETEC(str)
ETEC<- function(str) {
  B<-FALSE
  if (grepl("STa", str, ignore.case = TRUE)||grepl("STb", str, ignore.case = TRUE)||grepl("LT", str, ignore.case = TRUE))
  {
    B<-TRUE
  }
  return (B)
}
# pathotype #6 STEC
##' Boolean function to define the STEC pathotype
##' 22
##' @param str     string
##' @return boolean value
##' @export
##' @author Miguel Sautié PIAAS
##' @examples
##'  b <-STEC(str)
STEC<- function(str) {
  B<-FALSE
  if (grepl("STx1", str, ignore.case = TRUE)||grepl("STx2", str, ignore.case = TRUE))
  {
    B<-TRUE
  }
  return (B)
}
# pathotype #8 APEC
##' Boolean function to define the APEC pathotype
##' 23
##' @param str     string
##' @return boolean value
##' @export
##' @author Miguel Sautié PIAAS
##' @examples
##'  b <-APEC(str)
APEC<- function(str) {
  B<-FALSE
  #iroN, iss, hlyF, ompT     Aero
  if ((grepl("ompT", str, ignore.case = TRUE)&&grepl("hlyF", str, ignore.case = TRUE)&&grepl("iss", str, ignore.case = TRUE)&&grepl("iroN", str, ignore.case = TRUE))&&(!grepl("Aero", str, ignore.case = TRUE)) )
  {
    B<-TRUE
  }
  #iroN,hlyF, ompT, Aero   iss
  else if ((grepl("iroN", str, ignore.case = TRUE)&&grepl("hlyf", str, ignore.case = TRUE)&&grepl("ompT", str, ignore.case = TRUE)&&grepl("Aero", str, ignore.case = TRUE))&&(!grepl("iss", str, ignore.case = TRUE)) )
  {
    B<-TRUE
  }
  #iss, hlyF, ompT, Aero   iroN
  else if ((grepl("iss", str, ignore.case = TRUE)&&grepl("hlyf", str, ignore.case = TRUE)&&grepl("ompT", str, ignore.case = TRUE)&&grepl("Aero", str, ignore.case = TRUE))&&(!grepl("iroN", str, ignore.case = TRUE)) )
  {
    B<-TRUE
  }
  #iroN, iss, hlyF, Aero    ompT
  if ((grepl("iroN", str, ignore.case = TRUE)&&grepl("iss", str, ignore.case = TRUE)&&grepl("hlyF", str, ignore.case = TRUE)&&grepl("Aero", str, ignore.case = TRUE))&&(!grepl("ompT", str, ignore.case = TRUE)) )
  {
    B<-TRUE
  }
  #iroN, iss, ompT, Aero     hlyF
  if ((grepl("iroN", str, ignore.case = TRUE)&&grepl("iss", str, ignore.case = TRUE)&&grepl("ompT", str, ignore.case = TRUE)&&grepl("Aero", str, ignore.case = TRUE))&&(!grepl("hlyF", str, ignore.case = TRUE)) )
  {
    B<-TRUE
  }
  #iroN, iss, hlyF, ompT, Aero
  if (grepl("iroN", str, ignore.case = TRUE)&&grepl("iss", str, ignore.case = TRUE)&&grepl("hlyF", str, ignore.case = TRUE)&&grepl("ompT", str, ignore.case = TRUE)&&grepl("Aero", str, ignore.case = TRUE))
  {
    B<-TRUE
  }
  return (B)
}
# pathotype #9 probAPEC
##' Boolean function to define the probAPEC pathotype
##' 24
##' @param str     string
##' @return boolean value
##' @export
##' @author Miguel Sautié PIAAS
##' @examples
##'  b <-probAPEC(str)
probAPEC<- function(str) {
  B<-FALSE
  #iroN, iss, hlyF   ompT Aero
  if ( ((grepl("iroN", str, ignore.case = TRUE)||grepl("iss", str, ignore.case = TRUE)||grepl("hlyF", str, ignore.case = TRUE))&&((!grepl("ompT", str, ignore.case = TRUE))&&(!grepl("Aero", str, ignore.case = TRUE)) )) )
  {
    B<-TRUE
  }
  #iroN, iss, ompT    hlyF Aero
  else  if (((grepl("iroN", str, ignore.case = TRUE)||grepl("iss", str, ignore.case = TRUE)||grepl("ompT", str, ignore.case = TRUE))&&((!grepl("hlyF", str, ignore.case = TRUE))&&(!grepl("Aero", str, ignore.case = TRUE)) )) )
  {
    B<-TRUE
  }
  #iroN, iss, Aero   hlyF ompT
  else  if(((grepl("iroN", str, ignore.case = TRUE)||grepl("iss", str, ignore.case = TRUE)||grepl("Aero", str, ignore.case = TRUE))&&((!grepl("hlyF", str, ignore.case = TRUE))&&(!grepl("ompT", str, ignore.case = TRUE)) )) )
  {
    B<-TRUE
  }
  #iss, hlyF, ompT   Aero iroN
  else  if (((grepl("iss", str, ignore.case = TRUE)||grepl("hlyF", str, ignore.case = TRUE)||grepl("ompT", str, ignore.case = TRUE))&&((!grepl("Aero", str, ignore.case = TRUE))&&(!grepl("iroN", str, ignore.case = TRUE)) )) )
  {
    B<-TRUE
  }
  #iss, hlyF, Aero  iroN ompT
  else  if (((grepl("iss", str, ignore.case = TRUE)||grepl("hlyF", str, ignore.case = TRUE)||grepl("Aero", str, ignore.case = TRUE))&&((!grepl("iroN", str, ignore.case = TRUE))&&(!grepl("ompT", str, ignore.case = TRUE)) )) )
  {
    B<-TRUE
  }
  #hlyF, ompT, Aero  iss iroN
  else  if (((grepl("hlyF", str, ignore.case = TRUE)||grepl("ompT", str, ignore.case = TRUE)||grepl("Aero", str, ignore.case = TRUE))&&((!grepl("iroN", str, ignore.case = TRUE))&&(!grepl("iss", str, ignore.case = TRUE)) )) )
  {
    B<-TRUE
  }
  #iss, ompT, Aero   hlyF, iroN
  else  if (((grepl("iss", str, ignore.case = TRUE)||grepl("ompT", str, ignore.case = TRUE)||grepl("Aero", str, ignore.case = TRUE))&&((!grepl("iroN", str, ignore.case = TRUE))&&(!grepl("hlyF", str, ignore.case = TRUE)) )) )
  {
    B<-TRUE
  }
  #iroN, ompT, Aero  iss, hlyF
  else  if (((grepl("iroN", str, ignore.case = TRUE)||grepl("ompT", str, ignore.case = TRUE)||grepl("Aero", str, ignore.case = TRUE))&&((!grepl("iss", str, ignore.case = TRUE))&&(!grepl("hlyF", str, ignore.case = TRUE)) )) )
  {
    B<-TRUE
  }
  #iroN,  hlyF, Aero  iss,ompT
  else  if (((grepl("iroN", str, ignore.case = TRUE)||grepl("hlyF", str, ignore.case = TRUE)||grepl("Aero", str, ignore.case = TRUE))&&((!grepl("iss", str, ignore.case = TRUE))&&(!grepl("ompT", str, ignore.case = TRUE)) )) )
  {
    B<-TRUE
  }
  # iroN, hlyF, ompT  iss, Aero
  else  if (((grepl("iroN", str, ignore.case = TRUE)||grepl("hlyF", str, ignore.case = TRUE)||grepl("ompT", str, ignore.case = TRUE))&&((!grepl("iss", str, ignore.case = TRUE))&&(!grepl("Aero", str, ignore.case = TRUE)) )) )
  {
    B<-TRUE
  }
  return (B)
}
# Pathotype 10  hrAPEC: APEC haut-risque   (IF hlyF AND ompT AND O78)
##' Boolean function to define the hrAPEC pathotype
##' 25
##' @param str     string
##' @return boolean value
##' @export
##' @author Miguel Sautié PIAAS
##' @examples
##'  b <-hrAPEC(str)
hrAPEC<- function(str) {
  B<-FALSE
  if ( (grepl("hlyF", str, ignore.case = TRUE)&&grepl("ompT", str, ignore.case = TRUE))&&(!grepl("ompT", str, ignore.case = TRUE))&&(!grepl("078", str, ignore.case = TRUE)) )
  {
    B<-TRUE
  }
  return (B)
}
##' Concatenates strings
##' 26
##' @param st0     string 1
##' @param st     string 2
##' @param sep     separator(dv,:)
##' @return boolean value
##' @export
##' @author Miguel Sautié PIAAS
##' @examples
##'  b <-joinNames(str, str2)
joinNames<- function(st0, st, sep=":") {
  if (nchar(st0)==0) {
    st0<-paste0(st0, st)
  } else{
    st0<-paste0(st0, sep, st)
  }
  return(st0)
}
##' missing value for pathotypes (Other)
##' 27
##' @param st0        string 1
##' @param patho     string 2
##' @param mv       missing value
##' @return boolean value
##' @export
##' @author Miguel Sautié PIAAS
##' @examples
##'  b <-mvPathoTypes(str, patho)
mvPathoTypes<- function(st0, patho, mv=NA) {
  if ( (!hrAPEC(st0))&&(!probAPEC(st0))&&(!APEC(st0))&&(!NTEC2(st0))&&(!EPEC(st0))&&(!EXPEC(st0))&&(!EIEC(st0))&&(!EAEC(st0))&&(!ETEC(st0))&&(!STEC(st0))&&(!APEC(st0))&&(!probAPEC(st0))&&(!hrAPEC(st0)))
  {patho<-mv}
  return (patho)
}
#pathotypes
##' determines the pathotype for a given isolat based on the "Résultat" column
##' 28
##' @param fOmniL       dataframe Omnilab
##' @param df           dataframe
##' @return dataframe
##' @export
##' @author Miguel Sautié PIAAS
##' @examples
##'  df <-fpathotypes(fOmniL)
fpathotypes<- function(fOmniL,coname="Résultat", df=newdf) {
  cc<-1
  for(i in 1:nrow(fOmniL)){
    pathoT<-""
    if (NTEC2(fOmniL[i, coname])) {
      st<-"NTEC2"
      pathoT<-joinNames(pathoT, st)
    }
    if (EPEC(fOmniL[i, coname])) {
      st<-"EPEC"
      pathoT<-joinNames(pathoT, st)
    }
    if (EXPEC(fOmniL[i, coname])) {
      st<-"EXPEC"
      pathoT<-joinNames(pathoT, st)
    }
    if (EIEC(fOmniL[i, coname])) {
      st<-"EIEC"
      pathoT<-joinNames(pathoT, st)
    }
    if (EAEC(fOmniL[i, coname])) {
      st<-"EAEC"
      pathoT<-joinNames(pathoT, st)
    }
    if (ETEC(fOmniL[i, coname])) {
      st<-"ETEC"
      pathoT<-joinNames(pathoT, st)
    }
    if (STEC(fOmniL[i, coname])) {
      st<-"STEC"
      pathoT<-joinNames(pathoT, st)
    }
    if (APEC(fOmniL[i, coname])) {
      st<-"APEC"
      pathoT<-joinNames(pathoT, st)
    }
    if (probAPEC(fOmniL[i, coname])) {
      st<-"probAPEC"
      pathoT<-joinNames(pathoT, st)
    }
    if (hrAPEC(fOmniL[i, coname])) {
      st<-"hrAPEC"
      pathoT<-joinNames(pathoT, st)
    }
    pathoT<-mvPathoTypes(fOmniL[i, coname], pathoT)
    df[cc,"Pathotype"] <-pathoT
    cc<-cc+1
  }
  return(df)
}

#VirotypeStandard
##' determines the standard virotype for a given isolat based on the "Résultat" column
##' 29
##' @param fOmniL       dataframe Omnilab
##' @param df           dataframe
##' @param C_Omni       gene set for determining virotypes
##' @return dataframe
##' @export
##' @author Miguel Sautié PIAAS
##' @examples
##'  df <-fVirotypeStandard(fOmniL)
fVirotypeStandard<- function(fOmniL,coname="Résultat", df=newdf, v=C_Omni) {
  cc<-1
  for(ii in 1:nrow(fOmniL)){
    viroT<-""
    for(i in 1:length(v)){
      if (grepl(v[i], fOmniL[ii, coname], ignore.case = TRUE)){
        viroT<-joinNames(viroT, v[i])
      }
    }
    if (nchar(viroT)==0){
      viroT<-NA
    }
    df[cc,"VirotypeStandard"]<-viroT
    cc<-cc+1
  }
  return(df)
}
##' #VirotypeElargis
##' determines the extended virotype for a given isolat based on the "Résultat" column
##' 30
##' @param fOmniL       dataframe Omnilab
##' @param df           new  dataframe (newdf)
##' @param v           gene set for determining extended virotypes (C_OmniVE)
##' @return dataframe
##' @export
##' @author Miguel Sautié PIAAS
##' @examples
##'  df <-fVirotypeElargis(fOmniL)

fVirotypeElargis<- function(fOmniL,coname="Résultat", df=newdf, v=C_OmniVE) {
  cc<-1
  for(ii in 1:nrow(fOmniL)){
    viroT<-""
    for(i in 1:length(v)){
      if (grepl(v[i], fOmniL[ii, coname], ignore.case = TRUE)){
        viroT<-joinNames(viroT, v[i])
      }
    }
    if (nchar(viroT)==0){
      viroT<-NA #"Other"
    }
    df[cc,"VirotypeElargis"]<-viroT  #viroT
    cc<-cc+1
  }
  return(df)
}
#VirotypeDterminants
##' detects the virotype determinants for a given isolat based on the "Résultat" column
##' 31
##' @param fOmniL       dataframe Omnilab
##' @param df           new  dataframe (newdf)
##' @param v           gene set for detecting virotypes determinants (C_OmniDet)
##' @return dataframe
##' @export
##' @author Miguel Sautié PIAAS
##' @examples
##'  df <-fVirotypeDterminants(OmniL)
fVirotypeDterminants<- function(fOmniL,coname="Résultat", df=newdf, v=C_OmniDet) {
  cc<-1
  for(ii in 1:nrow(fOmniL)){
    viroD<-""
    for(i in 1:length(v)){
      if (grepl(v[i], fOmniL[ii, coname], ignore.case = TRUE)){
        viroD<-joinNames(viroD, v[i], ",")
      }
    }
    if (nchar(viroD)==0){
      viroD<-NA #"Other"
    }
    df[cc,"VirotypeDterminants"]<-viroD
    cc<-cc+1
  }
  return(df)
}
##'Pathovirotypes
##' determine the pathovirotype for a given isolat based on the "Résultat" column
##' 32
##' @param fOmniL       dataframe Omnilab
##' @param df           new  dataframe (newdf)
##' @return dataframe
##' @export
##' @author Miguel Sautié PIAAS
##' @examples
##'  df <-fPathovirotype()
fPathovirotype<- function(df=newdf) {
  cc<-1
  for(i in 1:nrow(newdf)){
    pt<-newdf[i,"Pathotype"]
    ve<-newdf[i,"VirotypeElargis"]
    if ((!is.na(pt))&&(!is.na(ve)) ) {   #((pt!="Other")&&(ve!="Other")){
      newdf[cc,"Pathovirotype"]<-paste0(pt, ":", ve)
    } else if (is.na(pt))  #(pt=="Other")
    {
      newdf[cc,"Pathovirotype"]<-NA   #ve
    }
    else if (is.na(ve))   #(ve=="Other")
    {
      newdf[cc,"Pathovirotype"]<-NA   #pt
    }
    cc<-cc+1
  }
  return(newdf)
}
##' Hémolyse
##' ddetermine whether a given isolat is hemolytic or not based on the "Résultat" column
##' 33
##' @param fOmniL       dataframe Omnilab
##' @param df           new  dataframe (newdf)
##' @return dataframe
##' @export
##' @author Miguel Sautié PIAAS
##' @examples
##'  df <-fHemolyse(fOmniL)
#Hémolyse  "Nh", "Bh"
fHemolyse<- function(fOmniL,coname="Résultat", df=newdf) {
  cc<-1
  for(i in 1:nrow(fOmniL)){                           # if a given case has more than one hemolytic status, it will be beta-hemolytic
    if (grepl("Bh", fOmniL[i, coname], ignore.case = TRUE)){
      df[cc,"Hemolyse"]<-"Beta hemolytic"
    }
    else if (grepl("Nh", fOmniL[i, coname], ignore.case = TRUE)){
      df[cc,"Hemolyse"]<-"Non hemolytic"
    }
    else {
      df[cc,"Hemolyse"]<-"Unknown" }   #"Unknown"

    cc<-cc+1
  }
  return(df)
}
#ratio_isolats
##' extracts ratio_isolats from the "Résultat" column
##' 34
##' @param fOmniL       dataframe Omnilab
##' @param df           new  dataframe (newdf)
##' @return dataframe
##' @export
##' @author Miguel Sautié PIAAS
##' @examples
##'  df <-fratio_isolats(fOmniL)

fratio_isolats<- function(fOmniL,coname="Résultat", df=newdf) {
  cc<-1                    #if a given case has more than one isolate ratio, this function will keep the highest
  for(i in 1:nrow(fOmniL)){
    ri<-"unknown"
    if (grepl("3/3", fOmniL[i, coname], ignore.case = TRUE)){
      ri<-"(3/3)"
    }
    else if (grepl("2/3", fOmniL[i, coname], ignore.case = TRUE)){
      ri<-"(2/3)"
    }
    else if (grepl("1/3", fOmniL[i, coname], ignore.case = TRUE)){
      ri<-"(1/3)"
    }
    # else {
    #   ri<-"(1/3)"
    # }
    df[cc,"ratio_isolats"]<-ri
    cc<-cc+1
  }
  return(df)
}

#DescEchantillon
##' extracts sample description from the Nom.de.l.animal" column
##' 35
##' @param fOmniL       dataframe Omnilab
##' @param df           new  dataframe (newdf)
##' @return dataframe
##' @export
##' @author Miguel Sautié PIAAS
##' @examples
##'  df <-fDescEchantillon(fOmniL)
fDescEchantillon<- function(fOmniL,coname="Nom.de.l.animal",df=newdf) {
  cc<-1
  for(i in 1:nrow(fOmniL)){
    #ILE
    de<-"Other"
    if (grepl("ile", fOmniL[i, coname], ignore.case = TRUE))
    {
      de<-"Iléon"
    }else
      if (grepl("place", fOmniL[i, coname], ignore.case = TRUE))
      {
        de<-"Placenta"
      }
    else
      if (grepl("live", fOmniL[i, coname], ignore.case = TRUE))
      {
        de<-"Foie"
      }
    else
      if (grepl("fece", fOmniL[i, coname], ignore.case = TRUE))
      {
        de<-"Feces"
      }
    else
      if (grepl("bra", fOmniL[i, coname], ignore.case = TRUE))
      {
        de<-"Cerveau"
      }
    else
      if (grepl("col", fOmniL[i, coname], ignore.case = TRUE))
      {
        de<-"Côlon"
      }
    else
      if (grepl("mamm", fOmniL[i, coname], ignore.case = TRUE))
      {
        de<-"Glande mammaire"
      }
    else
      if (grepl("pou", fOmniL[i, coname], ignore.case = TRUE))
      {
         de<-"Poumon"
      }
    df[cc, "DescEchantillon"]<-de
    cc<-cc+1
  }
  return(df)
}
#Age
##' extracts age from the "Race" column
##' 36
##' @param fOmniL       dataframe Omnilab
##' @param df           new  dataframe (newdf)
##' @return dataframe
##' @export
##' @author Miguel Sautié PIAAS
##' @examples
##'  df <-fAge(fOmniL), old:df <-fRace(fOmniL)
fAge<- function(fOmniL,coname="Race", df=newdf) {
  cc<-1
  for(i in 1:nrow(fOmniL)){
    if ((grepl("MATERNITE" , fOmniL[i, coname], ignore.case = TRUE))||(grepl("Mamelle" , fOmniL[i, coname], ignore.case = TRUE)) )
    {df[cc, "Age"]<-"MATERNITE"}
    else    #if (is.na(fOmniL[i, "Race"]))
    {df[cc, "Age"]<-fOmniL[i, coname]}
    cc<-cc+1
  }
  return(df)
}
#Province
##' extracts province from the "Ville.du.propriétaire.de.l.animal" column
##' 37
##' @param fOmniL       dataframe Omnilab
##' @param df           new  dataframe (newdf)
##' @return dataframe
##' @export
##' @author Miguel Sautié PIAAS
##' @examples
##'  df <-fProvince(fOmniL)
fProvince<- function(fOmniL,coname="Ville.du.propriétaire.de.l.animal", df=newdf) {
  cc<-1
  for(i in 1:nrow(fOmniL)){
    #		 ST-ALEXIS			L'Avenir	Saint-Liboire			St-ESPRIT		Sainte-Helene-de-Bagot		Saint-Damase-de-Lauzon		St-Aimé	Saint MICHEL DE BELLECHASSE	 Les Maskoutains 	Saint-Frederic Robert-Cliche	Saint-Liguori	ST-NARCISSE DE BEAURIVAGE	Saint-Ignace-de-Stanbridge			St-ZEPHIRIN
    Prov <-"Other"
    if (!is.na(fOmniL[i,coname])) {
      if ((grepl("Amiante",fOmniL[i,coname], ignore.case = TRUE))||(grepl("Sainte-Praxede",fOmniL[i,coname], ignore.case = TRUE))||(grepl("Saint-Alexandre",fOmniL[i,coname], ignore.case = TRUE))||(grepl("Levis",fOmniL[i,coname], ignore.case = TRUE))||(grepl("Sainte-Sabine",fOmniL[i,coname], ignore.case = TRUE))||(grepl("Saint-Hyacinthe",fOmniL[i,coname], ignore.case = TRUE)) )
      {
        Prov <-"Québec"
      }
      else if ((grepl("ST-ALPHONSE",fOmniL[i,coname], ignore.case = TRUE))||(grepl("St-ISIDORE",fOmniL[i,coname], ignore.case = TRUE))||(grepl("Sainte-Cecile-de-Milton",fOmniL[i,coname], ignore.case = TRUE))||(grepl("Saint-FLAVIEN",fOmniL[i,coname], ignore.case = TRUE))||(grepl("Saint-Frederic",fOmniL[i, coname], ignore.case = TRUE))||(grepl("Saint-Etienne-des-Gres",fOmniL[i,coname], ignore.case = TRUE)) )
      {
        Prov <-"Québec"
      }
      else if ((grepl("Lotbinière",fOmniL[i,coname], ignore.case = TRUE))||(grepl("Cheneville",fOmniL[i,coname], ignore.case = TRUE))||(grepl("Bellechasse",fOmniL[i,coname], ignore.case = TRUE))||(grepl("Bagot",fOmniL[i,coname], ignore.case = TRUE))||(grepl("Saint-Guillaume",fOmniL[i,coname], ignore.case = TRUE))||(grepl("Saint-Nazaire-d'Acton",fOmniL[i,coname], ignore.case = TRUE)) )
      {
        Prov <-"Québec"
      }
      else if ((grepl("Saint-Dominique",fOmniL[i,coname], ignore.case = TRUE))||(grepl("ST-LEON-DE-STANDON",fOmniL[i,coname], ignore.case = TRUE))||(grepl("Sainte-Clotilde-de-Horton",fOmniL[i,coname], ignore.case = TRUE))||(grepl("Saint-Bernard-de-Michaudville",fOmniL[i,coname], ignore.case = TRUE))||(grepl("Mirabel",fOmniL[i,coname], ignore.case = TRUE))||(grepl("STE-MARIE",fOmniL[i,coname], ignore.case = TRUE)) )
      {
        Prov <-"Québec"
      }
      else if ((grepl("Farnham",fOmniL[i,coname], ignore.case = TRUE))||(grepl("Ange-Gardien",fOmniL[i,coname], ignore.case = TRUE))||(grepl("Upton",fOmniL[i,coname], ignore.case = TRUE))||(grepl("Saint-Theodore-d'Acton",fOmniL[i,coname], ignore.case = TRUE))||(grepl("Mirabel",fOmniL[i,coname], ignore.case = TRUE))||(grepl("Saint-Damase",fOmniL[i,coname], ignore.case = TRUE)) )
      {
        Prov <-"Québec"
      }
      else if ((grepl("Lyster",fOmniL[i,coname], ignore.case = TRUE))||(grepl("Ursule",fOmniL[i,coname], ignore.case = TRUE))||(grepl("Becancour",fOmniL[i,coname], ignore.case = TRUE))||(grepl("Saint-Jacques",fOmniL[i,coname], ignore.case = TRUE))||(grepl("Saint-Jude",fOmniL[i,coname], ignore.case = TRUE))||(grepl("Richelieu",fOmniL[i,coname], ignore.case = TRUE)) )
      {
        Prov <-"Québec"
      }
      else if (grepl("Clarenceville",fOmniL[i,coname], ignore.case = TRUE))
      {Prov <-"Québec"}
      if ((grepl("Beaurivage",fOmniL[i,coname], ignore.case = TRUE))||(grepl("Saint-Louis",fOmniL[i,coname], ignore.case = TRUE))||(grepl("Villeroy",fOmniL[i,coname], ignore.case = TRUE))||(grepl("CRANBOURNE",fOmniL[i,coname], ignore.case = TRUE))||(grepl("Saint-Prosper",fOmniL[i,coname], ignore.case = TRUE))||(grepl("Durham",fOmniL[i,coname], ignore.case = TRUE)) )
      {
        Prov <-"Québec"
      }
      else if ((grepl("La Broquerie",fOmniL[i,coname], ignore.case = TRUE))||(grepl("Niverville",fOmniL[i,coname], ignore.case = TRUE))||(grepl("Steinbach",fOmniL[i,coname], ignore.case = TRUE))||(grepl("Saint-Adolphe",fOmniL[i,coname], ignore.case = TRUE))||(grepl("Saint-François-Xavier",fOmniL[i,coname], ignore.case = TRUE))||(grepl("Sainte-Agathe",fOmniL[i,coname], ignore.case = TRUE)) )
      {
        Prov <-"Manitoba"
      }
    }
    df[cc,"Province"]<-Prov
    cc<-cc+1
    }
  return(df)
}

#Pays
##' extracts pays from the "Ville.du.propriétaire.de.l.animal" column
##' 38
##' @param fOmniL       dataframe Omnilab
##' @param df           new  dataframe (newdf)
##' @return dataframe
##' @export
##' @author Miguel Sautié PIAAS
##' @examples
##'  df <-fPays(fOmniL)
fPays<- function(fOmniL,coname="Ville.du.propriétaire.de.l.animal", df=newdf) {
  cc<-1
  for(i in 1:nrow(fOmniL)){
    #
    pays<-"Other"
    if (grepl("BEAUCOUZÉ",fOmniL[i,coname], ignore.case = TRUE))
    {
      pays<-"France"
    }
    else {
      pays<-"Canada"
    }
    df[cc,"Pays"]<-pays
    cc<-cc+1
  }
  return(df)
}
#Origine
##' extracts Origine from the "Espèce" column
##' 39
##' @param fOmniL       dataframe Omnilab
##' @param df           new  dataframe (newdf)
##' @return dataframe
##' @export
##' @author Miguel Sautié PIAAS
##' @examples
##'  df <-fOrigin(fOmniL)
fOrigin<- function(fOmniL,coname="Espèce", df=newdf) {
  cc<-1
  for(i in 1:nrow(fOmniL)){
    if (fOmniL[i,coname]=="PORCINE")
    {df[cc,"Origine"]<-"PORC"}
    else
    {df[cc,"Origine"]<-"Other"}
    cc<-cc+1
  }
  return(df)
}

firstDF<- function(fOmniL2, newdf)  {
  i<-1
  c<-1
  de<-c()
  dr<-c()
  dp<-c()
  ndA<-c()
  apa<-c()
  vpa<-c()
  ori<-c()
  race<-c()
  while (i<nrow(newdf)+1){
    ii<-1
    c<-1
    while(ii<nrow(fOmniL2)){

      if (!is.na(fOmniL2[ii,"NR"])&&(!is.na(newdf[i,"NR"]))&&newdf[i,"NR"]==fOmniL2[ii,"NR"]) {
        c<-ii
        break
      }
      ii<-ii+1
    }
    dr<-c(dr, fOmniL[c, "Date.de.réception"])
    dp<-c(dp,  fOmniL2[c,"Date.de.prélèvement"])
    ndA<-c(ndA, fOmniL2[c, "No.Dossier.Animal"])
    apa<-c(apa,fOmniL2[c,"Adresse.du.propriétaire.de.l.animal"])
    vpa<-c(vpa,fOmniL2[c,"Ville.du.propriétaire.de.l.animal"])
    de<-c(de,fOmniL2[c,"Nom.de.l.animal"])
    ori<-c(ori,fOmniL2[c, "Espèce"])
    race<-c(race,  fOmniL2[c,"Race"])

    i<-i+1
  }
  newdf <- cbind(newdf,
                 Date.de.réception=dr,
                 Date.de.prélèvement=dp,
                 No.Dossier.Animal=ndA,
                 Adresse.du.propriétaire.de.l.animal=apa,
                 Ville.du.propriétaire.de.l.animal=vpa,
                 Nom.de.l.animal=de,
                 Espèce=ori,
                 Race=race
  )
  return(newdf)
}

secondDF<- function(newdf, newdf2,C_Omni)  {
cc<-1
for(i in 1:length(C_Omni)){
  cc<-1
  for(ii in 1:nrow(newdf)){
    if (i==1){
      newdf2[cc,"DatePrel"]<-newdf[ii, "Date.de.prélèvement"]
      newdf2[cc,"DateReception"]<-newdf[ii, "Date.de.réception"]
      newdf2[cc,"AnPrel"]<-year(newdf[ii, "Date.de.prélèvement"], sep<-"/") #sep<-"/")
       newdf2[cc,"NDossier"]<-newdf[ii, "No.Dossier.Animal"]
       newdf2[cc,"ID"]<-newdf[ii,"NR"]
      newdf2[cc,"AdressePA"]<-newdf[ii,"Adresse.du.propriétaire.de.l.animal"]  #####
      newdf2[cc,"VillePA"]<-  newdf[ii,"Ville.du.propriétaire.de.l.animal"]   ####

    }
    if (grepl(C_Omni[i], newdf[ii, "genes"], ignore.case = TRUE)){
      newdf2[cc, C_Omni[i]] <-"1"
    } else {
      newdf2[cc, C_Omni[i]] <-"0"
    }
    cc<-cc+1
  }
}
return(newdf2)
}
