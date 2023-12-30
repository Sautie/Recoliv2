##' Determines whether x is a date object 
##' 6
is.date <- function(x) inherits(x, 'Date')
##' function for the conversion of bacterial colony diameters into three-valued categorical variable (S, I, R)
##' for a given isolate and antibiotic
##' 7
##' @param SABmatR    resistance threshold for a given antibiotic
##' @param SABmatS    susceptibility threshold for a given antibiotic
##' @param SBiomic    bacterial colony diameter
##' @return c         three-valued categorical variable
##' @export 
##' @author Miguel Sautié PIAAS
##' @examples 
##'  st <-Bmatrix(TR, TS, S)
Bmatrix <- function(SABmatR,SABmatS,SBiomic) {
  if (SABmatR=="resistance_naturelle")
  {
    c <-"NR"
  }
  else if (as.numeric(SBiomic) <=  as.numeric(SABmatR)){
    c <-"R"
  }
  else if (as.numeric(SBiomic) >= as.numeric(SABmatS)){
    c <-"S"
  }
  else if ((as.numeric(SBiomic) >as.numeric(SABmatR))&&(as.numeric(SBiomic)<as.numeric(SABmatS)))
  {
    c <-"I"
  }
  return(c)
}
##' Extracts the year from the date 
##' 8
##' @param str        date string 
##' @return s         four-digit year       
##' @export 
##' @author Miguel Sautié PIAAS
##' @examples 
##'  y <-an(yearstring)
an<- function(str) {
  sts <- unlist(strsplit(str,"/"))
  if (length(sts)==3) {
    if (sts[3]=="21"){
      s<-"2021"
    }
    if (sts[3]=="22"){
      s<-"2022"
    }
    if (sts[3]=="23"){
      s<-"2023"
    }
  } else {
    sts <- unlist(strsplit(str,"-"))
    if (length(sts)==3) {
      s<-sts[1]
    }
  }
  return(s)
}
##' generates categorical values (R, S or I) from colony diameters per isolate and per antibiotic  (Biomic),
##' from a minimum set of common antibiotics (C_Biomic) and a set of resistance and susceptibility thresholds
##' (ABmat).  
##' 9
##' @param Biomic     dataframe containing colony diameters per isolate and per antibiotic 
##' @param C_Biomic   minimum set of common antibiotics 
##' @param ABmat      dataframe containning the set of resistance and susceptibility thresholds
##' @param newdf      the new generated dataframe
##' @return newdf        
##' @export 
##' @author Miguel Sautié PIAAS
##' @examples 
##'  df<-Transformation3States(Biomic,C_Biomic,ABmat,df)
##'  
Transformation3States<-function(Biomic,C_Biomic,ABmat,newdf) {
for(i in 1:length(C_Biomic)){
  cc<-1
for(ii in 1:nrow(Biomic)){
  for(iii in 1:nrow(ABmat)){
    c <-NA
    if ((!is.na(ABmat[iii, "antibiotiques"])&&(!is.na(Biomic[ii, C_Biomic[i]]))&&(!is.na(Biomic[ii,"ANIMAL"])))){ 
      if ((C_Biomic[i]==ABmat[iii, "antibiotiques"]))
      {
        if (ABmat[iii,"espece_animale"]=="aviaire") {
          if ((Biomic[ii,"ANIMAL"]=="Goose")||(Biomic[ii,"ANIMAL"]=="Duck")||(Biomic[ii,"ANIMAL"]=="Chicken")||(Biomic[ii,"ANIMAL"]=="Bird")||(Biomic[ii,"ANIMAL"]=="Turkey")) #
          {   
          c <-Bmatrix (ABmat[iii,"limite_resistant"],ABmat[iii,"lim_sensible"],Biomic[ii, C_Biomic[i]])
          break
          }
        }
        else if (ABmat[iii,"espece_animale"]=="ruminant") {
          if ((Biomic[ii,"ANIMAL"]=="Cattle")||(Biomic[ii,"ANIMAL"]=="Buffalo")||(Biomic[ii,"ANIMAL"]=="Goat")||(Biomic[ii,"ANIMAL"]=="Bovine mastitis")||(Biomic[ii,"ANIMAL"]=="Sheep")||(Biomic[ii,"ANIMAL"]=="Reindeer"))
          {
            c <-Bmatrix (ABmat[iii,"limite_resistant"],ABmat[iii,"lim_sensible"],Biomic[ii, C_Biomic[i]])
            break
          }
        }
        else if (ABmat[iii,"espece_animale"]=="equin")  {
          if (Biomic[ii,"ANIMAL"]=="Horse")
          {
            c <-Bmatrix (ABmat[iii,"limite_resistant"],ABmat[iii,"lim_sensible"],Biomic[ii, C_Biomic[i]])
            break
          }
        }
        else  if (ABmat[iii,"espece_animale"]=="porc")  { #alpaga: pseudo-ruminant
          if ((Biomic[ii,"ANIMAL"]=="Cat")||(Biomic[ii,"ANIMAL"]=="Alpaga")||(Biomic[ii,"ANIMAL"]=="Amphibian")||(Biomic[ii,"ANIMAL"]=="Furet")||(Biomic[ii,"ANIMAL"]=="Fish")||(Biomic[ii,"ANIMAL"]=="Salmon")||(Biomic[ii,"ANIMAL"]=="Swine")||(Biomic[ii,"ANIMAL"]=="Dog")||(Biomic[ii,"ANIMAL"]=="Reptile")||(Biomic[ii,"ANIMAL"]=="Snake")||(Biomic[ii,"ANIMAL"]=="Trout")||(Biomic[ii,"ANIMAL"]=="Rabbit")||(Biomic[ii,"ANIMAL"]=="Primate")||(Biomic[ii,"ANIMAL"]=="Lizard")||(Biomic[ii,"ANIMAL"]=="Mammal")||(Biomic[ii,"ANIMAL"]=="Salmon")||(Biomic[ii,"ANIMAL"]=="Turtle/turtoise")||(Biomic[ii,"ANIMAL"]=="Lama")||(Biomic[ii,"ANIMAL"]=="Furet")||(Biomic[ii,"ANIMAL"]=="Primate"))
          {
            c <-Bmatrix (ABmat[iii,"limite_resistant"],ABmat[iii,"lim_sensible"],Biomic[ii, C_Biomic[i]])
            break
          }
        }
       }
    }
  }
  newdf[cc, C_Biomic[i]] <-c
  cc<-cc+1
}
}
  return(newdf)
}

##' Generates the profileAMR column in the newdf dataframe 
##' from the ternary matrix (I, R, S) (Biomic) 
##' and the minimum set of common antibiotics (C_Biomic).
##' 10
##' @param Biomic      Dataframe containing ternary matrix (I, R, S) for a given set of isolates and antibiotics
##' @param C_Biomic    The minimum set of common antibiotics 
##' @param newdf       
##' @return c         
##' @export 
##' @author Miguel Sautié PIAAS
##' @examples 
##'  st <-profileAMR(Biomic, CBiomic, st)
profileAMR<-function(Biomic,C_Biomic,newdf) {
cc<-1
for(i in 1:nrow(newdf)){
  AMR<-NA
  for(ii in 1:length(C_Biomic)){
   if (!is.na(newdf[i, C_Biomic[ii]])) {
    if ((newdf[i, C_Biomic[ii]]=="R")||(newdf[i, C_Biomic[ii]]=="NR"))
    {
      if (is.na(AMR)){
        AMR<-C_Biomic[ii]
      }
      else {
        AMR<-paste0(AMR,":",C_Biomic[ii])
      }
    }
  }
  }
  newdf[cc,"profil_AMR"]<-AMR 
  cc<-cc+1
}
return(newdf)
}

##' Generates the Animal column in the newdf dataframe 
##' based on the grouping of the veterinary species in 5 groups: 
##' "ruminants", "aviaire", "horse", "swine" and "other".
##' 11
##' @param Biomic      Dataframe containing the old ANIMAL column 
##' @param newdf       dataframe containing the new Animal column
##' @return c         
##' @export 
##' @author Miguel Sautié PIAAS
##' @examples 
##'  df <-Animal(Biomic,df)
Animal<-function(Biomic,newdf) {
  cc<-1
  for(ii in 1:nrow(Biomic)){
    c<-NA
    if (!is.na(Biomic[ii,"ANIMAL"])) {
      if (Biomic[ii,"ANIMAL"]!="") {
        if ((Biomic[ii,"ANIMAL"]=="Goose")||(Biomic[ii,"ANIMAL"]=="Duck")||(Biomic[ii,"ANIMAL"]=="Chicken")||(Biomic[ii,"ANIMAL"]=="Bird")||(Biomic[ii,"ANIMAL"]=="Turkey")) #
        { c<-"aviaire"}
        else if ((Biomic[ii,"ANIMAL"]=="Cattle")||(Biomic[ii,"ANIMAL"]=="Buffalo")||(Biomic[ii,"ANIMAL"]=="Goat")||(Biomic[ii,"ANIMAL"]=="Bovine mastitis")||(Biomic[ii,"ANIMAL"]=="Sheep")||(Biomic[ii,"ANIMAL"]=="Reindeer"))
        { c<-"ruminant"}
        else if (Biomic[ii,"ANIMAL"]=="Horse")
        { c<-"equin"}
        else if (Biomic[ii,"ANIMAL"]=="Swine")
        { c<-"Swine"} 
        else
        { c<-"other"}
      }
    }
    newdf[cc, "Animal"] <-c
    cc<-cc+1
  }
  return(newdf)
}
##' copies 5 columns into the new dataframe
##'and generates 2 new columns, 
##'one containing the year and one containing another grouping of the veterinary species in 5 categories.
##' 12
##' @param Biomic      Dataframe containing old columns
##' @param df          dataframe containing the column copies
##' @param C_Biomic    minimal set of antibiotics   
##' @return df       
##' @export 
##' @author Miguel Sautié PIAAS
##' @examples 
##'  df <-AllCButAB(Biomic,C_Biomic, df)
AllCButAB<-function(Biomic,C_Biomic,df) {
cc<-1
for(ii in 1:nrow(Biomic)){
  df[cc,"Date"] <-Biomic[ii,"DATE"] # "DATE"
  df[cc,"SPECIMEN_ID"] <- Biomic[ii,"SPECIMENID"] #"SPECIMENID"
  df[cc,"SPECIMEN"] <- Biomic[ii,"SPECIMEN"]
  df[cc,"ORGANISM"] <- Biomic[ii,"ORGANISM"]
  df[cc,"ANIMALo"] <- Biomic[ii,"ANIMAL"]
  df[cc,"AN"] <-an(Biomic[ii,"DATE"])
  cc<-cc+1
}
df<-Animal(Biomic,df)
return (df)
}

##'Generates a new column (profileMDR) in the dataframe df
##' 13
##' @param Lmdr       minimum number of antibiotic families (Lmdr+2) to which a given isolate is resistant to define the value "MDR" or "Non MDR".
##' @param newdf      new generated dataframe
##' @param C_Biomic   minimal set of common antibiotics 
##' @param C_Family   families to which the antibiotics in the minimum set of common antibiotics belong
##' @return newdf       
##' @export 
##' @author Miguel Sautié PIAAS
##' @examples 
##'  df <-profileMDR1(Biomic,C_Biomic, df)

profileMDR1<-function(newdf, C_Family, C_Biomic, Lmdr=1) {
     cc<-1
     for(i in 1:nrow(newdf)){
       mdr<-"NonMDR"
       cmdr<-""
       if (!is.na(newdf[cc,"profil_AMR"])) {
         po<-unlist(gregexpr(pattern =':',newdf[cc,"profil_AMR"]))
         if (length(po)>1)
         {
           f<-c(0,po, nchar(newdf[cc,"profil_AMR"])+1)
           for(i in 1:(length(f)-1)){
             ss<- substr(newdf[cc,"profil_AMR"], f[i]+1, f[i+1]-1)
            
             p<-grep(ss, C_Biomic)
             if (length(p)==1){
              if (C_Family[p]!="RN") {
               b<-grepl(C_Family[p], cmdr)
               if (!b) {
                 if (nchar(cmdr)==0){
                   cmdr<-C_Family[p]  
                 } else {
                   cmdr<-paste0(cmdr,":", C_Family[p])
                 }
               }
             }
             }
             }
           mdrpo<-unlist(gregexpr(pattern =':',cmdr))
           if (length(mdrpo)>Lmdr)
           {
             mdr<-"MDR"
           }
           else  {
             mdr<-"NonMDR"
           }
         }
       }
       newdf[cc,"profil_MDR"]<-mdr
       cc<-cc+1
     }
  return(newdf)
}
