
#
#
##' search in the Biomic No_Ref column for the identifier x or its transformed version according to
##' the simplified M prefix rule 
##' 1
##' @param x      Identifier string
##' @param Biomic dataframe
##' @return x  the original (or transformed )identifier  
##' @export 
##' @author Miguel Sautié PIAAS
##' @examples 
##'  st <- funM2(s, Biomic)
funM2 <- function(x, Biomic) {
  if (grepl('et', x))  {
    cp<-unlist(gregexpr('et', x))
    x<-substring(x,1,cp[1]-1) }
  if (grepl('M', x))  {
    fin<-c("a", "tr", "e", "u", "s", "-CL")
    if (sum(str_detect(Biomic$No_Ref, x)) > 0)
      {
      print(x)
      return(x)
      }
      else 
      { 
        for (f in fin) {
          nom1<-paste0(x, f)
          if (sum(str_detect(Biomic$No_Ref, nom1)) > 0)
          { x<- nom1 
       
          break}
        }
      }
   
  }
  return(x)
}

##' Extract the st2 suffix remaining after removing the maximum size zero prefix.
##' The parameter dd is defined as the number of zeros that are ignored because they are not part of 
##' the st2 prefix of zeros .
##' 2
##' @param st2      string identifier
##' @param dd      integer
##' @return        the original (or transformed )identifier
##' @export 
##' @author Miguel Sautié PIAAS
##' @examples 
##'  st<-funST2(st2, 3) 
##'  
funST2<- function(st2, dd) {
  #dd=2
  cp<-unlist(gregexpr('0', st2))
  if (cp[length(cp)]!=-1) {
    j<-0
    repeat {
      if (cp[length(cp)-j]==length(cp)-j)
      {
        st2<-substring(st2,cp[length(cp)-j]+1,nchar(st2))  #st: st2
        return(st2)
      }
      j<-j+1
      if (j==dd)
      { return(st2)}
    }
  }
}

##'Looks for the identifier x, or its transformed version 
##'in the No_Ref column of df2 (Biomic). 
##'he transformation rules for SHY and STF prefixes are considered.
##' 3
##' @param df2     dataframe
##' @param x       string identifier
##' @return        s the original (or transformed )identifier
##' @export 
##' @author Miguel Sautié PIAAS
##' @examples 
##'  s<- funSHYST(st, df) 
##'  
funSHYST<- function(x, df2, sts=c("SHY23", "SHY2023", "SHY-2023", "STF-2023", "STF-2022") ) {
  if (grepl('SH', x)||grepl('ST', x))   {
    stp<-unlist(gregexpr('-', x))
    #print(stp)
    for (i in sts) 
    {
      if (length(stp)==2)
      { st2<-substring(x,stp[2]+1,nchar(x)) }  #substring of x, st2 (OmniL)
      else if (length(stp)==1)
      { st2<-substring(x,stp[1]+1,nchar(x)) }
      nom0<-paste0(i,"-",st2)                   #transformation 1 of identifier x
      lst2<-nchar(st2)                         #st2 substring size
      if (lst2==5)
      {dd<-3}
      else if (lst2==4)
      {dd<-2}
      nom1<-paste0(i,"-0", st2)                #transformation 2 of identifier x
      if  (sum(str_detect(df2$No_Ref, nom0)) > 0)  #search for transformed version 1 of x in Biomic
      { x<-nom0
      break}
      else if (sum(str_detect(df2$No_Ref, nom1)) > 0) #hardly likely
      { x<-nom1
      break}
      else {
        st2<-funST2(st2, dd)                  #Determines the st2 suffix from the removal of a maximal prefix of zeros 
        
        if (nchar(st2)<lst2)
        {
          nom1<-paste0(i,"-",st2)
          nom2<-paste0(i,"-0",st2)
          if (sum(str_detect(df2$No_Ref, nom1)) > 0)   #Biomic search for transformed version 1 of x based on maximal prefix of zeros
          { x<-nom1 
             break
          }
          else if (sum(str_detect(df2$No_Ref, nom2)) > 0)  # search in Biomic for transformed version 2 of x based on maximal prefix of zeros 
          { x<-nom2 
          break}
        }
      }
      
    }
  }
  return(x)
}
##' Search in Biomic for OmniL identifiers or their modified versions
##'according to the 3 transformation rules for prefixes M, SHY and STF.
##' The output: OmniL with the original identifiers or the modified versions of them 
##' found in Biomic as well as the Biomic  dataframe with additional lines corresponding 
##' to the non-prefixed identifiers in the OmniLab file. 
##'This function is intended to be run before merging the two dataframes 
##'with the native merge function. 
##' 4
##' @param Omnil   Omnilab dataframe
##' @param Biomic  Biomic dataframe
##' @return        list of both modified dataframes 
##' @export 
##' @author Miguel Sautié PIAAS
##' @examples 
##'  Ldf<-selConversion(df1, df2) 
selConversion<- function(OmniL,Biomic){
for(i in 1:nrow(OmniL)) {       # 
  word<-toupper(OmniL[i, "No_Ref"])
  OmniL[i, "No_Ref"] <- funM2(word, Biomic)
  OmniL[i, "No_Ref"]<-  funSHYST(word, Biomic)
  if ((!grepl('ST', word))&&(!grepl('SH', word))&&(!grepl('M', word))){
    cp2<-unlist(gregexpr('22',word))
    cp3<-unlist(gregexpr('23',word))
    if (cp2[1]==1||cp3[1]==1){
      a <- rep(NA, 22)
      a[1]<-word
      Biomic <- rbind(Biomic, a)
    }
  }
}
  return(list(OmniL, Biomic))
}

##' simplified version of selConversion that does not add non-prefixed identifiers to Biomic
##' 5
##' @param Omnil   Omnilab dataframe
##' @param Biomic  Biomic dataframe
##' @return        modified OmniLab dataframe
##' @export 
##' @author Miguel Sautié PIAAS
##' @examples 
##'  mdf-SselConversion(df1, df2) 
SselConversion<- function(OmniL,Biomic){
  for(i in 1:nrow(OmniL)) {       # 
    word<-toupper(OmniL[i, "No_Ref"])
    OmniL[i, "No_Ref"] <- funM2(word, Biomic)
    OmniL[i, "No_Ref"]<-  funSHYST(word, Biomic)
  }
  return(OmniL)
}