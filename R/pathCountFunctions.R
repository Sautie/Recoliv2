#source("pathFunctions.R")
##' case count
##' 40
##' @param df       dataframe Omnilab
##' @param c           type of count, absolute or relative.
##' @return count
##' @export
##' @author Miguel Sautié (PIAAS)
##' @examples
##'  n<-countcas(fOmniL)
countcas<- function(df, c=1) {
  Lu<-unique(df$NR)
  if (c==1) {
   ncas<-length(Lu)
  } else {
    ncas<-length(Lu)/nrow(df)
  }
  return(ncas)
}
##' Negative case count. The output of this function is affected by the absence of 4007 for a given case or sample
##' 41
##' @param df       dataframe Omnilab
##' @param c           type of count, absolute or relative.
##' @return count
##' @export
##' @author Miguel Sautié (PIAAS)
##' @examples
##'  nc<-countcasneg1(fOmniL)
countcasneg1<- function(df, c=1) {
df2 <- df[df$Code.d.analyse=="4007", ]
df3 <- df2[df2$Résultat== "NÉG", ]
Lu<-unique(df3$NR)
if (c==1) {
nncas<-length(Lu)
} else {
nncas<-length(Lu)/length(df3$NR)
}
return(nncas)
}
##' Positive case count. The output of this function is affected by the absence of 4007 for a given case or sample.
##' 42
##' @param df       dataframe Omnilab
##' @param c        type of count, absolute or relative.
##' @return count
##' @export
##' @author Miguel Sautié (PIAAS)
##' @examples
##'  nc<-countcaspos1(fOmniL)
countcaspos1<- function(df, c=1) {
df2 <- df[df$Code.d.analyse=="4007", ]
df3 <- df2[df2$Résultat!= "NÉG", ]
Lu<-unique(df3$NR)
if (c==1) {
  noncas<-length(Lu)
} else {
  noncas<-length(Lu)/length(df3$NR)
}
return(noncas)
}
##' positive case count 2. The output of this function is affected by the absence of 4007 for a given case or sample.
##' 43
##' @param df       dataframe Omnilab
##' @return count
##' @export
##' @author Miguel Sautié (PIAAS)
##' @examples
##'  nc<-countcaspos2(fOmniL)
countcaspos2<- function(df) {
df <- df[df$Code.d.analyse=="4007", ] ##29
i<-2
w<-1
vcas<-0
pp<-0
while(i<nrow(df))
{
  if ((df[i, "NR"]==df[i-1, "NR"]))
  { w<-i
  c<-0
  pp<-0
  while (df[w, "NR"]==df[w-1, "NR"])
  {
    if (df[w, "Résultat"]!="NÉG") { #at least one sample must be positive
      c<-1
    }
    w<-w+1
    pp<-pp+1
  }
  if(c==1)  #if there is at least one positive sample
  {
    vcas<-vcas+1
  }
  }
  else
    if ((df[i, "NR"]!=df[i-1, "NR"])&&(df[i, "Résultat"]!="NÉG"))  {
      vcas<-vcas+1
    }
  i<-i+pp+1
}
return(vcas)
}
##' Negative case count 2.
##' The output of this function is affected by the absence of 4007 for a given case or sample.
##' 44
##' @param df       dataframe Omnilab
##' @return count
##' @export
##' @author Miguel Sautié (PIAAS)
##' @examples
##'  nc<-countcasneg2(fOmniL)
countcasneg2<- function(df)
{
df <- df[df$Code.d.analyse=="4007", ] #54
neg<-0
Lu<-unique(df$NR)
i<-2
w<-1
n<-0
negn<-0
pp<-0
while(i<nrow(df))
{
  c<-0
  if ((df[i, "NR"]==df[i-1, "NR"]))
  {
    w<-i
    n<-0
    pp<-0
    while (df[w, "NR"]==df[w-1, "NR"])
    {
      if (fOmniL2[w, "Résultat"]=="NÉG") { #all samples must be negative
        n<-n+1
      }
      w<-w+1
      pp<-pp+1
    }
  }
  else {
    if ((df[i, "NR"]!=df[i-1, "NR"])&&(df[i, "Résultat"]=="NÉG")){
      n<-n+1
      c<-1
    }
  }
  if ((pp==n)||(c==1)) {
    negn<-negn+1
  }
  i<-i+pp+1
}
return(negn)
}
##'Sample counting.
##'The output of this function is affected by the absence of 4007 for a given sample.
##'The more samples that are not associated with any 4007.The more it underestimates the number of cases
##'45
##' @param df       dataframe Omnilab
##' @param c        type of count, absolute or relative.
##' @param corr     correction factor
##' @return count
##' @export
##' @author Miguel Sautié (PIAAS)
##' @examples
##'  nc<-countsamples(fOmniL)
countsamples<- function(df, c=1, corr=0) {
  #corr 1, corr 0
d <- df[df$Code.d.analyse=="4007", ] #172 0.419
if (c==1) {
 Echan<-nrow(d)-corr
}
else {
 Echan<-(nEchan)/(nrow(df)-corr)
}
return(Echan)
}
##' isolate counting
##'46
##' @param fOmniL2      dataframe Omnilab
##' @return count
##' @export
##' @author Miguel Sautié (PIAAS)
##' @examples
##'  nc<-countIsolates(fOmniL)
countIsolates<- function(fOmniL2) {
  d<-fOmniL2[fOmniL2$Résultat != "NÉG", ]
  d <- d[d$Code.d.analyse!="4007", ]
  d<-rmNA(d)

  i<-2
  ii<-0
  s<-0
  count_IS<-0
  cv<-c()
  cvv<-c()
  pp<-0
  while (i<nrow(d)) {
    if ((!is.na(d[i, "NR"]))&&(!is.na(d[i-1, "NR"]))&&(d[i, "NR"]==d[i-1, "NR"])){
      cp<-0
      c<-0
      ii<-i
      v<-0
      vv<-0
      pp<-0
      while ((!is.na(d[ii, "NR"]))&&(!is.na(d[ii-1, "NR"]))&&(d[ii, "NR"]==d[ii-1, "NR"])) {
        if (d[ii, "Nom.de.l.animal"]==d[ii-1, "Nom.de.l.animal"]){
          if ((d[ii, "Code.d.analyse"]=="4000")||(d[ii, "Code.d.analyse"]=="4002")||(d[ii, "Code.d.analyse"]=="4004"))
          {if (cp==0) {cv<-d[ii, "Code.d.analyse"] }
            else
            { cv<-c(cv, d[ii, "Code.d.analyse"]) } }
          else if ((d[ii, "Code.d.analyse"]=="4001")||(d[ii, "Code.d.analyse"]=="4003")||(d[ii, "Code.d.analyse"]=="4005"))
          { if (cp==0) {cvv<-d[ii, "Code.d.analyse"] }
            else
            { cvv<-c(cvv, d[ii, "Code.d.analyse"]) } }
          cp<-cp+1}
        else {
          if(c==0) {v<-length(unique(cv))}
          else {v<-v+length(unique(cv))}
          if(c==0) {vv<-length(unique(cvv)) }
          else {vv<-vv+length(unique(cv))}
          if (v<vv) {
            count_IS<-count_IS+vv
          }
          else {
            count_IS<-count_IS+v
          }
          v<-0
          vv<-0
          c<-0
        }
        ii<-ii+1
        pp<-pp+1
      }
    }
    i<-i+pp+1
  }
  return(count_IS)
}
##' Auxiliar function for hemolyse variable
##' 47
##' @param hemoI  hemolyse variable
##' @param st    string representing the identifier
##' @param chemo hemolyse categories
##' @return
##' @noRd
##' @author Miguel Sautié (PIAAS)
##' @examples
##'
hemof<- function(hemoI,st, chemo) {
  if (grepl(chemo[1], st, ignore.case = TRUE)) {
    if (!grepl(chemo[1], hemoI, ignore.case = TRUE))  {
      hemoI<-joinNames(hemoI, chemo[1])
    }
    }
  if (grepl(chemo[2],st, ignore.case = TRUE))  {
    if (!grepl(chemo[2], hemoI, ignore.case = TRUE))  {
      hemoI<-joinNames(hemoI, chemo[2])
    }
  }
return(hemoI)
}
##' auxiliar function for gene variable
##' 48
##' @param geneI  gene variable
##' @param st    string representing the identifier
##' @param cgenes gene categories
##' @return
##' @noRd
##' @author Miguel Sautié (PIAAS)
##' @examples
##'
genef<- function(geneI,st, cgenes) {
g<-1
while (g<length(cgenes))
{
  if (grepl(cgenes[g], st, ignore.case = TRUE)) {
    if (!grepl(cgenes[g], geneI, ignore.case = TRUE))  {
      geneI<-joinNames(geneI, cgenes[g])
    }
  }
  g<-g+1
}
return(geneI)
}
##' auxiliar function for isolate ratio
##' 49
##' @param ratioIso  ir variable
##' @param st    string representing the identifier
##' @param C_RatioIso IR categories
##' @return
##' @noRd
##' @author Miguel Sautié (PIAAS)
##' @examples
##'
ratisof<- function(ratioIso,st, C_RatioIso) {
  g<-1
  while (g<length(C_RatioIso))
  {
    if (grepl(C_RatioIso[g], st, ignore.case = TRUE)) {
      if (!grepl(C_RatioIso[g], ratioIso, ignore.case = TRUE))  {
        ratioIso<-joinNames(ratioIso, C_RatioIso[g])
      }
    }
    g<-g+1
  }
  return(ratioIso)
}

##' Case table. The output of this function is not affected by the absence of 4007 for a given case.
##' 50
##' @param d     dataframe Omnilab
##' @param cgenes vector-library for genes
##' @param chemo  vector of hemolyse status
##' @param st    string representing the identifier
##' @return      new dataframe
##' @noRd
##' @author Miguel Sautié (PIAAS)
##' @examples
##'
CasDF2<- function(newdf,d, cgenes, chemo) {
  unr<-unique(d$NR)
  u<-1
  while(u<length(unr)){
    i<-2
    w<-1
    cc<-1
    hemoI<-""
    geneI<-""
    ratioI<-""
    isolatesh<-c()
    isolatesg<-c()
    pp<-0
    n<-0
    c<-0
    while(i<nrow(d)){

      if (!is.na(d[i, "NR"])&&(!is.na(d[i-1, "NR"]))&&(!is.na(unr[u]))&&(unr[u]==d[i, "NR"])&&(d[i, "NR"]==d[i-1, "NR"]))
      { w<-i
      n<-0
      hemoI<-""
      geneI<-""
      ratioI<-""
      isolatesh<-c()
      isolatesg<-c()
      if (((d[w-1, "Résultat"]!="NÉG")&&(d[w-1, "Code.d.analyse"]=="4007") )||((d[w, "Résultat"]!="NÉG")&&(d[w, "Code.d.analyse"]=="4007") )) { #at least one sample linked to 4007 must be positive
              geneI<-genef(geneI,d[w-1, "Résultat"], cgenes)
              geneI<-genef(geneI,d[w, "Résultat"], cgenes)
      }
      if ((d[w-1, "Code.d.analyse"]=="4001")||(d[w-1, "Code.d.analyse"]=="4003")||(d[w-1, "Code.d.analyse"]=="4005"))
      { hemoI<-hemof(hemoI,d[w-1, "Résultat"], chemo)
      isolatesh<-c(isolatesh, d[w-1, "Code.d.analyse"]) }
      else if ((d[w-1, "Code.d.analyse"]=="4000")||(d[w-1, "Code.d.analyse"]=="4002")||(d[w-1, "Code.d.analyse"]=="4004"))
      {geneI<-genef(geneI,d[w-1, "Résultat"], cgenes)
      isolatesg<-c(isolatesg, d[w-1, "Code.d.analyse"])}
      ratioI<-ratisof(ratioI,d[w-1, "Résultat"], C_RatioIso)
      while (d[w, "NR"]==d[w-1, "NR"])      {
            if ((d[w, "Code.d.analyse"]=="4001")||(d[w, "Code.d.analyse"]=="4003")||(d[w, "Code.d.analyse"]=="4005"))
            { hemoI<-hemof(hemoI,d[w, "Résultat"], chemo)
            isolatesh<-c(isolatesh, d[w, "Code.d.analyse"]) }
             else if ((d[w, "Code.d.analyse"]=="4000")||(d[w, "Code.d.analyse"]=="4002")||(d[w, "Code.d.analyse"]=="4004"))
             {geneI<-genef(geneI,d[w, "Résultat"], cgenes)
             isolatesg<-c(isolatesg, d[w, "Code.d.analyse"]) }
        ratioI<-ratisof(ratioI,d[w, "Résultat"], C_RatioIso)
        if (((d[w, "Résultat"]!="NÉG")&&(d[w, "Code.d.analyse"]=="4007") )) { #at least one sample linked to 4007 must be positive
               geneI<-genef(geneI,d[w, "Résultat"], cgenes)
               n<-1
        }
        if ((n==0)&&((nchar(geneI)>0)||(nchar(hemoI)>0))){  #It was assumed that if there is no 4007 and at least one gene or hemo result then it is a positive case.
          n<-1
          ratioI<-ratisof(ratioI,d[w, "Résultat"], C_RatioIso)
        }
        w<-w+1
        pp<-pp+1
      }
        pp<-pp+1

        break }
       else if (!is.na(d[i, "NR"])&&(!is.na(d[i-1, "NR"]))&&(!is.na(unr[u]))&&(unr[u]==d[i, "NR"])&&(d[i, "NR"]!=d[i-1, "NR"])) {

         if (((d[i, "Résultat"]=="NÉG")&&(d[i, "Code.d.analyse"]=="4007") )) {
           n<-0
           c<-i
           #print(paste0(unr[u], " " ,as.character(n), " ",as.character(c), " ", as.character(pp)))
           break
         }
      }
       i<-i+pp+1
    }
    lg<-length(unique(isolatesg))
    lh<-length(unique(isolatesh))
    if (lh < lg) {
      newdf[u, "Niso"]<-lg
      newdf[u, "Niso2"]<-lh
    } else {
      newdf[u, "Niso"]<-lh
      newdf[u, "Niso2"]<-lg
    }
    newdf[u, "NR"]<-unr[u]  #
    newdf[u, "Isoratio"]<- ratioI
    ##newdf[u, "rows"]<-pp
    newdf[u, "genes"]<-geneI
    newdf[u, "hemolyse"]<-hemoI
    if (n==1)  {
      newdf[u, "vcas"]<-TRUE
    } else {
      newdf[u, "vcas"]<-FALSE
    }

    u<-u+1
  }
return (newdf)
}

   ##'@description
  ##' isolate table construction 2
  ##' 51
  ##' @param fOmniL2    dataframe Omnilab
  ##' @return new dataframe
  ##' @export
  ##' @author Miguel Sautié PIAAS
  ##' @examples
  ##'  bdf<-IsolatesDF2(df)
  IsolatesDF2<- function(fOmniL2) {
    d<-fOmniL2[fOmniL2$Résultat != "NÉG", ]
    d<-d[d$Code.d.analyse != "4007", ]
    d<-rmNA(d)
    newdf <- data.frame(isolates=(character()),
                        stringsAsFactors=FALSE)
    i<-2
    isolates<-c()
    ii<-0
    cc<-0
    ff<-0
    while (i<nrow(d)) {
      if ((!is.na(d[i, "NR"]))&&(!is.na(d[i-1, "NR"]))&&(d[i, "NR"]==d[i-1, "NR"])){
        ii<-i
        arr<-numeric(3)
        arr1<-numeric(3)
        cp<-0
        cp1<-0
        a<-0
        b<-0
        bb<-0
        dd<-0
        dd2<-0
        dd3<-0
        cc<-cc+1
        isolate<-c()
        ff<-0
        while ((!is.na(d[ii, "NR"]))&&(!is.na(d[ii-1, "NR"]))&&(d[ii, "NR"]==d[ii-1, "NR"])) {
          if (d[ii, "Nom.de.l.animal"]==d[ii-1, "Nom.de.l.animal"]) {
            if ((d[ii, "Code.d.analyse"]=="4000")&&(a==0))
            {a<-1
            arr[1]<-ii
            cp<-1
            }
            if ((d[ii, "Code.d.analyse"]=="4002")&&(b==0))
            {b<-1
            arr[2]<-ii
            cp<-2
            }
            if ((d[ii, "Code.d.analyse"]=="4004")&&(bb==0))
            {bb<-1
            arr[3]<-ii
            cp<-3
            }
            if ((d[ii, "Code.d.analyse"]=="4001")&&(dd==0))
            {dd<-1
            arr1[1]<-ii
            cp1<-1
            }
            if ((d[ii, "Code.d.analyse"]=="4003")&&(dd2==0))
            {dd2<-1
            arr1[2]<-ii
            cp1<-2
            }
            if ((d[ii, "Code.d.analyse"]=="4005")&&(dd3==0))
            {dd3<-1
            arr1[3]<-ii
            cp1<-3
            }
          }
          if (d[ii, "Nom.de.l.animal"]!=d[ii-1, "Nom.de.l.animal"])  {
            if (cp>cp1){
              isolate <-arr[arr>0]
            }
            else {
              isolate<-arr1[arr1>0]
            }
            if (cc==1) {isolates<-d[isolate[1], "NR"]}
            else {for (u in 1:length(isolate)) {
              isolates<-c(isolates, d[isolate[u], "NR"])
            }
            }
            arr<-numeric(3)
            arr1<-numeric(3)
            cc<-cc+1
          }
          ii<-ii+1
          ff<-ff+1
        }
          if ((!is.na(d[ii, "NR"]))&&(!is.na(d[ii-1, "NR"]))&&(d[ii, "NR"]!=d[ii-1, "NR"]))  {
           if (cp>cp1){
            isolate <-arr[arr>0]
           }
         else {
           isolate<-arr1[arr1>0]
        }
          if (cc==1) {isolates<-d[isolate[1], "NR"]}
         else {for (u in 1:length(isolate)) {
           isolates<-c(isolates, d[isolate[u], "NR"])
          }
         }
          arr<-numeric(3)
          arr1<-numeric(3)
         cc<-cc+1
         }
      }
      i<-ff+i+1
    }
    isolates <- isolates[!is.na(isolates)]
    newdf <- data.frame(isolates)
    return(newdf)
  }
  ##'@description
  ##' removes ratios of isolates from the name
  ##' 52
  ##' @param fOmniL2    string containing virotype or hemolysis outcome
  ##' @return           substring containing virotype or hemolysis outcome without isolate ratio
  ##' @export
  ##' @author Miguel Sautié (PIAAS)
  ##' @examples
  ##'  bdf<-isolateGenotype(df)
  isolateGenotype <-function(str) {
    subo<-substr(str,1,unlist(gregexpr("\\(", str))-1) #consider isolate ratio with parenthesis
    if (length(subo)==1){                              # whether there is not parenthesis
      subo<-substr(str,1,unlist(gregexpr("/3", str))-1) # consider isolate ratio without parenthesis
      if (length(subo)==1) {
        subo<-substr(str,1,nchar(str)-1)
      }
    }
    return (subo)
  }
  ##'Sample counting 2
  ##'The output of this function is not affected by the absence of 4007 for a given sample.
  ##'53
  ##' @param df       dataframe Omnilab
  ##' @return count
  ##' @export
  ##' @author Miguel Sautié (PIAAS)
  ##' @examples
  ##'  nc<-samplecount2(fOmniL)
  samplecount2<-function(d) {
    #this function must be tested
    i<-2
    w<-1
    pp<-0
    s<-0
    c<-0
    echan<-c()
    while(i<nrow(d)){
      pp<-0
      echan<-c()
      if (!is.na(d[i, "NR"])&&(!is.na(d[i-1, "NR"]))&&(d[i, "NR"]==d[i-1, "NR"]))
      { w<-i
      while (d[w, "NR"]==d[w-1, "NR"])
      {
        if (pp==0) {
          echan<-fOmniL2[w-1,"Nom.de.l.animal"]
          echan<-c(echan, fOmniL2[w,"Nom.de.l.animal"])
        }
        else {
          echan<-c(echan, fOmniL2[w,"Nom.de.l.animal"])
        }
        w<-w+1
        pp<-pp+1
      }
      }
      else {
        s<-s+1
      }
      if (pp!=0) {
      s<-s+length(unique(echan))
      }
      i<-i+pp+1
    }
    return (s)
  }

