########################################################################

########################################################################

source("ParseInfoFunctions.R")

Groupid <- integer(length(infolist))
Gender <- character(length(infolist))
Numid <- integer(length(infolist))
Wechat <- character(length(infolist))
Age <- character(length(infolist))
City <- character(length(infolist))
Job <- character(length(infolist))
Height <- character(length(infolist))


for (i in 1:length(infolist)) {
  info <- infolist[[i]]
  
  tempret <- ParseID(infolist[[i]])
  Groupid[i] <- as.integer(tempret[[1]])
  Gender[i] <- tempret[[3]]
  Numid[i] <- as.integer(tempret[[2]])
  Wechat[i] <- FindWechat(info)
  Age[i] <- FindAge(info)
  City[i] <- FindCity(info)
  Job[i] <- FindJob(info)
  Height[i] <- FindHeight(info)
}

femaleid <- Gender == "F>>M"
maleid <- Gender == "M>>F"

remove(i, info, tempret)
