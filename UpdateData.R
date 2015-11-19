########################################################################
htmllist <- list.files("html/", pattern="nlxj[0-9]+.html$",
                       full.names=T)
maxgroup <- max(Groupid)
htmlnum <- gsub(".*nlxj([0-9]+).*", "\\1", htmllist)
htmlnum <- as.numeric(htmlnum)
maxhtml <- max(htmlnum)

newfilenum <- setdiff(htmlnum, unique(Groupid))

if (length(newfilenum)==0) {
  print("all updated.")
} else if (length(newfilenum)>0) {
  print("yes")
  for (i in which(htmlnum %in% newfilenum)) {
    print(htmllist[i])
    rawinfo <- ParseHTML(htmllist[i])
    infolist <- c(infolist, ParseList(rawinfo))
  }

}
