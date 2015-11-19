########################################################################

DownloadImage <- function(start, end) {
  # Download images
  # Args:
  #   start, end    indices of infolist that needs downloading
  
  for (i in start:end) {
    
    imageid <- grep("http://", infolist[[i]])
    if (length(imageid)==1) {
      download.file(infolist[[i]][imageid],
                    paste("pic/", Groupid[i], "_", Numid[i], sep=""))
    } else if (length(imageid)>1) {
      for (j in 1:length(imageid)) {
        download.file(infolist[[i]][imageid[j]],
                      paste("pic/", Groupid[i], "_", Numid[i], "_", j,
                            sep=""))
      }
    }
  }
}
