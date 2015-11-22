########################################################################

########################################################################
ParseHTML <- function(filename) {
  # Parse the HTML file, find the line contain the informations.
  # Return a list of strings??? corresponding to the <p> tags, which
  # contains informations of many people
  # The title of page is saved as the top of the list.
  # The link to the image is stripped from <img> tag.
  
  con <- file(filename, open="r", encoding="utf-8")
  line <- readLines(con,n=1)
  while (length(line)!= 0) {
    idx <- grep('<title>', line)
    if (length(idx) > 0) {
      title <- gsub(".*<title>(.*)</title>", "\\1", line)
    }
    idx <-
      grep('<div class=\"rich_media_content[ ]*\" id=\"js_content\">',
           line)
    line <- readLines(con, n=1)
    if (length(idx) == 0) next
    else break
  }
  close(con)
  
  
  # Split the line by "-----"
  rawinfo <- as.list(strsplit(line, "-{5,}")[[1]])
  # Drop the last one in the list (ad of the platform)
  rawinfo[[length(rawinfo)]] <- NULL  
  for (i in 1:length(rawinfo)) {
    # Split the line by "</p>" tags
    rawinfo[[i]] <- strsplit(rawinfo[[i]], "</p>")[[1]]
    # Delete "<p ...>" tags
    rawinfo[[i]] <- gsub("<p[^<]*?>", "", rawinfo[[i]])
    # Replace "<br ...>" tags with spaces
    rawinfo[[i]] <- gsub("<br[^<]* />", " ", rawinfo[[i]])
    # Strip the urls of the image from "<img ...>" tags
    rawinfo[[i]] <- gsub("<img.*src=\"(http://[^ ]*)\"[^<]*>", "\\1",
                         rawinfo[[i]])
    # Delete "<em ...>" and "</em>" tags
    rawinfo[[i]] <- gsub("<em[^<]*>", "", rawinfo[[i]])
    rawinfo[[i]] <- gsub("</em>", "", rawinfo[[i]])
    # Delete "<span ...>" and "</span>" tags
    rawinfo[[i]] <- gsub("<span[^<]*>", "", rawinfo[[i]])
    rawinfo[[i]] <- gsub("</span>", "", rawinfo[[i]])
    # Delete empty strings
    rawinfo[[i]] <- rawinfo[[i]][!rawinfo[[i]]==""]
    rawinfo[[i]] <- c(title, rawinfo[[i]])
    # Delete &nbsp;
    rawinfo[[i]] <- gsub("&nbsp;", "", rawinfo[[i]])
    rawinfo[[i]] <- gsub("&amp;", "&", rawinfo[[i]])
    # Delete spaces in beginning and end
    rawinfo[[i]] <- gsub("^ +", "", rawinfo[[i]])
    rawinfo[[i]] <- gsub(" +$", "", rawinfo[[i]])
  }
  return(rawinfo)
}


########################################################################
# Scan the HTML files, parse the individuals into a list
htmllist <- list.files("html/", pattern="nlxj[0-9]+.html$",
                       full.names=T)
infolist <- list()
for (i in 1:length(htmllist)) {
  print(htmllist[i])
  rawinfo <- ParseHTML(htmllist[i])
  infolist <- c(infolist, rawinfo)
}

remove(i, rawinfo)
