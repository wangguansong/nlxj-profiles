# Functions

########################################################################
ParseID <- function(info) {
  # Find the group number, ID number and gender of each record.
  # Return:
  #   A list of [group number, id number, gender]
  
  # id number: from the second string in the list
  words <- info[2]
  if (length(grep("(男找男)|(女找女)", words))>0) {   # LGBT issue 16
    words <- info[3]
  }
  numid <- gsub("[^0-9]*([0-9]+)[^0-9]*", "\\1", words)
  
  # group number: the number in the title string
  words <- info[1]
  groupid <- gsub("[^0-9]*([0-9]+)[^0-9]*", "\\1", words)
  
  # gender: from the title string
  if (length(grep("女生|女士", words)) > 0) {
    gender <- "F>>M"
  } else if (length(grep("男生|男士", words)) > 0) {
    gender <- "M>>F"
  } else if (length(grep("LGBT", words)) > 0) {
    if ((numid)<6) {
      gender <- "F>>F"
    } else {
      gender <- "M>>M"
    }
  } else {
    gender <- NA
  }
  
  return(list(groupid, numid, gender))
}

########################################################################
FindAge <- function(info) {
  # Parse age
  # Return:
  #   A string representing birth year, or NA
  regkey <-
    paste("^ *",
          "(年龄|年齡|年  龄|出生年月|芳龄|年纪|年您|出生日期|生日|年轮)",
          "[ ：:;]+", "(.*) *",
          sep="")
  # Find the age item, and get the number/string
  rowid <- grep(regkey, info)
  age <- gsub(regkey, "\\2", info[rowid])
  if (length(age)==0) return(NA)
  # Remove spaces.
  age <- gsub("^ *", "", age)
  age <- gsub(" *$", "", age)
  # Parse the year
  if (length(grep(".*(19[7-9][0-9]).*", age))>0) {
    # If the year number yyyy shows up
    age <- gsub(".*(19[7-9][0-9]).*", "\\1", age)
  } else if (length(grep(".*[7-9][0-9].*", age))>0) {
    # If the year number yy shows up
    age <- paste(19,
                 gsub(".*([7-9][0-9]).*", "\\1", age),
                 sep="")
  } else if (length(grep("[2-4][0-9]", age))>0) {
    # Issue 119 was on 20150101
    groupid <- as.numeric(gsub(".*([0-9]+).*", "\\1", info[1]))
    age <- gsub(".*([2-4][0-9]).*", "\\1", age)
    if (groupid<199) {
      age <- as.character(2014 - as.numeric(age))
    } else {
      age <- as.character(2015 - as.numeric(age))
    }
  } else if (length(grep("马年", age))>0) {
    age <- "1990"
  } else if (length(grep("蛇", age))>0) {
    age <- "1989"
  } else if (length(grep("狮子猴~", age))>0) {
    age <- "1992"
  } else {
    age <- NA
  }
  return(age)
}
########################################################################
FindWechat <- function(info) {
  regkey <- "^ *(微信号|微信|微信名|微 信 号|微信號|信号)[ ：:;]+(.*) *"
  rowid <- grep(regkey, info)
  wechat <- gsub(regkey, "\\2", info[rowid])
  if (length(wechat)==0) return(NA)
  wechat <- gsub("^ *", "", wechat)
  wechat <- gsub(" *$", "", wechat)
  wechat <- gsub("\\(.*\\)", "", wechat)
  return(wechat)
}
########################################################################
FindCity <- function(info) {
  regkey <-
    paste("^ *",
          "(城市|坐标|位置|居住地|地标|所在地|工作城市|地址|工作地方|",
          "座标|城  市|现居住地|地域|地点|籍贯|籍贯及现居地|所在城市|",
          "地区|生活城市)",
          "[ ：:;，]+(.*) *",
          sep="")
  rowid <- grep(regkey, info)
  city <- gsub(regkey, "\\2", info[rowid])
  if (length(city)==0) return(NA)

  # assume what's inside a bracket is not important.
  city <- gsub("[\\（\\(].+[\\）\\)]", "", city)
  
  # Special values
  if (city=="魔都") city <- "上海"
  if (length(grep("学校在重庆", city))==1) city <- "重庆"
  if (length(grep("开车塞成狗的广州", city))==1) city <- "广州"
  if (length(grep("家在西安", city))==1) city <- "西安"
  if (length(grep("武汉生长&暂居", city))==1) city <- "武汉"
  
  # phrases
  city <- gsub(".*(人|妹子)在([^ ]+)", "\\2", city)
  city <- gsub(".*在(.+)(定居|打拼|生活|工作)*的(.+)(人|姑娘|妞|妹子|菇凉|MM)",
               "\\1", city)
  
  city <- gsub(".*现居([^ ,，]+).*", "\\1", city)
  city <- gsub("市$", "", city)
  
  # Major cities
  city <- gsub("^北京.*", "北京", city)
  city <- gsub("^上海.*", "上海", city)
  city <- gsub("^南京.*", "南京", city)
  
  # get rid of space, comma, period, etc
  city <- gsub("^ *", "", city)
  city <- gsub(" *$", "", city)
  city <- gsub("[；。]$", "", city)
  city <- gsub("(\t *)+", "", city)

  return(city)
}
########################################################################
FindJob <- function(info) {
  regkey <-
    paste("^ *(职业|工作性质|職業|职  业|职位|工作|职务|工作|饭碗|职责|行业)",
          "[ ：:;]+(.*) *",
          sep="")
  rowid <- grep(regkey, info)
  job <- gsub(regkey, "\\2", info[rowid])
  if (length(job)==0) return(NA)
  job <- gsub("^ *", "", job)
  job <- gsub(" *$", "", job)
  return(job)
}
########################################################################
FindHeight <- function(info) {
  rowid <- grep("^ *(身高|体型|身型|身高/体重|身  高|净身高)",
                "[ ：:;]+.*[0-9]+.*", info)
  height <- gsub("^ *(身高|体型|身型|身高/体重|身  高|净身高)[ ：:;]+(.*) *",
                 "\\2", info[rowid])
  if (length(height)==0) return(NA)
  height <- gsub("^ *", "", height)
  height <- gsub(" *$", "", height)
  if (length(grep("[0-9]{3}(\\.[0-9])?[^0-9]* *(cm|CM)", height))>0) {
    height <- gsub("[^0-9]*([0-9]{3}(\\.[0-9])?)[^0-9]* *(cm|CM).*", "\\1", height)
  } else if (length(grep("[^0-9]*1\\.[0-9]{2}", height))>0) {
    height <- gsub("[^0-9]*(1\\.[0-9]{2}).*", "\\1", height)
    height <- gsub("\\.", "", height)
  } else if (length(grep("[^0-9]*[0-9]{3}(\\.[0-9])?[^0-9]*.*", height))>0) {
    height <- gsub("[^0-9]*([0-9]{3}(\\.[0-9])?)[^0-9]*.*", "\\1", height)
  } else if (length(grep("√3", height))>0) {
    height <- "173"
  }
  if (as.numeric(height)>300) return(NA)
  return(height)
}

