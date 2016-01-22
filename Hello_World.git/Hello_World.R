# Testing version control with BitHub repository
# Jadon Thomson
# 2016-01-22

HW <- "Hello World"
len <- nchar(HW)
for (i in 1:len){
  print(as.character(substr(HW, i, i)))     #splitting up the characters
}

#reverse text
WH <- ""
for (i in 1:len){
  WH <- paste0(WH, substr(HW, len + 1 - i, len + 1 - i))
}