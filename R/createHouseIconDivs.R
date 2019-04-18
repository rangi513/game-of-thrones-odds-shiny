createHouseIconDivs <- function(houseList){
  houseIcons <- list()
  for(name in houseList){
    listOfImages <- list.files("www/")
    possibleImage <- listOfImages[str_detect(listOfImages, str_replace_all(name, "[0-9]", ""))]
    if(length(possibleImage) > 0){
      # If a match exists, arbitrarily select the first one
      theImage <- possibleImage[1]
    }else{
      theImage <- "got-icon3.png"
    }
    thisHouseIcon <- list(fluidRow(
      column(12, 
             align = "center",
             img(src=theImage, 
                 width= "80px"
             ), 
             # HTML(
               p(str_to_title(str_replace_all(name, "_"," "))
               ))))
    houseIcons <- c(houseIcons, thisHouseIcon)
  }
  return(houseIcons)
}
