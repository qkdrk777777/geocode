#' geocode by using selenium and googlemaps
#'
#' @param name region list(unique list ; if name is not unique then change unique).
#' @param plot browser open or not.
#' @param n is sleep time.
#' @examples
#' geocode(name=c('daegu','daegu','Gyeongju','Daegu University'),plot=F)
#' @return
#' @export
geocode=function(name,port1=4565L,port2=4566L,port3=4567L,n=3,plot=F){
  if(!require('devtools'))install.packages('devtools')
  library(devtools)
  if (!require("RSelenium")) {
    install_version("binman", version = "0.1.0", repos = "https://cran.uni-muenster.de/")
    install_version("wdman", version = "0.2.2", repos = "https://cran.uni-muenster.de/")
    install_version("RSelenium", version = "1.7.1", repos = "https://cran.uni-muenster.de/")
  }
  library(RSelenium)
  name=unique(name)
  suppressMessages(install_github('qkdrk777777/kma2'))
  library(kma2)

  pack2(c("rvest", "httr", "stringr", "RCurl", "XML", "progress"))
  tryCatch({
    pJS <<- wdman::phantomjs(port = port1)
    if(plot==T){
      eCaps <<- list(chromeOptions = list(prefs = list(profile.default_content_settings.popups = port2, download.prompt_for_download = FALSE)))
      rD <<- rsDriver(extraCapabilities = eCaps,port=port3)
      remDr <<- rD$client}else {remDr <<- remoteDriver(port=port1, browserName = 'chrome')}
  },error=function(e){

    try(silent = T,rD$server$stop())
    try(silent = T,remDr$close())
    try(silent = T,pJS$stop())
    pJS <<- wdman::phantomjs(port = port1)
    if(plot==T){
      eCaps <<- list(chromeOptions = list(prefs = list(profile.default_content_settings.popups = port2, download.prompt_for_download = FALSE)))
      rD <<- rsDriver(extraCapabilities = eCaps,port=port3)
      remDr <<- rD$client}else {remDr <<- remoteDriver(port=port1, browserName = 'chrome')}

  })
  tryCatch({
    url='https://www.google.com/maps'
    if(plot==F)remDr$open()

    remDr$navigate(url)
    Sys.sleep(n)

    search=NULL
    while(length(search)==0){
      assign('search',remDr$findElement(using='css selector',value='input#searchboxinput.tactile-searchbox-input'))
    }
  },error=function(e){
    Sys.sleep(n)
    search=NULL
    while(length(search)==0){
      assign('search',remDr$findElement(using='css selector',value='input#searchboxinput.tactile-searchbox-input'))
    }
  })

  data=NULL;data2=NULL
  for(i in 1:length(name)){
    search$clearElement()
    search$sendKeysToElement(list(name[i],key='enter'))
    Sys.sleep(n)
    lonlat=as.numeric(str_split(substr(remDr$getCurrentUrl()[[1]],
                                       regexpr('@',remDr$getCurrentUrl()[[1]])+1,regexpr(',[0-9]+z',remDr$getCurrentUrl()[[1]])-1),',')[[1]])
    if(i>1){
      if((data[nrow(data),'lon']==lonlat[2])&(data[nrow(data),'lat']==lonlat[1])){lonlat=c(NA,NA)}}
    ifelse(sum(is.na(lonlat))>=1,
           data2<-rbind(data2,data.frame(name=name[i],lat=lonlat[1],lon=lonlat[2]))
           ,data<-rbind(data,data.frame(name=name[i],lat=lonlat[1],lon=lonlat[2]))
    )
  }
  try(silent = T,rD$server$stop())
  try(silent = T,remDr$close())
  try(silent = T,pJS$stop())
  if(nrow(data2)!=0){
    message('please check data2')}
  return(list(list=data,check=data2))
}

#devtools::document()
