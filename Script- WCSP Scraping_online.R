#Le 10/04/2019
# /!\ The script is not completely clean. It works but some lines are supplementary and should not be ran.
# So, be careful of what you launch, to the risk of losing the previous work.
# Do not forget to change the family you aim to extract (line 45, via changing the position of the family in the scroll down menu (here 149))
# An update is planned for september as the later.

if(!require(RSelenium)) {install.packages("RSelenium") ; library(RSelenium)}
if(!require(rvest)){install.packages("rvest") ; library(rvest)}
if(!require(xml2)){install.packages("xml2") ; library(xml2)}
if(!require(tidyverse)){install.packages("tidyverse") ; library(tidyverse)}
if(!require(stringr)){install.packages("stringr") ; library(stringr)}
if(!require(data.table)){install.packages("data.table") ; library(data.table)}
if(!require(crayon)){install.packages("crayon") ; library(crayon)}

'%!in%' <- function(x,y)!('%in%'(x,y))

## FOR WINDOWS (I never tried)
#https://grishagin.com/r/rselenium/2017/11/11/setup-rselenium-windows10.html
#https://community.rstudio.com/t/setting-up-rselenium/11622

## FOR LINUX
#Use Docker : https://hub.docker.com/u/wernight

#For using HEADLESS FIREFOX >
#sudo docker run -d -p 4444:4444 -v /dev/shm:/dev/shm selenium/standalone-chrome:3.141.59-mercury
remDr <- remoteDriver(browserName = 'phantomjs', port = 8910L)
remDr$open()


#For using PHANTOMJS >
#https://hub.docker.com/r/wernight/phantomjs/
#In the terminal : sudo docker run -d -p 8910:8910 wernight/phantomjs phantomjs --webdriver=8910
remDr <- remoteDriver(browserName = 'firefox', port = 4444L) # or pJS <- wdman::phantomjs()
remDr$open()



remDr$navigate("http://www.google.com/ncr")

remDr$screenshot(display = T)

## List of genera ####
# Extract the list of genera which will be used to browse the website later
remDr$navigate("http://wcsp.science.kew.org/reportbuilder.do?method=Reset") #Entering our URL gets the browser to navigate to the page
element<- remDr$findElement(using = 'css selector', "#family > option:nth-child(149)")
element$clickElement()

## Only First initialisation /!\
#sp.dist <- data.frame(matrix(NA, ncol = 5, nrow = 0), stringsAsFactors = F)
#names(sp.dist) <- c("family","species","dist.lvl2","dist.lvl3","lifeform")
#nb.accnames <- 0
#scraping.summary <- data.frame(matrix(NA, ncol = 5, nrow = 0), stringsAsFactors = F)
#names(scraping.summary) <- c("genera", "nb.names", "nb.accnames.tmp","nb.accnames","lengthsp.dist","error.status")

genera <- xml2::read_html(remDr$getPageSource()[[1]]) %>%
  rvest::html_node("#genus") %>%
  html_children() %>%
  html_text() %>%
  unlist %>%
  unique
genera <- genera[!grepl("all genera",genera)]
#fwrite(data.table(as.list(sort(genera))), "Data/genera_list.csv", col.names = F)

remDr$screenshot(display = TRUE) #This will take a screenshot and display it in the RStudio viewer

sp.dist <- fread("Data/WCSP_species_distribution.csv")
nb.accnames <- unname(unlist(fread("Data/WCSP_nbaccnames.csv")[1,1]))
scraping.summary <- fread("Data/WCSP_summary.csv")

alreadydone.genera <- scraping.summary$genera %>% 
  unique()
last.genera <- alreadydone.genera[length(alreadydone.genera)]

genera <- genera[match(last.genera,genera)+1:length(genera)] #Genres du dernier traité + 1 jusqu'au dernier de la liste

remDr$closeall
remDr$open(silent = T)

## Distribution extraction ####
{for (k in genera) {
    remDr$navigate("http://wcsp.science.kew.org/home.do")
    #Ok for the cookies..
    element <- try(remDr$findElement(using = "css", "a.eupopup-button:nth-child(1)", retry = ), silent = T)
    if('try-error' %!in% class(element)) element$clickElement()
    
    element <- remDr$findElement(using = "xpath", "//*[@id='plantName']")
    element$sendKeysToElement(list(as.character(k)))
    element <- remDr$findElement(using = "css", ".btn")
    element$clickElement()
    
    blob <- xml2::read_html(remDr$getPageSource()[[1]])
    
    if(identical(html_text(html_nodes(blob, "#main > p > b")),character(0)) == F) {    #Si il y a plusieurs résultats, alors ... :
      
      nb.accnames.tmp <- blob %>%  #To see if every accepted names have been extracted
        rvest::html_nodes(xpath = "/html/body/div[1]/div[2]/main/b") %>%
        html_node('p') %>%
        dplyr::tibble(species = .) %>%
        nrow()
      
      nb.accnames <- nb.accnames + nb.accnames.tmp
      
      currentlist <- blob %>%
        rvest::html_nodes(css = ".onwardnav") %>%
        html_attr('href') %>%
        sprintf("http://wcsp.science.kew.org%s",.) %>%
        dplyr::tibble(links = .)
      
      n <- currentlist %>% nrow
      m <- 0
      
      for(l in currentlist$links) {
        
        m <- m + 1
        
        remDr$navigate(as.character(l))
        page.tmp <- xml2::read_html(remDr$getPageSource()[[1]])  
        
        name.status <- page.tmp %>%
          rvest::html_node(".container-fluid > p:nth-child(2)") %>%
          rvest::html_text()
        
        sp.tmp <- page.tmp %>%
          rvest::html_node('.plantname') %>%
          rvest::html_text()
        
        if(grepl("accepted",name.status)) {
          
          #Sometimes the html code is not well downloaded. It allows to try again, until the needed information is available
          
          table.tmp <- page.tmp %>%
            rvest::html_node('#main > div > table') %>%
            rvest::html_table(trim = T, fill = T)
          names(table.tmp) <- c("char","value")
          
          
          lifeform.tmp <- ifelse(any(grepl("Lifeform", table.tmp$char)),table.tmp[table.tmp$char == "Lifeform:",2], NA)
          family.tmp <- ifelse(any(grepl("Family", table.tmp$char)),table.tmp[table.tmp$char == "Family:",2], NA)
          country.lvl3.tmp <- ifelse(any(grepl("Distribution", table.tmp$char)),
                                     paste(unique(unlist(str_extract_all(table.tmp[table.tmp$char == "Distribution:",2],"[A-Z][A-Z][A-Z]"))), collapse = ", "),
                                     NA)
          country.lvl2.tmp <- ifelse(any(grepl("Distribution", table.tmp$char)),
                                     paste(unique(unlist(str_extract_all(table.tmp[table.tmp$char == "Distribution:",2],"[0-9][0-9]"))), collapse = ", "),
                                     NA)
          
          sp.dist <- rbind(sp.dist,
                           data.frame(family = family.tmp, species = sp.tmp, dist.lvl2 = country.lvl2.tmp, dist.lvl3 = country.lvl3.tmp, lifeform = lifeform.tmp))
          
          cat(m, "/", n, " - ", style(sp.tmp, as = "deepskyblue1"), "\n", sep = "")
        }
        else  {
          cat(m, "/", n, " - ", style(sp.tmp, as = "red"), "\n", sep = "")
          remDr$closeServer()
          }
        }
      }
    else {
        page.tmp <- blob
        
        name.status <- page.tmp %>%
          rvest::html_node(".container-fluid > p:nth-child(2)") %>%
          rvest::html_text()
        
        sp.tmp <- page.tmp %>%
          rvest::html_node('.plantname') %>%
          rvest::html_text()
        
        if(grepl("accepted",name.status)) {
          
          table.tmp <- page.tmp %>%
            rvest::html_node('#main > div > table') %>%
            rvest::html_table(trim = T, fill = T)
          names(table.tmp) <- c("char","value")
          
          lifeform.tmp <- ifelse(any(grepl("Lifeform", table.tmp$char)),table.tmp[table.tmp$char == "Lifeform:",2], NA)
          family.tmp <- ifelse(any(grepl("Family", table.tmp$char)),table.tmp[table.tmp$char == "Family:",2], NA)
          country.lvl3.tmp <- ifelse(any(grepl("Distribution", table.tmp$char)),
                                     paste(unique(unlist(str_extract_all(table.tmp[table.tmp$char == "Distribution:",2],"[A-Z][A-Z][A-Z]"))), collapse = ", "),
                                     NA)
          country.lvl2.tmp <- ifelse(any(grepl("Distribution", table.tmp$char)),
                                     paste(unique(unlist(str_extract_all(table.tmp[table.tmp$char == "Distribution:",2],"[0-9][0-9]"))), collapse = ", "),
                                     NA)
          
          sp.dist <- rbind(sp.dist,
                           data.frame(family = family.tmp, species = sp.tmp, dist.lvl2 = country.lvl2.tmp, dist.lvl3 = country.lvl3.tmp, lifeform = lifeform.tmp))
          
          cat(m, "/", n, " - ", style(sp.tmp, as = "deepskyblue1"), "\n", sep = "")
        }
        else  cat(m, "/", n, " - ", style(sp.tmp, as = "red"), "\n", sep = "")
        remDr$closeServer()
      }
    summary.tmp <- data.frame(genera = as.character(k), nb.names = n, nb.accnames.tmp = nb.accnames.tmp, nb.accnames = nb.accnames, lengthsp.dist = nrow(sp.dist), error.status = ifelse(nb.accnames == nrow(sp.dist), "OK", "ERROR"), stringsAsFactors = F)
    scraping.summary <- rbind.data.frame(scraping.summary,summary.tmp, stringsAsFactors = F)
    fwrite(scraping.summary, "Data/WCSP_summary.csv")
    fwrite(sp.dist,"Data/WCSP_species_distribution.csv")
    fwrite(as.list(nb.accnames),"Data/WCSP_nbaccnames.csv")
    Sys.sleep(2)
    
  }
}

wcsp.data <- fread("Data/WCSP_species_distribution.csv")
wcsp.data$species <- str_remove_all(wcsp.data$species, "× ")
wcsp.data$Taxon <- wcsp.data$species %>% 
  str_extract_all("(^[A-Z][a-z]+ [a-z]+-[a-z]+ [a-z]+\\. [a-z]+)|(^[A-Z][a-z]+ [a-z]+-[a-z]+)|
                  (^[A-Z][a-z]+ [a-z]+ [a-z]+\\. [a-z]+)|(^[A-Z][a-z]+ [a-z]+)|(^[A-Z][a-z]+ )") %>%
  str_remove_all("[a-z]+\\. ")
taxa <- wcsp.data$Taxon %>% unique
fwrite(unname(as.data.frame(taxa)), "taxa_WCSP.csv")
