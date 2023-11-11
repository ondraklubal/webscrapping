rm(list = ls())

library(RSelenium)
library(data.table)
library(rvest)
library(selectr)
library(stringr)
library(stringi)
library(rebus)
library(XML)
library(plyr)

firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

# vzor URL
# https://auto.bazos.cz/?hledat=%C5%A1koda&rubriky=auto&hlokalita=&humkreis=25&cenaod=50000&cenado=250000&Submit=Hledat&kitx=ano

sub_auta_url <- "https://auto.bazos.cz/?hledat=&rubriky=auto&hlokalita=&humkreis=25&cenaod=50000&cenado=51000&Submit=Hledat&kitx=ano"
html_auta_sub <- read_html(sub_auta_url)

auta_url <- "https://auto.bazos.cz/"
html_auta <- read_html(auta_url)

# Zjisti celkový počet inzerátů
pocet_all <- 
  html_auta %>% 
  html_nodes(xpath = '//a[text() = "Mobilní verze"]//following::b') %>% 
  html_text()


# Zjisti počet vyfiltrovaných aut
pocet_sub <- 
  html_auta_sub %>% 
  html_nodes(xpath = '//div[contains(text(), "Zobrazeno ")]') %>% 
  html_text()

pocet_sub <- as.numeric(gsub(" ", "", sub(".*\\ů z ","", pocet_sub)))


if(pocet_sub %% 20 != 0){pocet_sub <- pocet_sub + (20-(pocet_sub %% 20))}

url_index <- seq(0,pocet_sub,20)
url_index <- url_index[-1]

urls <- paste0('https://auto.bazos.cz/', url_index,"/")

# Stáhni inzeráty aut
auta_all <- lapply(urls, function(x) {
  print( paste0( "scraping: ", x, " ... " ) )
  html_auta_x <- read_html(x)
  
  nazvy = html_auta_x %>%
    html_nodes(xpath = './/div[contains(@class,"inzeratynadpis")]//a//img') %>%
    html_attr('alt')
  print(nazvy)
  # Sys.sleep(2)
  
  inzeraty = html_auta_x %>% 
    html_nodes(xpath = './/h2[contains(@class,nadpis)]//a') %>% 
    html_attr('href')
  print(inzeraty)
  # Sys.sleep(2)
  
  ceny = html_auta_x %>% 
    html_nodes(xpath = './/div[contains(@class,"inzeratycena")]') %>% 
    html_text()
  print(ceny)
  # Sys.sleep(2)
  
  ceny <- ceny[-1]
  
  data.table(nazvy, inzeraty, ceny)
  
})

# Bind list to single data.table
auta_all_dt <- data.table::rbindlist(auta_all, use.names = TRUE, fill = TRUE)

(nazvy <- data.frame("nazvy" = auta_all_dt[[1]]))
(inzeraty <- data.frame("inzeraty" = auta_all_dt[[2]]))
(ceny <- data.frame("ceny" = auta_all_dt[[3]]))

# nazvy[nazvy$nazvy == "Galerie", ] <- NA
# nazvy <- na.omit(nazvy)

# nth_delete <- function(dataframe, n){
#   dataframe[-(seq(n, to=nrow(dataframe), by=n)), ]
# }

# inzeraty <- as.data.frame(nth_delete(inzeraty, 21))

inzeraty <- lapply(inzeraty, function(x) {
  x <- str_sub(x, 2, nchar(x))
  auta_url_1 <- gsub('\\./', '', paste0(auta_url, x))
  print(auta_url_1)
})

inzeraty <- data.frame("inzeraty" = unlist(inzeraty), row.names = 1:length(inzeraty[[1]]))

# ceny[ceny$ceny %in% c("Dohodou"),] <- NA
# ceny <- na.omit(ceny)

# for (x in 1:nrow(ceny)){
#   print(ceny[x, 1])
#   if(ceny[x, 1] %in% c("Dohodou", "V textu")) next()
#   ceny[x, 1]  <- sub("Kč", "", ceny[x, 1] )
#   ceny[x, 1]  <- gsub("[[:space:]]", "", ceny[x, 1] )
# }


# ceny <- data.frame("ceny" = unlist(ceny), row.names = 1:length(ceny[[1]]))

auta_all <- data.frame(nazvy, ceny, inzeraty)
# auta_all[auta_all$unlist.ceny. == "Dohodou" | auta_all$unlist.ceny. == "Vtextu",] <- NA
# auta_all[[2]] <- as.integer(auta_all[[2]])


# Stáhni značky aut
znacky <- 
  html_auta %>% 
  html_nodes(xpath = './/div[contains(@class,"barvaleva")]//a') %>% 
  html_attr('href')

znacky <- znacky[1:24]

znacky <- lapply(znacky, function(x) {
  x <- str_sub(x, start = 2, end = nchar(x)-1)
  firstup(x)
})

znacky <- do.call(rbind.data.frame, znacky)
znacky[21,] <- "Škoda"
colnames(znacky) <- "Značka"

znacky <- rbind(znacky, "VW", "Iveco", "Subaru") # dodej se ještě další varianty značek

znacky_upper <- lapply(1:nrow(znacky), function(x) {
  x <- toupper(znacky[x,1])
})
znacky_upper <- do.call(rbind.data.frame, znacky_upper)
colnames(znacky_upper) <- "Značka"

znacky <- rbind(znacky, znacky_upper)
rm(znacky_upper)

auta_all$znacka <- NA
vec_znacky <- unlist(c(znacky))
for ( i in 1:length(vec_znacky)) {
  TFvec <- grepl(vec_znacky[i], auta_all[[1]])
  auta_all$znacka[TFvec] <- vec_znacky[i]
}
rm(vec_znacky)


colnames(auta_all) <- c("Název", "Cena (v Kč)", "Odkaz", "Značka")
auta_all

