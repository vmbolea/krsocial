library(rvest)
library(xml2)
library(pxR)
library(httr)
library(sf)


# Define the URL of the website you want to scrape
url <- "https://www.aragon.es/-/estadisticas-ganaderas"

url.dl <- "https://www.aragon.es/"

# Use rvest to extract the URLs of all CSV files on the website
links <- url %>%
  read_html() %>%
  html_nodes("a") 


class(xml_attrs(links[[208]]))

pop.url <- "https://opendata.aragon.es/aod/services/web/datasets/cifras-de-poblacion-revision-del-padron-municipal/resourceCSV/02-01-01-01-0201010101.px"
pop.file <- "data/pop.px"
download.file(pop.url, pop.file)
pop.data <- read.csv(pop.file, sep = ",")

pop.data <- pop.data[41:nrow(pop.data),]
pop.codes <- sapply(strsplit(as.character(pop.data$Lugar.de.residencia), " "), "[", 1)



pop.data <- pop.data[, endsWith(names(pop.data), "os")]
pop.names <- paste0("h_", gsub(pattern = ".*enero.",
                          replacement = "",
                          x = gsub( pattern = ".Ambos.*$",
                                    replacement = "",
                                    x = names(pop.data))))

names(pop.data) <- pop.names

pop.data <- pop.data[,c(paste0("h_",2021:2013))]
pop.data$h_2022 <- pop.data$h_2021


pop.data <- cbind( ine = pop.codes, pop.data)

muni.shp <- sf::read_sf("data/municipios.shp", crs = 4326)
muni.geo <- muni.shp[, "c_muni_ine"]

muni.data <-sf::st_drop_geometry(muni.shp)

muni.data <- muni.data[,c("c_muni_ine", "d_muni_ine", "c_comarca",  "d_comarca", "sup_of_km2")]





if(!dir.exists("data")){

  dir.create(path = "data" )

  
}


output.path <- "data"
output.name <- "ganaderia_municipal"
x <- c()


  
for( i in 1:length(links)){
  
  #i<-216
  
  i.path <- grep(pattern = "municipal",
            x = xml_attrs(links[[i]]), 
            value = T) 
  
  
  if(length(i.path) > 0){
    print(i)
    i.path <- gsub(pattern = "\\+",
            replacement = "_",
            x = i.path)
    
  year.i <- as.numeric(gsub( pattern = ".*ipal_",
                  replacement = "",
                  x = gsub( pattern = ".x.*$",
                            replacement = "",
                            x = i.path)))
    
  if(!is.na(year.i)){
 
  temp_file <- tempfile()
  
  download.file(paste0(url.dl,i.path), temp_file, mode = "wb") 
  
  
  
  for(j in c("Bovino","Ovino-Caprino","Porcino")){
    print(j)
  data.i <- readxl::read_excel(temp_file,sheet = j) 
  

                           
  
  new.name <- tolower(substr(j, 0, 1))
  
  data.i <- data.i[,c(2,ncol(data.i))]
  
  names(data.i) <- c("code",paste0(new.name,"_",year.i))
   
  print(nrow(data.i)) 
  print(paste0("diff ", data.i$code[!data.i$code %in% muni.data$c_muni_ine]))
  
  data.i$code <- ifelse( test = data.i$code %in% c("24420", "24421"),
                         yes = data.i$code + 19800,
                         no =  data.i$code )
  
  
  data.i <- data.i[!data.i$code %in% "22",]
  print(paste0("diff after ", data.i$code[!data.i$code %in% muni.data$c_muni_ine]))
  
  
  data.i <- data.i[!duplicated(data.i$code), ]
  
  
  muni.data <- merge(muni.data, data.i,  by.x = "c_muni_ine" , by.y = "code", all.x = TRUE)
 
 print(nrow(muni.data))
  }
  }
  }
  
}
  
    


muni.data <- merge(x = muni.data,
                   y = pop.data,
                   by.x = "c_muni_ine",
                   by.y = "ine",
                   all.x = TRUE)




muni.geo <-merge(muni.geo, muni.data) 



muni.geo2 <- muni.geo[duplicated(muni.geo$c_muni_ine), ]


muni.geo2 <- replace
muni.geo2[is.na(muni.geo2)] <- 0

sf::st_write(muni.geo2, paste0("data/g_data.shp"))
sf::st_write(muni.geo2, paste0("data/g_data.geojson"))

str(muni.geo2)
