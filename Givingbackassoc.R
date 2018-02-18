### Website : https://www.givingbackassoc.org
### Objective: To extract the charities listed in the above website  
## Output : all_charities.csv

## Load the required librariries
library(rvest)
library(stringr)
library(dplyr)

## Refer to the required webpage in the website.
url <- 'https://www.givingbackassoc.org/english/charities-database/'

webpage <- read_html(url)

## Extract the Causes categories . They are set up as weblinks so that when clicked on each, it refers to another webpage which lists 
## all the charities concerned with each cause.

all_charities_html <- html_nodes(webpage , '.j-text a , .j-hgrid+ .j-hgrid a span')

causes_websites <- html_attr(all_charities_html , "href")
####################################################################################
## Some formatting required to remove the blanks and to select the required websites.
causes_websites <- causes_websites[!(is.na(causes_websites))]

## We dont need the weblinks for 'youth education' , welfare -community-development' ,
## 'Around Bangkok' , 'North Thailand' and  'http://www.concordiathailand.org/'
#causes_websites <- causes_websites[1:13]
causes_websites <- causes_websites[-c(6, 8, 9,10, 14)]


###################################################################################
## Build the entire link to the websites
causes_websites <- paste("https://www.givingbackassoc.org", causes_websites , sep="")

welfare_dev_websites <- c("https://www.givingbackassoc.org/english/charities-database/welfare-community-development/" ,"https://www.givingbackassoc.org/english/charities-database/welfare-community-development-2/")

youth_add_websites <- c("youth-education/" ,"youth-education-e-g/", "youth-education-h-k/" , "youth-education-l-r/", "youth-education-s-z/")

youth_add_websites <- paste("https://www.givingbackassoc.org/english/charities-database/" , youth_add_websites, sep ="")


###################################################################################

all_charities_df <- data.frame()

#### Loop along each website and extract data . Create a single dataframe with all the data .
for(j in seq_along(causes_websites)){
  
  charity.df <- extract_data(causes_websites[j], css = 1)
  
  all_charities_df <- rbind(all_charities_df , charity.df, stringsAsFactors =FALSE )
}

for(j in seq_along(welfare_dev_websites)){
  
  charity.df <- extract_data(welfare_dev_websites[j], css = 2)
  
  
  all_charities_df <- rbind(all_charities_df , charity.df , stringsAsFactors =FALSE)
}

for(j in seq_along(youth_add_websites)){
  
  charity.df <- extract_data(youth_add_websites[j], css = 2)
  
  
  all_charities_df <- rbind(all_charities_df , charity.df , stringsAsFactors =FALSE)
}


### Write out the csv file 

write.csv(all_charities_df , file = "All_charities.csv", row.names = FALSE)

#########################################################################################

### Create a function for extracting data from each of  the required websites .

extract_data <- function(read_website, css){
  
  url <- read_website
  
  webpage <- read_html(url)
  
  if(css == 1){
    extracted_html <- html_nodes(webpage , '.last .j-text') }
  if(css ==2){ 
    ## Only for Youth - education charities and for welfare development charities websites
    ## Use the css selectors as below .
    extracted_html <- html_nodes(webpage , '.last:nth-child(3) .j-text') 
  }# 
  
  ####### Scrape out each of the charity details as a list .
  
  details <- lapply(extracted_html , FUN= function(extracted_html){ html_text(html_children(extracted_html) , trim =TRUE)})
  
  ## Remove the blank spaces
  details <- lapply(details , function(details){details[details != ""]})
  
  #################################################################################
  
  ## Check if the information in the list is for website , address , telephone number etc
  ## Website 
  is.website <- lapply(X=details , FUN= function(X){ str_detect(X ,"http|www") })
  ## Address
  is.address <- lapply (X=details , FUN= function(X){str_detect(X ,"Address")})
  ## Telephone
  is.telephone <- lapply(X= details , FUN= function(X){str_detect(X ,"Telephone|[Tt][Ee][Ll]|Mobile|[Pp]hone")})
  ## Email id
  is.email <- lapply (X = details , FUN= function(X){str_detect(X ,"[Ee][-]*[mM][aA][iI][lL]|E-mail|e-mail")})
  
  ## Province
  is.province <- lapply(X = details , FUN= function(X){str_detect(X ,"Province") })
  ## Tag the list data for all keywords  of the above  
  is.description_address <- lapply(X = details , FUN= function(X){str_detect(X ,"http|www|Email|E-mail|e-mail") })
  
  ## Initialize the values 
  name <- character()
  description_address <- character()
  address <- character()
  website <- character()
  telephone <- character()
  #contact <- character()
  email <- character()
  province <- character()
  
  ##################################################################################
  ## Once the list is marked for existance of the required word , seperate them out to different 
  ## fields for every charity
  
  
  for(i in seq_along(details)){
    
    ## Get the associated cause 
    Cause <- str_sub(url, start =60 )
    Cause <- str_replace(Cause , "/" , "")
    
    name <- details[[i]][1]
    
    website <- unlist(details[[i]])[unlist(is.website[[i]])] 
    if(length(website) == 0){ website <- " "}
    else { 
      
      pattern <- "[www]*[http://]*.[[:alnum:]]+[-_]*.[[:lower:]]*.[[:lower:]]*[/]*[[:alnum:]]*[[:punct:]]*[/]*[[:alnum:]]*|[www]*[http://]*.[[:alnum:]]+[-_]*.[[:lower:]]*.org"
      
      website <- unlist(str_extract_all(website , pattern))
      
      website <- paste(website , collapse = " ")
        }
    
    address <- unlist(details[[i]])[unlist(is.address[[i]])]
    if(length(address) == 0){ address <- " "}
    else { address <- paste(address , collapse = " ")}
    address <- str_trim(str_replace_all(address ,"Address:" , "")) ## Remove "Address" from the  text  
    
    
    telephone <- unlist(details[[i]])[unlist(is.telephone[[i]])]
    if(length(telephone) == 0){ telephone <- " "}
    else {
      
      telephone <- paste(telephone , collapse = " ")
      
      ## Check if other text is also extracted with telephone number . This happens when the html page
      ## is written in that format . 
      if(nchar(telephone) > 20 ){
        telephone <- str_split(telephone , "Telephone|Phone|[Tt][Ee][Ll]")[[1]][2]
        telephone <- str_extract(telephone , "[+]*[[:punct:]]*\\d+[[:punct]]*\\s*[[:punct]]*\\d+[[:punct:]]*\\s*[[:punct]]*\\d*\\s*[[:punct:]]*\\d*\\s*[[:punct:]]*\\d*") 
      }
      else { telephone <- str_replace_all(telephone , "Tel|Phone|Telephone|TEL|Mobile|:|ephone" ,"")}
      }
    
    email <- unlist(details[[i]])[unlist(is.email[[i]])]
    if(length(email) == 0){ email <- " "}
    else { 
      
     email <- unlist(str_extract_all(email , "[[:alnum:]]+[-._]*[[:alnum:]]*@[[:alnum:]]*.[[:alnum:]]+[.]*[[:alnum:]]*[.]*[[:alnum:]]*"))
     ## Incase there are more than one emails , concat them into one field 
     email <- paste(email , collapse = " ") 
    
      }
    
    province <- unlist(details[[i]])[unlist(is.province[[i]])]
    if(length(province) == 0){ province <- " "}
    else { pos <- str_locate(province, "Province:") 
          province <- str_sub(province , (pos[2] + 1) , (pos[2] + 15))
          province <- paste(province , collapse = " ")
    }
    
    ## Seperate out the remaining description and any other info that is not seperated above.
    description_address <- unlist(details[[i]])[!unlist(is.description_address[[i]])]
    if(length(description_address) == 0){ description_address <- " "}
    else { description_address <- paste(description_address , collapse =" ") }
    
    ##################################################################################
    ## Create a data frame containing the seperated fields /information
    
    if(i ==1){
      temp.df1 <- data.frame("cause_area" = Cause, "name"= name, "description" = description_address , "address" = address , "website"= website,  "contact_number" = telephone ,  "email" = email, "city" = province, stringsAsFactors =FALSE)
      
    }
    else{
      temp.df2 <- data.frame("cause_area"= Cause, "name"= name, "description" = description_address , "address" = address ,"website"= website,  "contact_number" = telephone ,  "email" = email, "city" = province, stringsAsFactors =FALSE)
      temp.df1 <- rbind(temp.df1,temp.df2 , stringsAsFactors =FALSE)
    }
    
  }
  charity.df <- temp.df1
  
  return(charity.df)
}

