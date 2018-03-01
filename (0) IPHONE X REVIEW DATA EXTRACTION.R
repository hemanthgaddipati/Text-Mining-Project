require(stringr)||install.packages("stringr"); library(stringr)  
require(utils)||install.packages("utils"); library(utils)  


Amazon = function(url,      
                  n )       
  
{           
  
  text_page=character(0)   # define blank file
  
  pb <- txtProgressBar()    # define progress bar
  url = unlist(str_split(url,"&"))[1]
  
  for(i in 0:n){           # loop for url
    
    p =i*10	
    e = "&rating=1,2,3,4,5&reviewers=all&type=all&sort=most_helpful&start="
    url0 = paste(url,e,p,sep="")           
    
    text = readLines(url0)     # Read URL       
    
    text_start = grep("a-size-base review-text",text)   # start marker
    
    text_stop = grep("a-row a-expander-container a-expander-inline-container", text)  # end marker
    
    
    setTxtProgressBar(pb, i)             # print progress bar
    
    if (length(text_start) == 0) break    # checking for loop termination, i.e valid page found      
    
    for(j in 1:length(text_start))             
    {
      text_temp = paste(paste(text[(text_start[j]+1):(text_stop[j])]),collapse=" ")
      text_page = c(text_page,text_temp)
    }
    
  }
  
  text_page =gsub("<.*?>", "", text_page)       # regex for Removing HTML character 
  text_page = gsub("^\\s+|\\s+$", "", text_page) # regex for removing leading and trailing white space
  return(text_page)       # return reviews
}

url = "https://www.amazon.in/Apple-iPhone-Space-Grey-64GB/product-reviews/B072LPF91D/ref=cm_cr_getr_d_paging_btm_1?pageNumber=1"
Ix = Amazon (url,10)
length(Ix)

Ix1 <- as.data.frame(Ix)
Ix2 <- write.table(Hp, 'Iphone-X.txt')
getwd()

