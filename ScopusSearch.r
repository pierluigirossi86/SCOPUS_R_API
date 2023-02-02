library('rscopus')
library(tidyr)
library(dplyr)

#set your API key here, you can get it through Elsevier Dev page
#Limits can be found here: https://dev.elsevier.com/api_key_settings.html
Sys.setenv("elsevier_API" = "INSERT_KEY_HERE")

#Alternate API setting
Elsevier_API = "INSERT_KEY_HERE"
options("elsevier_api_key" = Elsevier_API)
set_api_key(Elsevier_API)


#create an object containing every scopus search result for a certain author given name and surname (provide authorID instead if more than one author is found)
all_dat = author_data(last_name = "Surname", 
                      first_name = "Name", verbose = TRUE, general = TRUE)
                      
#convert data to dataframe (table)
res2 = all_dat$df
res2 = res2 %>% 
  rename(journal = `prism:publicationName`,
         title = `dc:title`,
         description = `dc:description`)

#test your API key and perform a query on SCOPUS
if (have_api_key()) {
  res = scopus_search(query = "text here", max_count = 20,
                      count = 10)
  df = gen_entries_to_df(res$entries)
  head(df$df)
}

#search for an author
query_string = "SURNAME"
s = generic_elsevier_api(query = query_string,
                         type = "search", search_type = "author",
                         api_key = "APY_KEY_HERE")
                         
# This part of the code was created to obtain a dataset of articles for a given number of authors listed in a csv file in which the authors' ID are already present
# Code is meant to generate also information regarding the ranking of the journals, from source resurcify.com

#Import CSV dataset which must contain LastName, FirstName, Affiliation. Extra column if author's ID is already known
listauthors<-read.csv("FILENAMEHERE.csv")
#If author's ID column is missing, add it. Unique identification will avoid to pick any other authors with the same name
listauthors$AuthorID<-""
#Create a dataframe to contain papers' info
pub_list<-data.frame()

#Run a loop scanning for every author's papers
for (i in 1:nrow(listauthors)){
  print(listauthors$LastName[i])
  #In case of errors, start from existing correct author, i.e. add this to skip first 59 authors: 
  if(i<21){next}
  #Run this line for the first time, so that you can also check author's ID in the author's info dataframe
  #results = author_df(last_name = listauthors$LastName[i], first_name = listauthors$FirstName[i], affilname_1=listauthors$Affiliation[i] , verbose = FALSE, general = FALSE)
  #listauthors$AuthorID[i]=results$au_id
  #After filling author's dataset with names, you can re-run the code with the following line instead
  #If IDs are already known, then just delete the previous two lines and run the following
  results = author_df(last_name = listauthors$LastName[i], first_name = listauthors$FirstName[i], au_id = listauthors$AuthorID[i], verbose = FALSE, general = FALSE)
  
  #Subset the data, since sometimes output might contain different column names (i.e. older affiliations)
  subsetdata<-subset(results,select = c("auth_order","affilname_1", "n_auth", "citations", "journal", "description", "title", "pii", "doi","cover_date","prism_url","dc_identifier","dc_creator","prism_issn","prism_eIssn","prism_pageRange","dc_description","prism_aggregationType","subtype","authkeywords","source_id","first_name","last_name","au_id"))
  pub_list<-rbind(pub_list,subsetdata)
}

#Save to CSV
write.csv(pub_list,"publist.csv", row.names=TRUE)

#Filter only first authors when duplicate publications are found
my_data <- as_tibble(pub_list)
my_data <- my_data[order(my_data$dc_identifier, my_data$auth_order),]
my_data <- my_data[!duplicated(my_data$dc_identifier), ]
write.csv(my_data,"pub_list_no_duplicates.csv", row.names=TRUE)

#Add journal rankings from resurchify.com
#Create empty dataframe with column names
scores<-setNames(data.frame(matrix(ncol = 4, nrow = 0)), c("h_index","j_rank","i_score","SJR_score"))
#Run a loop to fill the dataframe with scores, getting journal information from previous dataset
#Need to sort the dataframe and list it by journal ID, saves time in loop by skipping already processed values
sorted_df<-my_data[order(my_data$source_id),]
row.names(sorted_df)<-NULL
#Now the loop can start, using the same values for same IDs, which after sorting will now appear one after another
journalidprevious<-""
for (i in 1:nrow(sorted_df)){
  journalid=sorted_df$source_id[i]
  print(i)
  print(journalid)
  if(journalid!=journalidprevious){
    journalidprevious<-journalid
    print("Ok")
    resurchpage<-readLines(paste("https://www.resurchify.com/impact/details/",journalid,sep=""),n=298)
    hindex=gsub("[a-zA-Z <>/]", "", resurchpage[279])
    if(nchar(hindex)>8){hindex=""}
    jrank=gsub("[a-zA-Z <>/]", "", resurchpage[283])
    if(nchar(jrank)>8){jrank=""}
    iscore=gsub("[a-zA-Z <>/]", "", resurchpage[291])
    if(nchar(iscore)>8){iscore=""}
    sjrscore=gsub("[a-zA-Z <>/]", "", resurchpage[287])
    if(nchar(sjrscore)>8){sjrscore=""}
    scores_to_add<-data.frame(hindex,jrank,iscore,sjrscore)
    scores<-rbind(scores,scores_to_add)
  } else{
    print("skip")
    scores_to_add<-data.frame(hindex,jrank,iscore,sjrscore)
    scores<-rbind(scores,scores_to_add)
  }
}
#Merge the dataset with scores
datasetfinale<-cbind(sorted_df,scores)
write.csv(datasetfinale,"datasetfinale.csv", row.names=TRUE)

