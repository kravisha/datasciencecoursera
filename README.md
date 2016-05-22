# Steps for project completion

## Step 1. First the datafiles are read into Data frames. some are read as fixed width files and others as csv.
## Step 2. the activity files is creaded by code as this is a small file with 2 colums and 6 rows
## Step 3. The subject test and train data appended
## Step 4. The Activity test and train data appended
## Step 5. The Samsumg readings for Test and Train data are merged
## Step 6. The labels file are Read from file and save to name vector
## step 7. The column names of the Samsung Readings are changed to what is in the labels file
## Step 8. The columns needed for final dataset are identified - by grepping for mean and Std 
## Step 9. The data is filtered for these columns alone
## Step 10. The Subject and Activity data are merged to this filtered through cbind
## Step 11. A lookup is done against the activites table created in step 2 and the actual activity name is identfied
## Step 12. the "()" and "-" in the column names are cleaned up
## Step 13. Group final data by activity and subject 
## Step 14. Summarize the dataset to obtain mean values
## Step 15. Using reshape2 library make each variable as a row item - normalize the data/ Tidy the data
## Step 16. Clean up the data to make the data meaningful throgh gsub

# Cook Book

## The final tidy dataset is called tidydata.txt
## it has 4 columns
## 1st Column - "ActivityName"  
## 2nd Column - "Subject"  
## 3rd Column - "variable"   - names of variables for which we are summarizing the data 
## 4th Column - "value"   - the mean of the variables grouped by activity and subject
