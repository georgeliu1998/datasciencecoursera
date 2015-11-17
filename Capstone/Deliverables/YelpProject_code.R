### GET THE DATA ### 

# Load necessary packages:
library(dplyr); library(jsonlite); library(caret); library(tm); library(wordcloud); library(ggplot2); library(SnowballC); library(cluster)
# Set working directory
setwd("C:/Users/George/Dropbox/WorkingDir/datasciencecoursera/Capstone")
# Download the data
addr <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/yelp_dataset_challenge_academic_dataset.zip"
download.file(addr, destfile = "yelp.zip", method = "internal", setInternet2(use = TRUE))
# List the files in the zip folder
l <- unzip("yelp.zip",list = TRUE); l
# Unzip the fils
unzip("yelp.zip")
# Load the JSON files and save them as corresponding RDS files for later retrieval
business <- stream_in(file(l$Name[3]))
business <- flatten(business, recursive = TRUE)
saveRDS(business, file = "business.RDS")
rm(business)
checkin <- stream_in(file(l$Name[4]))
checkin <- flatten(checkin, recursive = TRUE)
saveRDS(checkin, file = "checkin.RDS")
rm(checkin)
review <- stream_in(file(l$Name[5]))
review <- flatten(review, recursive = TRUE)
saveRDS(review, file = "review.RDS")
rm(review)
tip <- stream_in(file(l$Name[6]))
tip <- flatten(tip, recursive = TRUE)
saveRDS(tip, file = "tip.RDS")
rm(tip)
user <- stream_in(file(l$Name[7]))
user <- flatten(user, recursive = TRUE)
saveRDS(user, file = "user.RDS")
rm(user)

### CLEAN THE DATA ### 

# Read in the business RDS file
business <- readRDS("business.RDS")
# Now subset the business data frame taking only the first 13 columns that are relevant for the analysis and remove those rows with no category info (character(0)) or NA's
biz_sub <- select(business, business_id:type)
biz_sub <- biz_sub[biz_sub$categories != "character(0)",]
biz_sub <- biz_sub[!is.na(biz_sub$categories),]
# Next, we'll remove subcategories that are not part of this following major categories list as found on the Yelp.ca website:
lst <- c("Restaurants", "Food", "Nightlife", "Shopping", "Beauty & Spas", "Health & Medical", "Local Services", "Automotive", "Home Services", "Arts & Entertainment", "Event Planning & Services", "Hotels & Travel", "Active Life", "Local Flavor", "Pets", "Public Services & Government", "Education", "Professional Services", "Financial Services", "Real Estate", "Mass Media", "Religious Organizations") 
# Make a new list that keep all the entries that appear in the list above
cat <- lapply(biz_sub$categories, FUN = function(x) x[x %in% lst])
# Find the percentage of multi-category entries
len <- lapply(cat, FUN = function(x) length(x))
sum(len > 1) / length(cat) #almost 18% of the businesses belong to multiple categories
# For those multi-category businesses, randomly select one of the categories as the final category for the business
cat_new <- lapply(cat, FUN = function(x) 
  ifelse(length(x) == 1, 
         x,
         x[sample(1:length(x), 1)]
  )
)
biz_subnew <- data.frame(biz_sub, category = as.character(cat_new))
# Save the data frame into an RDS file
saveRDS(biz_subnew, file = "biz_subnew.RDS")

### ANALYZE THE DATA ### 

# Calculate the average and median stars by category
biz_subnew <- readRDS("biz_subnew.RDS")
group_cat <- group_by(biz_subnew, category)
cat_star <- select(group_cat, category, stars)
rating_summary <- summarise(cat_star, 
                            count = n(), 
                            avg_stars = round(mean(stars), digit = 1), 
                            median_stars = median(stars))
rating_summary <- arrange(rating_summary, avg_stars)
rating_summary <- as.data.frame(rating_summary); rating_summary
# Visualize the result
rating_summary$category <- factor(rating_summary$category, 
                                  levels = rating_summary$category,
                                  ordered = TRUE) #turn the factor into an ordered one
ggplot(rating_summary, aes(x = category, y = avg_stars)) + 
  geom_point() + 
  theme(axis.text.x=element_text(angle=45, hjust=1)) 
# We now plot the distributions of select categories
real_night_pet <- filter(biz_subnew, 
                         category == "Real Estate" | 
                           category == "Nightlife" | 
                           category == "Pets")
ggplot(real_night_pet, aes(x = stars)) + 
  geom_histogram() + 
  facet_wrap(~ category)

### PREPARE THE DATA FOR TEXT MINING ### 

# Read in the review data
review <- readRDS("review.RDS")
# Make a subset having only the info we need
review_sub <- select(review, business_id, stars, text)
# Create a business subset to be joined with review
biz_temp <- select(biz_subnew, business_id, category)
# Join review with business so that category can be identified in review
review_new <- left_join(review_sub, biz_temp, by = "business_id")
# Save the file as RDS file for later use
saveRDS(review_new, file = "review_new.RDS")

### TEXT MINING ### 

# First start with real estate. Subset the data to include Real Estate category only
review_new <- readRDS("review_new.RDS")
review_real <- filter(review_new, category == "Real Estate", stars <= 3.0)
review_real_df <- select(review_real, text)
review_source_real <- DataframeSource(review_real_df)
corpus_real <- Corpus(review_source_real)
# Perform text pre-processing
corpus_real <- tm_map(corpus_real, stemDocument) 
corpus_real <- tm_map(corpus_real, stripWhitespace)
corpus_real <- tm_map(corpus_real, content_transformer(tolower))
corpus_real <- tm_map(corpus_real, removeWords, stopwords("english"))
corpus_real <- tm_map(corpus_real, removePunctuation)
corpus_real <- tm_map(corpus_real, removeNumbers)   
corpus_real <- tm_map(corpus_real, PlainTextDocument)  
# Create the document-term matrix
dtm_real <- DocumentTermMatrix(corpus_real) 
# Find the frequency of words and plot a word cloud
freq_real <- colSums(as.matrix(dtm_real))
freq_real <- sort(freq_real, decreasing=TRUE)
head(freq_real, 100)
#findFreqTerms(dtm_real, lowfreq = 600)
wordcloud(names(freq_real), freq_real, min.freq = 200, random.order = FALSE)
# Find relationships between terms with term correlations
cor_terms <- findAssocs(dtm_real, head(names(freq_real), 150), corlimit = 0.3)
cor_terms[cor_terms != "numeric(0)"]
# Find relationships between terms through Clustering by Term Similarity
dtms_real <- removeSparseTerms(dtm_real, 0.75) #remove sparse terms
d_real <- dist(t(dtms_real), method="euclidian")   
fit_real <- hclust(d=d_real, method="ward"); fit_real
plot(fit_real, hang=-1)   
# Now repeat the process for Nightlife and Pets categories
# Nightlife
review_new <- readRDS("review_new.RDS")
review_nite <- filter(review_new, category == "Nightlife")
review_nite_df <- select(review_nite, text)
review_source_nite <- DataframeSource(review_nite_df)
corpus_nite <- Corpus(review_source_nite)
corpus_nite <- tm_map(corpus_nite, stemDocument) 
corpus_nite <- tm_map(corpus_nite, stripWhitespace)
corpus_nite <- tm_map(corpus_nite, content_transformer(tolower))
corpus_nite <- tm_map(corpus_nite, removeWords, stopwords("english"))
corpus_nite <- tm_map(corpus_nite, removePunctuation)
corpus_nite <- tm_map(corpus_nite, removeNumbers)   
corpus_nite <- tm_map(corpus_nite, PlainTextDocument)  
dtm_nite <- DocumentTermMatrix(corpus_nite) 
#freq_nite <- colSums(as.matrix(dtm_nite))
#freq_nite <- sort(freq_nite, decreasing=TRUE)
#head(freq_nite, 100)
freq_term_nite <- findFreqTerms(dtm_nite, lowfreq = 15000); freq_term_nite
# Find relationships between terms with term correlations
findAssocs(dtm_nite, c("never"), corlimit = 0.3)
# Find relationships between terms through Clustering by Term Similarity
dtms_nite <- removeSparseTerms(dtm_nite, 0.75) #remove sparse terms
d_nite <- dist(t(dtms_nite), method="euclidian")   
fit_nite <- hclust(d=d_nite, method="ward"); fit_nite
plot(fit_nite, hang=-1)   

