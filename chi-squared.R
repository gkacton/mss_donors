# This script prepares data for chi-squared tests, and runs those tests.


# Chi-Squared test: Maine vs. Non-Maine donations ----------------------

# Starts by making a distribution table --> 
  # counts of how many donations fit in each amount category.
# Donations are binned into <$5, $5-$10, $10-$20, and >$20

# initializing the table of counts
dist_table_1 <- as.data.frame(matrix(data = 0, nrow = 2, ncol = 4))
colnames(dist_table_1) <- c("<5", "5-10", "10-20", "20+")
rownames(dist_table_1) <- c("Maine", "Not Maine")

# populating the table with counts

for(i in 1:nrow(mss_donors)){
  if(mss_donors$is_not_maine[i] == 0) {
    current_amt <- mss_donors$Amount[i]
    if(current_amt < 5){
      dist_table_1[1,1] <- dist_table_1[1,1]+1
    } else if(current_amt >= 5 & current_amt < 10) {
      dist_table_1[1,2] <- dist_table_1[1,2]+1
    } else if(current_amt <= 10 & current_amt <20) {
      dist_table_1[1,3] <- dist_table_1[1,3]+1
    } else if(current_amt >= 20){
      dist_table_1[1,4] <- dist_table_1[1,4]+1
    }
  }
  if(mss_donors$is_not_maine[i] == 1) {
    current_amt <- mss_donors$Amount[i]
    if(current_amt < 5){
      dist_table_1[2,1] <- dist_table_1[2,1]+1
    } else if(current_amt >= 5 & current_amt < 10) {
      dist_table_1[2,2] <- dist_table_1[2,2]+1
    } else if(current_amt <= 10 & current_amt <20) {
      dist_table_1[2,3] <- dist_table_1[2,3]+1
    } else if(current_amt >= 20){
      dist_table_1[2,4] <- dist_table_1[2,4]+1
    }
  }
}

# running the chi-squared test

chisq.test(dist_table_1)


# Chi-Squared test: Counties ----------------------------------------------

# Starts by making a distribution table --> 
  # counts of how many donations fit in each amount category.
# Donations are binned into <$5, $5-$10, $10-$20, and >$20

# initializing the table

maine_counties <- c("Androscoggin", "Aroostook", "Cumberland", "Franklin", "Hancock", 
                    "Kennebec", "Knox", "Lincoln", "Oxford", "Penobscot", "Piscataquis",
                    "Sagadahoc", "Somerset", "Waldo", "Washington", "York")
mss_donors_maine <- mss_donors_maine[is.na(mss_donors_maine$Amount) == FALSE,]
dist_table_2 <- as.data.frame(matrix(nrow = 16, ncol = 4, data = 0))
colnames(dist_table_2) <- c("<5", "5-10", "10-20", "20+")
rownames(dist_table_2) <- maine_counties

# populating the table with counts

for(i in 1:16){
  county <- maine_counties[i]
  for(j in 1:nrow(mss_donors_maine)){
    current_amt <- mss_donors_maine$Amount[j]
    if(mss_donors_maine$county[j] == county){
      if(current_amt < 5){
        dist_table_2[i,1] <- dist_table_2[i,1]+1
      } else if(current_amt >= 5 & current_amt < 10) {
        dist_table_2[i,2] <- dist_table_2[i,2]+1
      } else if(current_amt <= 10 & current_amt <20) {
        dist_table_2[i,3] <- dist_table_2[i,3]+1
      } else if(current_amt >= 20){
        dist_table_2[i,4] <- dist_table_2[i,4]+1
      }
    }
  }
}

# Removing all rows with zeroes --> 
  # chisq.test() produced an error when these were not removed

dist_table_2 <- dist_table_2[dist_table_2$'<5' != 0 & 
                                   dist_table_2$'5-10' != 0 & 
                                   dist_table_2$'10-20' != 0 & 
                                   dist_table_2$'20+' != 0,]

# running the Chi-Squared test
chisq.test(dist_table_2)
