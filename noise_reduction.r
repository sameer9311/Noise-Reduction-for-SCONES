#
# Script to reduce noise from log ratio signal of 2 samples
# @ author : Sameer Sonawane - sameer9311@gmail.com
# last modified : 2016/3/4

# Read data from a file in an initial data_frame 
initial_data = read.table(file.choose(), head = TRUE, sep = ",")

#initialize an empty new data frame. This frame would contain the data after removing noise
new_data = initial_data[0,]

#Plot some graphs to get an understanding of distribution of data

#box plots
boxplot(initial_data$testSample1~initial_data$chr, main="Test Sample 1 - Initial", xlab="chr" , ylab = "Log Ratio",las = 1 )
boxplot(initial_data$testSample2~initial_data$chr, main="Test Sample 2 - Initial", xlab = "chr", ylab = "Log Ratio",las = 1 )

#scatter plots
plot(initial_data$start, initial_data$testSample1, col = "seagreen", main = "Test Sample 1 - Initial", ylab="Log Ratio", xlab="Start", las = 1)
plot(initial_data$start, initial_data$testSample2, col = "seagreen", main = "Test Sample 2 - Initial", ylab="Log Ratio", xlab="Start", las = 1)
plot(initial_data$testSample1, initial_data$testSample2, col = "seagreen" , main = "Test sample 1 vs 2 - Initial",ylab="Test Sample 2", xlab="Test Sample 1", las = 1)

#number of rows in new data frame
num_records = 0

#Inter quartile range for samples 1 and 2
sample1_iqr = IQR(testSample1)
sample2_iqr = IQR(testSample2)

#calculate the correlation coefficient before applying the algorithm
cor(initial_data$testSample1, initial_data$testSample2)

# set the outlier analysis factor (generally this is 1.5)
# this can be modified using regression to increase correlation coefficient
# but can lead to ignoring large amount of data
outlier_factor = 2.5


for(i in 1:dim(initial_data)[1])     # for each row of initial data
{
  if(initial_data[i,"testSample1"] > quantile(initial_data$testSample1, 3/4) && initial_data[i,"testSample1"]-quantile(initial_data$testSample1,3/4) > outlier_factor * sample1_iqr)
  {
    #positive outlier for sample 1
  }
  else if(initial_data[i,"testSample1"] < quantile(initial_data$testSample1, 1/4) && quantile(initial_data$testSample1,3/4) - initial_data[i,"testSample1"] > outlier_factor * sample1_iqr)
  {
    #negative outlier for sample 1
  }
  else if(initial_data[i,"testSample2"] > quantile(initial_data$testSample2, 3/4) && initial_data[i,"testSample2"]-quantile(initial_data$testSample2,3/4) > outlier_factor * sample2_iqr)
  {
    #positive outlier for sample 2
  }
  else if(initial_data[i,"testSample2"] < quantile(initial_data$testSample2, 1/4) && quantile(initial_data$testSample2,3/4) - initial_data[i,"testSample2"] > outlier_factor * sample2_iqr)
  {
    #negative outlier for sample 2
  }
  else
  {
    # data values good , not noise.
    # add the data values to new data
    new_data[num_records+1,] <- initial_data[i,]
    num_records = num_records + 1
  }
}

#plots to analyze the effect of algorithm

#box plots
boxplot(new_data$testSample1~new_data$chr, main="Test Sample 1 - Final", xlab="chr" , ylab = "Log Ratio",las = 1 )
boxplot(new_data$testSample2~new_data$chr, main="Test Sample 2 - Final", xlab = "chr", ylab = "Log Ratio",las = 1 )

#scatter plots
plot(new_data$start, new_data$testSample1, col = "seagreen", main = "Test Sample 1 - Final", ylab="Log Ratio", xlab="Start", las = 1)
plot(new_data$start, new_data$testSample2, col = "seagreen", main = "Test Sample 2 - Final", ylab="Log Ratio", xlab="Start", las = 1)
plot(new_data$testSample1, new_data$testSample2, col = "seagreen" , main = "Test sample 1 vs 2 - Final",ylab="Test Sample 2", xlab="Test Sample 1", las = 1)


# Correlation coefficient after applying the algorithm
cor(new_data$testSample1, new_data$testSample2)

# write the new noise reduced data in a csv file
write.csv(new_data, file = "noise_reduced_data.csv", row.names = TRUE)

