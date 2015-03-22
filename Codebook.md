##Data Set Information

The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data.

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain.

Source of the original data:
https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

Full Description at the site where the data was obtained:
http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

##Process

1. It downloads the UCI HAR Dataset data set and puts the zip file working directrory. After it is downloaded, it unzips the file into the UCI HAR Dataset folder.
2. It loads the train and test data sets and appends the two datasets into one data frame. This is done using rbind.
3. It extracts just the mean and standard deviation from the features data set. This is done using grep. The result is merged to the dataset
4. The activities data set was observed and the activities in the dataset are converted accordingly
5. Using gsub() descriptive variables are relabled, next participants are relabled
6. A tidy dataset is created ant exported into the working directory


##Attribute Information

For each record in the dataset it is provided:

*Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration.
*Triaxial Angular velocity from the gyroscope.
*Its activity label.

##Citation Request

[1] Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-Ortiz. Human Activity Recognition on Smartphones using a Multiclass Hardware-Friendly Support Vector Machine. International Workshop of Ambient Assisted Living (IWAAL 2012). Vitoria-Gasteiz, Spain. Dec 2012

http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

Variable Descriptions

<table>
   <tr> <td> <b> data1</td> <td>data2 </b> </td> </tr>
   <tr> <td>data3</td> <td>data4</td> </tr>
  </table>

<table>
<tr> <td> <b> Variable </td> <td>Description</b> </td> </tr>
<tr> <td>subject	</td> <td> Subject ID</td> </tr>
<tr> <td>tbodyacc.mean.x</td> <td>Mean time for acceleration of body for X direction</td> </tr>
<tr> <td>tbodyacc.mean.y</td> <td>Mean time for acceleration of body for Y direction</td> </tr>
<tr> <td>tbodyacc.mean.z</td> <td>Mean time for acceleration of body for Z direction</td> </tr>
<tr> <td>tbodyacc.std.x</td> <td>Standard deviation of time for acceleration of body for X direction</td> </tr>
<tr> <td>tbodyacc.std.y</td> <td>Standard deviation of time for acceleration of body for Y direction</td> </tr>
<tr> <td>tbodyacc.std.z</td> <td>Standard deviation of time for acceleration of body for Z direction</td> </tr>
<tr> <td>tgravityacc.mean.x</td> <td>Mean time of acceleration of gravity for X direction</td> </tr>
<tr> <td>tgravityacc.mean.y</td> <td>	Mean time of acceleration of gravity for Y direction</td> </tr>
<tr> <td>tgravityacc.mean.z</td> <td>	Mean time of acceleration of gravity for Z direction</td> </tr>
<tr> <td>tgravityacc.std.x</td> <td>Standard deviation of time of acceleration of gravity for X direction</td> </tr>
<tr> <td>tgravityacc.std.y</td> <td>Standard deviation of time of acceleration of gravity for Y direction</td> </tr>
<tr> <td>tgravityacc.std.z</td> <td>Standard deviation of time of acceleration of gravity for Z direction</td> </tr>
<tr> <td>tbodyaccjerk.mean.x</td> <td>Mean time of body acceleration jerk for X direction</td> </tr>
<tr> <td>tbodyaccjerk.mean.y</td> <td>Mean time of body acceleration jerk for Y direction</td> </tr>
<tr> <td>tbodyaccjerk.mean.z</td> <td>Mean time of body acceleration jerk for Z direction</td> </tr>
<tr> <td>tbodyaccjerk.std.x</td> <td>Standard deviation of time of body acceleration jerk for X direction</td> </tr>
<tr> <td>tbodyaccjerk.std.y</td> <td>Standard deviation of time of body acceleration jerk for Y direction</td> </tr>
<tr> <td>tbodyaccjerk.std.z</td> <td>Standard deviation of time of body acceleration jerk for Z direction</td> </tr>
<tr> <td>tbodygyro.mean.x</td> <td>Mean body gyroscope measurement for X direction</td> </tr>
<tr> <td>tbodygyro.mean.y</td> <td>Mean body gyroscope measurement for Y direction</td> </tr>
<tr> <td>tbodygyro.mean.z</td> <td>Mean body gyroscope measurement for Z direction</td> </tr>
<tr> <td>tbodygyro.std.x</td> <td>Standard deviation of body gyroscope measurement for X direction</td> </tr>
<tr> <td>tbodygyro.std.y</td> <td>Standard deviation of body gyroscope measurement for Y direction</td> </tr>
<tr> <td>tbodygyro.std.z</td> <td>Standard deviation of body gyroscope measurement for Z direction</td> </tr>
<tr> <td>tbodygyrojerk.mean.x</td> <td>Mean jerk signal of body for X direction</td> </tr>
<tr> <td>tbodygyrojerk.mean.y</td> <td>Mean jerk signal of body for Y direction</td> </tr>
<tr> <td>tbodygyrojerk.mean.z</td> <td>Mean jerk signal of body for Z direction</td> </tr>
<tr> <td>tbodygyrojerk.std.x</td> <td>Standard deviation of jerk signal of body for X direction</td> </tr>
<tr> <td>tbodygyrojerk.std.y</td> <td>Standard deviation of jerk signal of body for Y direction</td> </tr>
<tr> <td>tbodygyrojerk.std.z</td> <td>Standard deviation of jerk signal of body for Z direction</td> </tr>
<tr> <td>tbodyaccmag.mean</td> <td>Mean magnitude of body Acc</td> </tr>
<tr> <td>tbodyaccmag.std</td> <td>Standard deviation of magnitude of body Acc</td> </tr>
<tr> <td>tgravityaccmag.mean</td> <td>Mean gravity acceleration magnitude</td> </tr>
<tr> <td>tgravityaccmag.std</td> <td>Standard deviation of gravity acceleration magnitude</td> </tr>
<tr> <td>tbodyaccjerkmag.mean</td> <td>Mean magnitude of body acceleration jerk</td> </tr>
<tr> <td>tbodyaccjerkmag.std</td> <td>Standard deviation of magnitude of body acceleration jerk</td> </tr>
<tr> <td>tbodygyromag.mean</td> <td>Mean magnitude of body gyroscope measurement</td> </tr>
<tr> <td>tbodygyromag.std</td> <td>Standard deviation of magnitude of body gyroscope measurement</td> </tr>
<tr> <td>tbodygyrojerkmag.mean</td> <td>Mean magnitude of body body gyroscope jerk measurement</td> </tr>
<tr> <td>tbodygyrojerkmag.std</td> <td>Standard deviation of magnitude of body body gyroscope jerk measurement</td> </tr>
<tr> <td>fbodyacc.mean.x</td> <td>Mean frequency of body acceleration for X direction</td> </tr>
<tr> <td>fbodyacc.mean.y</td> <td>Mean frequency of body acceleration for Y direction</td> </tr>
<tr> <td>fbodyacc.mean.z</td> <td>Mean frequency of body acceleration for Z direction</td> </tr>
<tr> <td>fbodyacc.std.x</td> <td>Standard deviation of frequency of body acceleration for X direction</td> </tr>
<tr> <td>fbodyacc.std.y</td> <td>Standard deviation of frequency of body acceleration for Y direction</td> </tr>
<tr> <td>fbodyacc.std.z</td> <td>Standard deviation of frequency of body acceleration for Z direction</td> </tr>
<tr> <td>fbodyaccjerk.mean.x</td> <td>Mean frequency of body accerlation jerk for X direction</td> </tr>
<tr> <td>fbodyaccjerk.mean.y</td> <td>Mean frequency of body accerlation jerk for Y direction</td> </tr>
<tr> <td>fbodyaccjerk.mean.z</td> <td>Mean frequency of body accerlation jerk for Z direction</td> </tr>
<tr> <td>fbodyaccjerk.std.x</td> <td>Standard deviation frequency of body accerlation jerk for X direction</td> </tr>
<tr> <td>fbodyaccjerk.std.y</td> <td>Standard deviation frequency of body accerlation jerk for Y direction</td> </tr>
<tr> <td>fbodyaccjerk.std.z</td> <td>Standard deviation frequency of body accerlation jerk for Z direction</td> </tr>
<tr> <td>fbodygyro.mean.x</td> <td>Mean frequency of body gyroscope measurement for X direction</td> </tr>
<tr> <td>fbodygyro.mean.y</td> <td>Mean frequency of body gyroscope measurement for Y direction</td> </tr>
<tr> <td>fbodygyro.mean.z</td> <td>Mean frequency of body gyroscope measurement for Z direction</td> </tr>
<tr> <td>fbodygyro.std.x</td> <td>Standard deviation frequency of body gyroscope measurement for X direction</td> </tr>
<tr> <td>fbodygyro.std.y</td> <td>Standard deviation frequency of body gyroscope measurement for Y direction</td> </tr>
<tr> <td>fbodygyro.std.z</td> <td>Standard deviation frequency of body gyroscope measurement for Z direction</td> </tr>
<tr> <td>fbodyaccmag.mean</td> <td>Mean frequency of body acceleration magnitude</td> </tr>
<tr> <td>fbodyaccmag.std</td> <td>Standard deviation of frequency of body acceleration magnitude</td> </tr>
<tr> <td>fbodybodyaccjerkmag.mean</td> <td>Mean frequency of body acceleration jerk magnitude</td> </tr>
<tr> <td>fbodybodyaccjerkmag.std</td> <td>Standard deviation of frequency of body acceleration jerk magnitude</td> </tr>
<tr> <td>fbodybodygyromag.mean</td> <td>Mean frequency of magnitude of body gyroscope measurement</td> </tr>
<tr> <td>fbodybodygyromag.std</td> <td>Standard deviation of frequency of magnitude of body gyroscope measurement</td> </tr>
<tr> <td>fbodybodygyrojerkmag.mean</td> <td>Mean frequency of magnitude of body gyroscope jerk measurement</td> </tr>
<tr> <td>fbodybodygyrojerkmag.std</td> <td>Standard deviation frequency of magnitude of body gyroscope jerk measurement</td> </tr>
<tr> <td>activity</td> <td>The activity performed</td> </tr>