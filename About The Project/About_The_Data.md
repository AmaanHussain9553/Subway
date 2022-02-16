**About the Data:**
The data was taken from the Chicago Data Portal at: https://data.cityofchicago.org/Transportation/CTA-Ridership-L-Station-Entries-Daily-Totals/5neh-572f
The data is about all CTA train stations in Chicago and is aggregated based on daily statistics. For each station on each day, it gives us the station ID, day type, and number of rides on that day. 

Since my project is only looking at the stations by name, date, and the number of rides I would subset all the data by those 3 columns in order to process and display information more efficiently. I would also have to sync the side menu bar with the reactive objects that help the data so that as the user chooses a different year or a different train station 

In order to make sure that I could display the data in terms of number of rides based on day of the week I added a new column where each day’s worth of day had a day of the week and would aggregate the column accordingly in order to display the data table and bar plot. 

Similarly, for the data in terms of monthly average based on number of rides per month I created a new column that had the month associated with each of the days’ worth of data and would aggregate it accordingly in order to display the data table and the bar plot. 
