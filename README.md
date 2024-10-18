# SMT-2024

# Authors:

[Andy Hutchison](https://github.com/nunoor44), [Max Greaves](https://github.com/maxgreaves), [Ryan Dajani](https://github.com/RyanD17), [Shamar Phillips](https://github.com/ShamarP)

# Purpose:

This project was a submission to the SMT Data Challenge. It aims to tackle to question of whether stolen bases are the pitcher's fault or the catcher's fault, and provide tools that are easy to interpret that are also effective in tracking development.

# Data:

The data used for this project consisted of some time series files that tracked player movement and ball movement, and contained files that described game events and who was involved as well as the time of the event. This data was provided by SMT.

# Getting Started:

To begin working with this data it is important to run each file in order (ie Team102-code1.R first, then Team102-code2.R) since they build csvs that the next files are dependent on. The exception to this is Team102-supp-UI.R, which can be run independently.

# File Descriptions:

### Team102-supp-UI:

The files that have this prefix are files related to the UI. You can run the R file and it will produce the UI. The csv files are the results we display with the UI.

### Team102-code1.R:

This is our initial file it every file after builds off of it. This file requires the data provided by the competition.

### Team102-code2.R:

This file was used to calculate which plays are steals and calculate pitch time, pop time and exchange time in these steals

### Team102-code3.R:

This file calculates the location of the ball once it hits the strike zone on a steal play

### Team102-code4.R:

This file is where we create our base running "zone". As well as calculate when runners enter the "zone".

### Team102-code5.R:

This file is used to create various plots used in our paper.

### Team102-code6.R:

This file is used to create our tables highlighting the best pitchers and catchers in the system

### Team102-paper (1).pdf:

This is our final paper where we make our case on who is at fault when giving up a stolen base. What we deemed to be the most important graphs and figures are presented and described in detail within this paper as well.

