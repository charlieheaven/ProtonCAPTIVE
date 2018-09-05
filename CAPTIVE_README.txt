NAME
	app.R
	Program designed to display experimental chromosome aberration data in graphical format

VERSION
	1.1

SYNOPSIS
	R app.R

DESCRIPTION
	This program inputs a file called CombinedData.txt which contains known experimental values for chromosome aberrations and puts them into an interactive graphical format which can take in more data sets in csv or tab delim format.

The program has the ability to customise the graph with the following options:
x_val : Option to plot either LET or Dose on the X axis with the other being asigned to a colour scale
LET   : LET range to display
Dose  : Dose range to display
Organism : Checkboxes of different organism data loaded
Paper    : Select papers to be shown

App also has the option to upload files into the dataset with the formatting below. These files can then be submitted to the input file directory with a date and time stamp for viewing at a later date in a csv format. If the "Submit input file to directory" button is selected before the dataset is uploaded, the uploaded data will be automatically saved on upload.

FILE FORMAT:
	File types: text/plain or text/csv
Files must have the following columns:
Paper, LET, Dose, Cells Scored, Dicentrics per Cell, No of Chromosomes, Particle Type, Energy of Particle, Organsim, Cell Type, Cell Line, Gender
Data may be blank with the exception of Particle Energy, LET, Dose and Dicentrics per Cell as this will just read in as NAs.
File not in this format will not be uploaded to the app.

BUGS
	No known bugs

AUTHOR
	Charlotte Heaven
