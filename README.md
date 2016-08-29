# NASA-Data-Mining

# Requirements Documents

Python script that takes Element or System Requirements Document .txt file as input and outputs a .csv file. 

Extracts requirements from ERD/SRD, flags requirement fields for numeric measurements and given keywords, and records these requirements and flagged values to a spreadsheet.

## System Requirements 

Python 3.5.1 installed

## Configuration 

* Convert .doc files to .txt files 
	# Can be completed in Microsoft Word
		* Save as Plain Text (.txt)
		* In following File Conversion pop-up, choose text encoding Unicode (UTF-8)
* Store .txt files and .py script in same directory


Optional additional configuration:

* To change list of numeric measurements flagged for, update string list variable flag_num in script
* To change list of keywords flagged for, update string list variable flag_txt in script


## Execution

* From terminal command line in directory containing documents and script, run script with command: python txt_to_csv_miner.py
	* Current directory: NASA_ARES_documents
* Script will request required user input
	* For "Enter .txt filename:", enter filename of input .txt file including .txt extension
		* Example for NASA_ARES_documents: AI1-SYS-SRD v4.12.txt
		* Note: file must be in same directory as script for script to run 
	* For "Enter .csv filename:", enter filename of output .csv file including .csv extension
		* Example for NASA_ARES_documents: AI1-SYS-SRD v4.12.csv
		* Note: file doesn't exist yet, will be created by script
	* For "Enter document keyword:", enter acronym that precedes each requirement in document
		* Example for NASA_ARES_documents: FTV
		* Note: case-sensitive
* When finished, terminal will indicate that the file has been written, and new .csv file with given filename will be present in directory


Note: I had to remove the NASA_ARES_documents directory due to security clearance issues (the documents are not pubicly accessible). 


# Flight Incidence Reports 

R script that predicts the observed incidence altitude from flight incidence report narratives. Extracts list of potential altitudes using regular expressions to extract numbers in feet and FL measurements and from sentences containing "climbing" and "passing". From these, identifies the value with the highest frequency throughout the narratives as the most likely to be the observed altitude.
