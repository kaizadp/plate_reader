# plate_reader
21 May 2020

This code will process data from the plate reader and calculate concentrations using the corresponding calibration curves.  
### INPUT
Save all data *and calibration* files as `.xlsx` files in the `Data` folder.  
The calibration files must be named same as the corresponding data files, and appended with a `-cc` at the end.  

### PROCESSED
Processed files will be saved as `.xlsx` files in the `processed` folder.  
Do **not** delete files from this folder once completed.  

The script will identify and run only the newly added files, by comparing the input and processed files.
