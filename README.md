# How to run VPR plotting shiny app
### E. Chisholm, January 13th 2021

## Required scripts:
https://github.com/Echisholm21/VPR_plotting
The entire github directory should be cloned onto your local machine for best results

## Required data:
from the ‘rois’ directory of a typical VP structure
- VPR CTD data files (eg. h##ctd.dat)
- roi image folders (only required if you wish to visualize images)

## Step by Step:
__1) Opening the app__
- open VPR_plotting in RStudio
- open the script ‘shiny_vprr.R’
- click ‘Run App’ in top right corner of script window (where it would typically say ‘Source’), clicking the arrow next to this button will allow you to select between opening the app in a separate window or opening the app in the viewer window within RStudio.
__2) Uploading data__
- There are still some major bugs in the app and so it is best to update your basepath in the actual script itself, to avoid any errors when switching between directories. The default is currently ‘C:/data’
- This can be done by updating the ‘value’ argument on line 47 of ‘shiny_vprr.R’ where a text input is set as the basepath
- Once this is done the app can be reload by using the ‘Reload App’ button in the top right corner of the main script window (where you previously clicked ‘Run App’)
- It is important for this app that your directory structure follow the VP requirements exactly. For example within C:/data looks like this

	 - C:/data/IML2018051/rois/vpr0/d286/h22
   - C:/data/IML2018051/rois/vpr0/d286/h23
   - C:/data/IML2018051/rois/vpr0/d286/h22ctd.dat
   - C:/data/IML2018051/rois/vpr0/d286/h23ctd.dat

	`Development Note: This is bug is because updating the basepath without updating other information in the menu, causes the app to crash. In order to solve this, an action button should be used so that plots are only updated  once all metadata fields in the side menu have been updated. `

- Once your basepath is accurate, select a CTD file to load using the ‘Browse’ button under CTD Files in the left-hand app menu
	- This file should be in your basepath and should follow the format of h##ctd.dat
- The app will automatically load the CTD data based on the default column names which are specified in vprr
	**Development Note: If users would like to update the column names for the CTD .dat files, they should be able to do so. Some newer VPR models may output CTD files with different columns and this will need to be accounted for in the code.
- Update the rest of the metadata fields in the left-hand app menu to reflect the CTD data file you have just loaded (until these fields are accurate you will get an error where the plots are meant to be.
	- Only Cruise ID, VPR Tow ID, Day, Hour, Image Volume and Bin Size are mandatory (Station and Event ID are optional)

    `Development note: This error will also be fixed with the action button implementation, to delay updating plots until all metadata fields are updated.`

- You should now be able to see 4 plot panels in the app, the top one showing profile plots of salinity, temperature, density, fluorescence and concentration of ROIs, then 3 section plots of ROI concentration along the VPR path over, concentration of ROIs, temperature, and salinity.
- On the Summary tab you should see a summary of the VPR data displayed in the plots, including variable ranges and number of data points
- The Table tab should display all the data being plotted as a dataframe
- Note that if you change the range sliders in the plotting tab (for example to only display data from depths of 0-50m, the summary and table tabs will also reflect these changes

__3) Viewing Images__
- Once your CTD data is loaded in the plotting tab you can click to the Images tab to display a gallery of the ROI images being plotted.
- Click ‘Chose ROI directory’, this will bring up an upload pop up which allows you to select a directory of images
- Navigate through your basepath to the directory of images associated with your CTD data (eg. an ‘hour’ folder, like C:/data/IML2018051/rois/d286/h22)
- Your ROI images should display in the image gallery after a short (sometimes longer) loading time
- Each image will be scaled and have a red numeric ROI identifier printed in the top right corner. You can control the number of ROIs displayed by editing the value in ‘Number of images shown’, note that only one page of images is shown to prevent the app from being overloaded. If you would like to narrow down the images shown I would suggest editing the scales in the left hand app menu to define the range of data displayed (Time Range and Pressure Range may be particularly useful here).
