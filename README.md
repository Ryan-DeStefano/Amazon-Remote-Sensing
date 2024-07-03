# Amazon-Remote-Sensing
Project focused on tracking greenness trends of of plantations within the amazon rainforest aiming to find signals of deforestation and ultimately predict deforestation year. Research paper explaining all steps is located here:
(Will insert the link to the research paper later).


**LandsatTS Walkthrough Folder:**

Contains code walking through the usage of the R package, "LandsatTS". This package is the basis of the research and is being changed/adapted to the rainforest setting. Follows directly the steps laid out in https://github.com/logan-berner/LandsatTS?tab=readme-ov-file#4-quantify-growing-season-characteristics

**Brazil Folder:**

Main file for this folder is "Brazil Landasat Application.qmd". Contains code to:
- Extract evi measurements from landsat satellites for specified points within specified plantations of interest
- Calibrate the evi measurmenets from multiple Landsat satellites
- Fit splines to the time series of evi measurmeents dating back to 1984.
- Plots that summarize the overall trends of evi in these plantations

**Peru Folder:**

Main file for this folder is "Peru Landasat Application.qmd". Contains code to:
- Extract evi measurements from landsat satellites for specified points within specified plantations of interest
- Calibrate the evi measurmenets from multiple Landsat satellites
- Fit splines to the time series of evi measurmeents dating back to 1984.
- Plots that summarize the overall trends of evi in these plantations

**Helper Functions Folder:**

Contains files for specifying the number of points to sample from each of the plantations and updated version of the lsat_fit_phenological_curves LandsatTS package function.

**ALOS Folder:**

Contains files for extracting measurmenets from the ALOS satellite rather than the Landsat. 
Note: Not completely working currently.

