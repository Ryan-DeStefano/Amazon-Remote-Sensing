# Amazon-Remote-Sensing
Project focused on tracking greenness trends of of plantations within the amazon rainforest aiming to find signals of deforestation and ultimately predict deforestation year. Research paper explaining all steps is located here, it is recommended to at least skim the paper before diving into the files contained in this repo:
(Will insert the link to the research paper later).



**LandsatTS Walkthrough Folder:**

Contains code walking through the usage of the R package, "LandsatTS". This package is the basis of the research and is being changed/adapted to the rainforest setting. Follows directly the steps laid out in https://github.com/logan-berner/LandsatTS?tab=readme-ov-file#4-quantify-growing-season-characteristics


**Helper Function Folder:**

Contains all helper functions used in the analysis files


**Brazil Folder:**

Contains the "Landsat Application", "ALOS Application", and "Plotting Plantations" R files. 

- The Landsat Application file contains all code for sampling and extracting sample points from the Landsat Satellites for plantations of interest in Brazil, as well as code to analyze the point measurements and obtain predicted deforestation years for each plantation of interest in Brazil.

- The ALOS Application file does the same as the Landsat Application file but for measumrents form the ALOS satellite.

- The Plotting Plantations file contains simple code to plot and take some sample points from the plantations of interest.

- Shapefiles for the plantations of interest are also included.


**Peru Folder:**

Contains the "Landsat Application", "ALOS Application", and "Plotting Plantations" R files. 

- The Landsat Application file contains all code for sampling and extracting sample points from the Landsat Satellites for plantations of interest in Peru, as well as code to analyze the point measurements and obtain predicted deforestation years for each plantation of interest in Brazil.

- The ALOS Application file does the same as the Landsat Application file but for measumrents form the ALOS satellite.

- The Plotting Plantations file contains simple code to plot and take some sample points from the plantations of interest.

- Shapefiles for the plantations of interest are also included.


**ALOS Folder:**

Contains files for extracting measurmenets from the ALOS satellite rather than the Landsat. 
