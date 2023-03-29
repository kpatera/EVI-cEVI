# EVI-cEVI
EVI-cEVI: the Epidemic Volatility and the convergence Epidemic Index as an early-warning tool for epidemic waves

## Installation

To install the current source from GitHub use:

    install.packages(c("devtools", "remotes"))
    require(devtools)
    require(remotes)
    remotes::install_github("kpatera/EVI-cEVI")
    

To install a stable version from the drat repository use:

    ## Will be added once a stable version is available

## Basic functions of EVI package

    ...

To load some example data:

    data("Italy")
    
To run EVI analysis on the example data:

    deviant(Italy$Cases)

To run EVI analysis when you already have historical data and need an update with the influx of new data you do the following: first you analyze the historical data. Let’s say we have first observed only the 148 cases from the Italian data. We initially run the deviant function:

    deviant(Italy$Cases[1:148]) # Runs EVI
    deviant(Italy$Cases[1:148], method="cEVI") # Runs cEVI

As a new observation (or observations) comes in we need to update our output file by adding the EVI output for the new case(s) as a new row(s). This is done by using the deviant_update function: 

    deviant_update(Italy$Cases[1:149]) # Updates based on EVI
    deviant_update(Italy$Cases[1:149], method="cEVI") # Updates based on cEVI

This has as a result an updated output file (the “EVI_output” file) with 149 rows now, after the addition of the row from the analysis of the newly observed data.

To create a plot of the analysed data: 

    evi.graphs(EVI_output)
    
To create a combined plot of two indeces use the following code: 

    overlap(EVI_output,cEVI_output)
    
The basic two functions of the EVI analysis are deviant() and evi.graphs(). For help on these functions type:  
    
    ?deviant
    ?deviant_update
    ?evi.graphs
    
## Troubleshooting

In case an error during download occur try the following

    remotes::install_github("kpatera/EVI-cEVI", force = TRUE, dependecies = TRUE)



