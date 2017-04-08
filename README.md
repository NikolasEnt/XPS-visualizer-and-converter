# XPS visualizer and converter

This app was developed to make it easier to convert raw .xy files from X-ray Photoelectron spectrometer PHOIBOS analyzers with angular channels (SpecsLab Software) to .csv files, calculate cumulative spectrum from a batch of channels and save only the desired region of interest.

## **[Go to the web app](https://nikolasent.shinyapps.io/XPS_visualizer_and_converter/)**

![Screenshot](/images/screen.png)

*Sample screenshot of the application*

## Run the app
### Web application
The most recent app release is available [online](https://nikolasent.shinyapps.io/XPS_visualizer_and_converter/) as a Shiny app.

Some examples of input files are included in the *[example](./example)* directory.
### Local application
The app could be used locally. It needs *R* (and [Rstudio](https://www.rstudio.com/), optionally) and the following libraries:
```R
shiny
ggplot2
reshape2
scatterplot3d
```

## Authors
- __Nikolay Falaleev__ - *The main author* Github: [NikolasEnt](https://github.com/NikolasEnt), Twitter: [@NFalaleev](https://twitter.com/NFalaleev)

## License
The project source is distributed under GNU v.3 license. See the [LICENSE](LICENSE) file for details.

## Change log
- 1.1.1 Add optional normalisation of intensity into [0,1] range
- 1.1.0 New 3D style of AR PES plots, batch file process
- 1.0.0 Initial upload

## Known bugs
- After detailed examination of individual spectrum, please, reload the app (F5), before uploading a new one, as there could be issues with number of angular channels.
