# XPS visualizer and converter

This app was developed to make it easier to convert raw .xy files from X-ray Photoelectron spectrometer PHOIBOS analyzers with angular channels (SpecsLab Software) to .csv files, calculate cumulative spectrum from a batch of channels and save only the desired region of interest.

**[Go to the web app](https://nikolasent.shinyapps.io/XPS_visualizer_and_converter/)**

![Screenshot](/images/screen.png)

*Sample screenshot of the application*

## Run the app
The most recent app release is available [online](https://nikolasent.shinyapps.io/XPS_visualizer_and_converter/) as a Shiny app.

Some examples of input files are included in the "example" directory.

## Authors
- __Nikolay Falaleev__ - *The main author* [Github](https://github.com/NikolasEnt) [Twitter](https://twitter.com/NFalaleev)

## License
The project source is distributed under GNU v.3 license. See the [LICENSE](LICENSE) file for details.

## Known bugs
After detailed examination of individual spectrum, please, reload the app, before uploading a new one, as there could be issues with number of angular channels.
