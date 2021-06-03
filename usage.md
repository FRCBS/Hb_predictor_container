# Usage

Download
the docker image with command `docker pull toivoja/hb-predictor`.
A specific version (0.23) can be downloaded with `docker pull toivoja/hb-predictor:0.23`.

```docker container run -it --rm -p 8080:8080 toivoja/hb-predictor```  

Then go to the following address in the browser:

http://localhost:8080/hb-predictor

Kill the server (and the server) by pressing control-c.

If you want to run a specific version of the container, use the form
```docker container run -it --rm -p 8080:8080 toivoja/hb-predictor:0.23```  
The version of the container is shown in red in the top right corner of the web page.

The input data to the container is two dataframes: one for donors and the other for donations.
These dataframes are given as text files, where each column is separated by the pipe character '|'.
If requested, I may add a possibility to specify the used separator in the user interface.
The first line of each file should contain the variable names. The variables are described
in this Excel file https://github.com/FRCBS/Hb_predictor_container/blob/master/minimal_input.xlsx . 
Note that this format may still change a bit or get clarified.

The container will first preprocess the input data before passing it to the prediction algorithms. The above
Excel file also gives the description of the preprocessed variables that will be used in prediction.

Two generated example files exist that should get you started. The example file that contains donations is here
https://raw.githubusercontent.com/FRCBS/Hb_predictor_container/master/generated_example_donations_sanquin.data
And the donor example file is here https://raw.githubusercontent.com/FRCBS/Hb_predictor_container/master/generated_example_donors_sanquin.data
The example data contains 5600 donations from 200 donors. The Hb unit in the example data is "g/dL", but the
output of the container always uses the unit "g/L".
Running the example data set using all predictions methods
took 6 minutes on my machine. The results the from the predictor are non-sensical.
This is provided to demonstrate the format of the data, and to let you test the container and see the results.
If time permits, I may add later a more realistic generated data set.
