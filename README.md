### What is this repository for? ###

This Software extends the analysis possibilities of [ILIAS](ilias.de) an e-assessment system. It provides the following features:

* computation of grades (variable schema)
* computation of Item-total correlation and difficulty for questions
* generate some diagrams

**Limitations**

* only supports single choice questions

### How do I get set up? ###

You need [R](http://www.r-project.org/) and the following R-libs:

* psych
* string
* R.utils
* XML
* [rCharts](https://github.com/ramnathv/rCharts)
* digest
* plyr

rCharts must be installed according to the installation instructions in the GitHub repository.
The other libraries should be available as packages and can be installed directly.

### How do I use it? ###

* In ILIAS, go the export tab of your test and download the CSV as well as the XML file and place them into the `input` directory
* have a look at the main.R, it shows the usage of the different functions
* run the main.R file
* the input files are backed up into the `output` directory
* all diagrams and other output is placed inside the `output` directory

### Suggestions? ###

* want to have support for other question types?
* need more metrics?
* Please raise a BitBUcket issue or - even better - fork & implement your favourite feature yourself. This is free software!
