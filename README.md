# F1000R_BiocWorkflows
Project for publishing from Bioconductor to F1000R and back

Currently this is intended to be a place to provide support for transitioning workflow manuscripts submitted to the [F1000Research Bioconductor channel](http://f1000research.com/channels/bioconductor) into executable packages hosted on Bioconductor.

## Structure

At the moment there is a separate folder for each workflow, named after a corresponding author.  

Under this is there is a folder named **originals**, holding the initial set of workflow files provided by the author.  This can be pretty much anything, but some combination of *Rmd*, *Rnw*, *LaTeX*, and *txt* is expected.  Any raw data files, pre-rendered images, style files etc are also found here.  The idea is not to edit these, but the can serve as a reference for how submitted manuscripts have been constructed.

The is also a second folder representing an **R** package for the workflow.  A temporary name based on the content has been choosen, but this can of course be decided by the author.  The intention is to get each workflow package into a shape where it can be submitted to the Bioconductor SVN and DocBuilder App ([https://www.bioconductor.org/developers/how-to/workflows/](https://www.bioconductor.org/developers/how-to/workflows/)).