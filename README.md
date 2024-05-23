## Overview

A working example of how to create a matsim population file.

The output is a xml file that can be used with matsim software.  
- Produces a population for the West of England Area.  
- To run a MATSim simulation you will aslo need a network file and a
config file. - Additional files such as public transport schedules can
also be used. - See matsim.org for details on how to use abm framework.

## Initial data requirements

#### Census csv data:

-   age profile
-   car availability
-   economic activity
-   ethnicity
-   industry

## Main R scripts

### 1. pop\_profile.R

This takes the census csv data and creates a summary data frame
containing the demographic attributes for each output area. In addition
the data frame contains an attribute for each plan type. Each OA has a
tally for the number of agents assigned to each plan type. The resulting
data frame is called profileTypeDF.

### 2. pop\_expander.R

The second script that is used to create the population xml file is the
pop\_expander script. This script takes the profileTypeDF data frame and
expands it out to create a longer data frame with each row corresponding
to an agent. The script also requires a random name generator function
to create unique agent names.

The script also allows you to sample a subset of the OAs to create a
smaller population file. This is useful for testing purposes.

(A tally of the number of agents who have each profile type is also
created.)

## Helper scripts

### help\_name\_gen.R

Various functions to generate random names for agents.
