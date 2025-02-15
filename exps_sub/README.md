# Experiments for the submission to Elsevier RAS - Planning and Learning for Autonomous Robotics

This folder contains the experiments run for the submitted article to Planning and Learning for 
Autonomous Robotics. It is divided on two levels: 

- the folder KMS contains the experiments for the generation of the knowledge-base, 
- the folder Planner contains the results of the experiments run for the different tests.

## KMS

The folder is divided into the 2 examples: blocks world and arch. The former has 5 examples and
the latter 2. Each example contains:

- the `query` folder, where the text files for the queries used for testing are stored;
- the folders for the models.

For instance, folder `KMS/arch/1/4o` contains the results of running the system with GPT-4o.

Inside each folder of the model, one can find:

- A small report on the output of the model;
- The folder `kb` containing the generated knowledge-base and, if there were error, the corrected 
  one;
- The folder `output`, which contains the output of the LLM model for the 3 different steps 
  (validation, high-level generation and low-level generation).

## Planner

Inside the folder `Planner`, one can find two CSV files:

- `results_prolog.csv` contains the results for the tests run using directly the SWIpl interpreter 
  from command line. Inside the file, each row is composed of 4 elements: knowledge-base file used,
  time to find the high-level total-order plan, time to apply the mappings and time to extract the
  enablers and find the resources. Each test was run 50 times for better statistical purposes.

- `results_python.csv` contains the results for the tests run using the Python3 interpreter. Each 
  row contains three elements: the knowledge-base file used, the time to find a plan and extract 
  the enablers by calling the pyswip module and finally the time to extract the behaviour tree.
  Also in this case, the tests were run 50 times.