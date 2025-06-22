# dsa3101-2310-13-carpark
![carpark img](https://onecms-res.cloudinary.com/image/upload/s--hZPlgu7j--/c_fill,g_auto,h_468,w_830/f_auto,q_auto/stock-open-air-carpark---632010.jpg?itok=D8qKBfl6)

# Car Park Simulation Project: OptPark

## Overview

This project simulates a car park system, allowing users to analyze and visualize the parking dynamics based on various parameters such as capacity, sheltered space, arrival rates, and more.

## Objective:

- Determine Optimal Red-White Allocation
- Assess the impact of Carpark Closures

## Features

- Simulation of car park lots with different capacities and characteristics.
- Dynamic modeling of car arrivals and departures based .
- Visualization of utilization and usage metrics for analysis.

## Prerequisites

- The project is written in R, and you need to have R installed on your machine.

## File Structure

### Backend Folder:

The backend folder contains the following sub-folders:

- `src`: Contains the R scripts required to run the simulation models and update the parameters.
- `params`: Contains JSON files for storing the model parameters.

#### src folder

The src folder contains the following:

- `get_results.R`: Contains the functions for running single and multiple car park simulations.
- `arr_dur_functions.R`: Contains the fucntions to derive the arrival and duration values for the simulation models.
- `pipeline.R`: Contains function to update parameters based on new data provided by user.
- `compute_sim_params.R`: Contains the code used to compute the initial set of simulation parameters.
- `run.R`: Contains the code for the sample exceution of single and multi-carpark simulations.

Besides these files, the following subdirectories are also present:

- `data_experiments`: Contains R scripts for some of the initial experiments we ran, including preliminary analyis, pre-processing, closure analysis, EDA and plots.
- `old_scripts`: Contains the old R scripts we worked on, stored for future reference only.

#### params folder

The params folder contains the following:

- `arrival_params.json`: Stores parameters for computing arrival functions.
- `duration_params.json`: Stores parameters for computing duration functions.

### Frontend Folder:

The frontend folder contains the following:

- `app.R`: Contains the ui and server functions to run the app.
- `www`: Sub-folder containing the app dependencies (images and `style.css`)
- `data`: Sub-folder for storing frontend datasets.
- `uploaded_data`: Sub-folder for storing user uplaoded datasets to update params.

## Dockerization

This project has been dockerised. There are two dockerfiles and a compose file:

- `ui_dockerfile`: To set up the frontend container, install the packages and expose it to port 3838.
- `model_dockerfile`: To set up the backend container, install the packages and expose it to port 3839.
- `docker-compose.yml`: To get both containers to sart running.

To build the images and get the containers to start running, please run:

```
docker compose up
```

The application then serves at `http://localhost:3838/`

## Deployment using AWS

The docker container has also been deployed on an AWS instance (rg_demo_13).If you would like to run it, go to research gateway start the ubuntu container
- Click start
- Go to connect -> SSH/RDP
- Enter username: ubuntu
- Select Pem file (available in this repo): `dsa3101-2310-13-carpark.pem`
- Click submit to enter terminal
- run: `bash run_docker.sh`
- Go to `http://0.0.0.0:3838`

The home page would look as follows:

![ui img](https://drive.google.com/uc?id=14jY2Dw96qXAnW8bFyoQ4adX22eg6A-Qo)


## License

This project is licensed under the MIT License 
