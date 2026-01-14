# From Coast to Coast: Code, Data and Ouputs

This is the code to run Particle Swarm Optimization to obtain initial parameter estimates followed by a Non-Linear Least Squares final estimation.

<img width="637" height="322" alt="procedure_explanation" src="https://github.com/user-attachments/assets/b2e27403-72a2-46d9-91ba-35174ee3d72c" />

To run the python component of this code, install [Anaconda](https://www.anaconda.com/docs/getting-started/anaconda/install) and create an environment with the required packages using the following:

`conda create --name <env> --file requirements.txt`

To run the R final estimation code, install [RStudio](https://posit.co/download/rstudio-desktop/) and the necessary libraries from within it.

Order of running files for the zonal model:

- izev_analysis.ipynb
- Viewing_new_data.ipynb # Poorly named, actually generates file that allows combining CSD and zonal data
- Bass_Model_Data_Prep4.ipynb
- Bass_Model_Parallelized_numba_zonal_sales.py
- Bass_Model_All_p_q_m_provided_with_plotting_gof_05_01_26.R
- ClimateData.ipynb
- Merging_census_zones.ipynb
- charging_stns_per_area.ipynb
- Zonal_search_distances.ipynb
- Regressing_Params_12_01_26.R

Running order for All Canada Gompertz model:

- Validating_Data.ipynb
- Bass_Model_Parallelized_numba_m_estimated_all_canada.py
- Bass_Model_All_p_q_m_provided_all_canada_with_plotting_gof_pqmsim_10_01_26.R
- Simulation_10_01_26.ipynb

Running order for All Canada Logistic Model:

- Validating_Data.ipynb
- Logistic_Model_Parallelized_numba_m_estimated.py
- Logistic_Model_All_p_q_m_provided_all_canada_with_plotting_gof_pqmsim_10_01_26.R
- Simulation_05_01_26.ipynb
