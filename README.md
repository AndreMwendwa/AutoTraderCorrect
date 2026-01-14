# Autotrader

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
