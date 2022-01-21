
# data

This is the data directory containing raw and tidy data.

    .
    ├── README.Rmd
    ├── README.md
    ├── raw
    │   ├── prolific_export_602aa93df732e9107ec837da_l2_reset.csv
    │   ├── prolific_export_602ab2f3ccc37d12d58ba79b_sp_mono.csv
    │   ├── prolific_export_60568611ec04488a9e9da93b_en_mono.csv
    │   ├── prolific_export_6057c54e64b0b4e5c128b4f9_l2_ne.csv
    │   ├── prolific_export_605b45a4d25f287719a41696_l2_e.csv
    │   ├── speech_rate_raw.csv
    │   └── speech_rate_raw_w_fillers.csv
    └── tidy
        ├── complete_2afc_tidy.csv
        ├── ddm_estimates.csv
        ├── ddm_sims.csv
        ├── learners_2afc_tidy.csv
        ├── learners_all_tasks_tidy.csv
        ├── learners_eq_tidy.csv
        ├── learners_lextale_tidy.csv
        ├── natives_2afc_tidy.csv
        ├── participants_removed_list.Rds
        ├── participants_returned_vector.Rds
        └── speech_rate_tidy.csv

The `speech_rate` files (raw and tidy) contain acoustic measurements of
the experimental items. The remaining tidy csv’s are the following:

-   **complete_2afc_tidy.csv**: combined learners and natives 2afc data
-   **ddm_estimates.csv**: Parameter estimates from the drift diffusion
    models
-   **ddm_sims.csv**: Simulations (n = 1000) pero condition for
    understanding drift rate and boundary separation estimates
-   **learners_2afc_tidy.csv**: learners 2afc data
-   **learners_all_tasks_tidy.csv**: learners data from all tasks
-   **learners_eq_tidy.csv**: learners empathy quotient data
-   **learners_lextale_tidy.csv**: learners LexTALE data
-   **natives_2afc_tidy.csv**: natives 2afc data
-   **participants_removed.list.Rds**: List of participants whose data
    was excluded
-   **participants_returned_vector.Rds**: Vector of participant IDs that
    were returned because they timed out
-   **speech_rate_tidy.csv**: Native speaker stimuli speech rate info
