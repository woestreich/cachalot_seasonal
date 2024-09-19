[![DOI](https://zenodo.org/badge/612036389.svg)](https://zenodo.org/badge/latestdoi/612036389)


# cachalot_seasonal

Data and code for Oestreich et al., 2024, Movement Ecology: "Evidence for seasonal migration by a cryptic top predator of the deep sea"

This project identifies and analyzes cachalot (sperm whale; Physeter macrocephalus) echolocation clicks found in 7+ years of near continuous recording from the Monterey Bay Aquarium Research Institute (MBARI) MARS hydrophone.

Full hydrophone data archive: https://www.mbari.org/data/passive-acoustic-data/  
MBARI Ocean Soundscape Team: https://www.mbari.org/team/ocean-soundscape/

The repository is organized as follows:

<ins>code</ins>

0_performance_assess: daily-resolution assessment of automated presence/absence processing as compared to manually-identified sperm whale clicks.

1_processing: daily-resolution processing of sperm whale click presence/absence

2_acoustics_analysis: statistics and visualization of daily-resolution presence/absence results and seasonality in detection range

3_simulations: simulations and visualization of individual-level movement strategies underlying acoustic detection results


<ins>outputs</ins>

Files and figures resulting from custom code described above.


<ins>data</ins>

BLED: daily tables of potential cachalot click detections. These tables are then processed to identify true positive sperm whale click sequences using inter-click-intervals of a constant, repetitive nature (code/1_processing/presence.R). This methodology has been assessed for performance and optimized for key parameters (code/0_performance_assess). 

manual_validation: tables containing results of manual assessment of sperm whale echolocation click presence/absence

detection_range: data for ambient noise, acoustic propagation loss, and detection range estimation. Some detection range and propagation loss modeling files exceed the GitHub file size limit but are available upon request.

recording_time.mat: matlab data file containing information on recording time and gaps

goa_perc.csv: data file containing Gulf of Alaska monthly sperm whale presence metrics estimated graphically from Diogou et al. (2019) and Mellinger et al. (2004), both cited in the manuscript text


<ins>A note on detections</ins>  
Detections were generated in Raven Pro v.1.6.3 (Cornell Lab of Ornithology), using a Band Limited Energy Detector (BLED) with the following parameters:

BLED signal calculation:  
Min. Frequency = 1.4 kHz  
Max. Frequency = 4.0 kHz  
Min. Duration = 8.125 ms  
Max. Duration = 32.5 ms  
Min. Separation = 32.5 ms  

BLED noise calculation:  
Block size = 2.0 s  
Hop size = 0.5 s  
Percentile = 20.0  

Signal-to-noise parameters:  
Min. Occupancy = 70.0%  
SNR Threshold = 5.0 dB  

Spectrogram calculation:  
Window = Hann  
Window Size = 512 samples  
Window Overlap = 95%  