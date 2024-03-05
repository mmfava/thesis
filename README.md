# Eco-Epidemiology Research on Lonomism in South America

This repository is the heart of my doctoral thesis, where I'm conducting a deep dive into the eco-epidemiology of Lonomia achelous and Lonomia obliqua envenomations (lonomism) in South America.  I'm analyzing data spanning six decades (1960-2020) to uncover risk factors, host-insect relationships, and the geographical spread of these venomous caterpillars.

## What you'll find here

- Epidemiological Analysis: I've analyzed over 13,000 case reports to reveal trends, high-risk areas, and the demographics most affected by lonomism.
- Ecological Explorations: I'm examining predation, relationships between Lonomia spp. and their host plants, and using ecological niche modeling to map lonomism risk zones across the continent.
- Public Health and Conservation Focus: My goal is to support informed public health strategies and biodiversity conservation efforts by understanding the ecology of these species.
- Work-in-Progress: Some aspects of my analysis are ongoing. I welcome your feedback and collaboration!

# Environment Setup

1. **Install Docker**

    To ensure reproducibility and simplify the analysis process, we use a Docker container. This eliminates potential compatibility issues across different operating systems.

    Begin by installing Docker on your system (Windows, macOS, or Linux). Follow the official installation instructions at https://docs.docker.com/get-docker/

2. **Clone the Project Repository**
   
   Open a Terminal (macOS/Linux) or Command Prompt (Windows) and clone this repository:
   
   ```bash
   git clone https://github.com/mmfava/thesis.git
   ```

3. **Prepare the Docker Environment**:
   
   Navigate to the root directory of the cloned repository (thesis) and execute the following commands:
   
   ```bash
   docker build -t lonomia-notebook-image .
   docker run -p 8888:8888 lonomia-notebook-image
   ```
   
   > This maps port 8888 of your local machine to port 8888 within the container, allowing you to access the "lonomia" environment.

4. **Install R**

    Download and install R version 4.3.2 from the official R project website: https://cran.r-project.org/bin/windows/base/R-4.3.2-win.exe
    > You can install R using CONDA or any other method that suits your setup, but ensure it matches the version requirements to avoid compatibility issues.

5. **Install Required R Packages**:
   
   The `requirements.R` script within the repository lists all necessary R packages. Run this script within the "lonomia" Docker environment to install dependencies.

**Note**: While these instructions generally apply to Windows, macOS, and Linux, there may be minor variations in how to open a terminal or command prompt between these operating systems.

### Data Acquisition

Interested parties in complete data can request data via email:
- <a href="mailto:biologist.mmf@gmail.com">biologist.mmf@gmail.com</a>
- <a href="mailto:marilia.melo.favalesso@gmail.com">marilia.melo.favalesso@gmail.com</a>

## Repository Structure:

- \data: Data (accuracy changed and sensitive data removed)
- \images: Images resulting from the analyzes.
- \scripts: R scritps with analysis of the chapters.
- Dockerfile
- requirements.R

## License
[MIT License](LICENSE)

