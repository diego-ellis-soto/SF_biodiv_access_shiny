FROM quay.io/jupyter/minimal-notebook:ubuntu-24.04

USER root

# R & RStudio (includes GDAL/PROJ/GEOS stack used by sf, terra)
RUN curl -s https://raw.githubusercontent.com/boettiger-lab/repo2docker-r/refs/heads/main/install_r.sh | bash
RUN curl -s https://raw.githubusercontent.com/boettiger-lab/repo2docker-r/refs/heads/main/install_rstudio.sh | bash

WORKDIR /code
COPY . .
RUN Rscript install.r

# Hugging Face Spaces expects port 7860
EXPOSE 7860

# Entry app (use app.R instead if that is your deployed entrypoint)
CMD ["R", "--quiet", "-e", "shiny::runApp('app_v2.R', host='0.0.0.0', port=7860)"]
