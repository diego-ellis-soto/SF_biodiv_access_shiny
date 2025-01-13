FROM quay.io/jupyter/minimal-notebook:ubuntu-24.04

USER root

# R & RStudio
RUN curl -s https://raw.githubusercontent.com/boettiger-lab/repo2docker-r/refs/heads/main/install_r.sh | bash
RUN curl -s https://raw.githubusercontent.com/boettiger-lab/repo2docker-r/refs/heads/main/install_rstudio.sh | bash

WORKDIR /code  
COPY . .
RUN Rscript install.r

CMD ["R", "--quiet", "-e", "shiny::runApp(host='0.0.0.0', port=7860)"]
