FROM rocker/r-ver

#RUN R -e "install.packages('remotes')"
#RUN R -e "install.packages(c('timetk'), repos='https://cloud.r-project.org/')"
#RUN R -e "remotes::install_github('rstudio/renv@${RENV_VERSION}')"
#RUN R -e "remotes::install_github('rstudio/renv')"

#RUN R -e "options(renv.config.repos.override = 'https://packagemanager.posit.co/cran/latest')"

COPY . /app

WORKDIR /app

RUN R -e "options(renv.config.repos.override = 'https://packagemanager.posit.co/cran/latest'); renv::restore()"

EXPOSE 8080 #GCP

CMD ["R", "-e", "shiny::runApp('./app.R', host='0.0.0.0', port=8080)"] # GCP