FROM rocker/r-ver

#ENV RENV_VERSION=v1.0.2
#RUN R -e "install.packages('remotes')"
#RUN R -e "install.packages(c('timetk'), repos='https://cloud.r-project.org/')"
#RUN R -e "remotes::install_github('rstudio/renv@${RENV_VERSION}')"
#RUN R -e "remotes::install_github('rstudio/renv')"

#RUN R -e "options(renv.config.repos.override = 'https://packagemanager.posit.co/cran/latest')"

COPY . /app

WORKDIR /app

RUN R -e "options(renv.config.repos.override = 'https://packagemanager.posit.co/cran/latest'); renv::restore()"

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('./app.R', host='0.0.0.0', port=3838)"]