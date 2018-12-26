FROM library/debian

LABEL description="Docker image for RHAPSODY WP3-PreDiabetes" \
  maintainer="mickael.canouil@cnrs.fr"
  
RUN apt-get update && apt-get install -y --no-install-recommends \
    apt-utils \
    nano \
    bash-completion \
    ca-certificates \
    file \
    fonts-texgyre \
    g++ \
    gfortran \
    gsfonts \
    libblas-dev \
    libbz2-1.0 \
    libcurl3 \
    libicu57 \
    libjpeg62-turbo \
    libopenblas-dev \
    libpangocairo-1.0-0 \
    libpcre3 \
    libpng16-16 \
    libreadline7 \
    libtiff5 \
    liblzma5 \
    locales \
    make \
    unzip \
    zip \
    zlib1g \
    curl \
    libxml2-dev \
    libssl-dev \
    libssh2-1-dev \
    libudunits2-dev \
    libgdal-dev \
    imagemagick \
    libmagick++-dev \
    default-jdk \
    libbz2-dev \
    libcairo2-dev \
    libcurl4-openssl-dev \
    libpango1.0-dev \
    libjpeg-dev \
    libicu-dev \
    libpcre3-dev \
    libpng-dev \
    libreadline-dev \
    libtiff5-dev \
    liblzma-dev \
    libx11-dev \
    libxt-dev \
    perl \
    tcl8.6-dev \
    tk8.6-dev \
    texinfo \
    texlive-extra-utils \
    texlive-fonts-recommended \
    texlive-fonts-extra \
    texlive-latex-recommended \
    texlive-full \
    x11proto-core-dev \
    xauth \
    xfonts-base \
    xvfb \
    zlib1g-dev \
    git \
    libapparmor1 \
    libedit2 \
    lsb-release \
    psmisc \
    python-setuptools \
    sudo \
    wget \
    multiarch-support \
    ssh \
    automake \
    autoconf \
    pkg-config \
  && apt-get autoremove -y

RUN sed -i '/^#.* en_US.* /s/^#//' /etc/locale.gen \
  && locale-gen \
  && export LANG="en_US.UTF-8" \
  && export LANGUAGE="en_US.UTF-8" \
  && export LC_CTYPE="en_US.UTF-8" \
  && export LC_NUMERIC="en_US.UTF-8" \
  && export LC_TIME="en_US.UTF-8" \
  && export LC_COLLATE="en_US.UTF-8" \
  && export LC_MONETARY="en_US.UTF-8" \
  && export LC_MESSAGES="en_US.UTF-8" \
  && export LC_PAPER="en_US.UTF-8" \
  && export LC_NAME="en_US.UTF-8" \
  && export LC_ADDRESS="en_US.UTF-8" \
  && export LC_TELEPHONE="en_US.UTF-8" \
  && export LC_MEASUREMENT="en_US.UTF-8" \
  && export LC_IDENTIFICATION="en_US.UTF-8" \
  && export LC_ALL="en_US.UTF-8" \
  && git config --system credential.helper 'cache --timeout=3600' \
  && git config --system push.default simple \
  && sed -i 's/^#PermitRootLogin prohibit-password/PermitRootLogin no/' /etc/ssh/sshd_config
 
ENV R_VERSION=3.4.2 \
  RSTUDIO_VERSION=1.1.456 \
  LANGUAGE=en_US.UTF-8 \
  LANG=en_US.UTF-8 \
  PATH=/usr/lib/rstudio-server/bin:$PATH

RUN useradd rhapsody \
  && echo "rhapsody:wp3" | chpasswd \
  && addgroup rhapsody rhapsody \
  && addgroup rhapsody root \
  && adduser rhapsody sudo \
  && usermod -g rhapsody rhapsody
  
RUN mkdir -p /home/rhapsody/.rstudio/monitored/user-settings \
  && chown -R rhapsody:rhapsody /home/rhapsody \
  && echo 'alwaysSaveHistory="0" \
    \ncleanTexi2DviOutput="1" \
    \ncleanupAfterRCmdCheck="1" \
    \ncranMirrorCountry="us" \
    cranMirrorHost="RStudio" \
    \ncranMirrorName="Global (CDN)" \
    \ncranMirrorUrl="http://cran.rstudio.com/" \
    \ncustomShellCommand="" \
    \ncustomShellOptions="" \
    \ndefaultTerminalShell="7" \
    \nenableLaTeXShellEscape="1" \
    \nerrorHandlerType="1" \
    \nhideObjectFiles="1" \
    \nlineEndingConversion="1" \
    \nloadRData="0" \
    \nnewlineInMakefiles="1" \
    \nremoveHistoryDuplicates="0" \
    \nrestoreLastProject="0" \
    \nreuseSessionsForProjectLinks="1" \
    \nrprofileOnResume="0" \
    \nsaveAction="0" \
    \nsecurePackageDownload="1" \
    \nshowLastDotValue="0" \
    \nshowUserHomePage="sessions" \
    \nuiPrefs="{\\n    \"always_complete_characters\" : 3,\\n    \"always_complete_console\" : true,\\n    \"always_complete_delay\" : 250,\\n    \"always_enable_concordance\" : true,\\n    \"ansi_console_mode\" : 1,\\n    \"auto_append_newline\" : true,\\n    \"auto_expand_error_tracebacks\" : false,\\n    \"auto_run_setup_chunk\" : true,\\n    \"background_diagnostics_delay_ms\" : 2000,\\n    \"blinking_cursor\" : true,\\n    \"busy_detection\" : 0,\\n    \"busy_whitelist\" : [\\n        \"tmux\",\\n        \"screen\"\\n    ],\\n    \"check_arguments_to_r_function_calls\" : false,\\n    \"clear_hidden\" : false,\\n    \"code_complete\" : \"manual\",\\n    \"code_complete_other\" : \"manual\",\\n    \"continue_comments_on_newline\" : false,\\n    \"default_encoding\" : \"UTF-8\",\\n    \"default_latex_program\" : \"pdfLaTeX\",\\n    \"default_project_location\" : \"~\",\\n    \"default_sweave_engine\" : \"knitr\",\\n    \"diagnostics_in_function_calls\" : true,\\n    \"diagnostics_on_save\" : true,\\n    \"doc_outline_show\" : \"show_sections_only\",\\n    \"enable_background_diagnostics\" : true,\\n    \"enable_emacs_keybindings\" : false,\\n    \"enable_rsconnect_publish_ui\" : true,\\n    \"enable_snippets\" : true,\\n    \"enable_style_diagnostics\" : false,\\n    \"execution_behavior\" : \"statement\",\\n    \"flat_theme\" : \"default\",\\n    \"focus_console_after_exec\" : false,\\n    \"fold_style\" : \"markbegin\",\\n    \"font_size_points\" : 10,\\n    \"git_diff_ignore_whitespace\" : false,\\n    \"handle_errors_in_user_code_only\" : true,\\n    \"hide_console_on_chunk_execute\" : true,\\n    \"highlight_code_chunks\" : true,\\n    \"highlight_r_function_calls\" : false,\\n    \"highlight_selected_line\" : false,\\n    \"highlight_selected_word\" : true,\\n    \"ignore_uppercase_words\" : true,\\n    \"ignore_words_with_numbers\" : true,\\n    \"insert_matching\" : false,\\n    \"insert_numbered_latex_sections\" : true,\\n    \"insert_parens_after_function_completion\" : false,\\n    \"insert_spaces_around_equals\" : true,\\n    \"latex_preview_on_cursor_idle\" : \"always\",\\n    \"navigate_to_build_error\" : true,\\n    \"num_spaces_for_tab\" : 2,\\n    \"packages_pane_enabled\" : true,\\n    \"pane_config\" : {\\n        \"consoleLeftOnTop\" : false,\\n        \"consoleRightOnTop\" : true,\\n        \"panes\" : [\\n            \"Source\",\\n            \"TabSet1\",\\n            \"Console\",\\n            \"TabSet2\"\\n        ],\\n        \"tabSet1\" : [\\n            \"Environment\",\\n            \"History\",\\n            \"Connections\",\\n            \"VCS\",\\n            \"Presentation\"\\n        ],\\n        \"tabSet2\" : [\\n            \"Files\",\\n            \"Plots\",\\n            \"Packages\",\\n            \"Help\",\\n            \"Build\",\\n            \"Viewer\"\\n        ]\\n    },\\n    \"pdf_previewer\" : \"rstudio\",\\n    \"preferred_document_outline_width\" : 110,\\n    \"print_margin_column\" : 80,\\n    \"reindent_on_paste\" : false,\\n    \"restore_source_documents\" : false,\\n    \"rmd_chunk_output_inline\" : true,\\n    \"rmd_preferred_template_path\" : \"\",\\n    \"rmd_viewer_type\" : 1,\\n    \"root_document\" : \"\",\\n    \"save_before_sourcing\" : true,\\n    \"save_files_before_build\" : false,\\n    \"scroll_past_end_of_document\" : false,\\n    \"show_diagnostics_cpp\" : true,\\n    \"show_diagnostics_other\" : true,\\n    \"show_diagnostics_r\" : true,\\n    \"show_doc_outline_rmd\" : false,\\n    \"show_help_tooltip_on_idle\" : false,\\n    \"show_indent_guides\" : true,\\n    \"show_inline_toolbar_for_r_code_chunks\" : true,\\n    \"show_invisibles\" : false,\\n    \"show_line_numbers\" : true,\\n    \"show_margin\" : false,\\n    \"show_publish_diagnostics\" : false,\\n    \"show_publish_ui\" : false,\\n    \"show_signature_tooltips\" : false,\\n    \"soft_wrap_r_files\" : false,\\n    \"source_with_echo\" : false,\\n    \"spelling_dictionary_language\" : \"en_GB\",\\n    \"strip_trailing_whitespace\" : false,\\n    \"surround_selection\" : \"never\",\\n    \"syntax_color_console\" : false,\\n    \"tab_multiline_completion\" : false,\\n    \"terminal_autoclose\" : true,\\n    \"terminal_local_echo\" : true,\\n    \"terminal_track_env\" : true,\\n    \"terminal_websockets\" : false,\\n    \"theme\" : \"Cobalt\",\\n    \"toolbar_visible\" : true,\\n    \"truncate_long_lines_in_console\" : 1000,\\n    \"use_dataimport\" : true,\\n    \"use_rcpp_template\" : true,\\n    \"use_roxygen\" : false,\\n    \"use_spaces_for_tab\" : true,\\n    \"use_vim_mode\" : false,\\n    \"valign_argument_indent\" : true,\\n    \"warn_if_no_such_variable_in_scope\" : false,\\n    \"warn_if_variable_defined_but_not_used\" : false,\\n    \"wrap_tab_navigation\" : false\\n}" \
    \nuseDevtools="1" \
    \nuseInternet2="1"' > /home/rhapsody/.rstudio/monitored/user-settings/user-settings \
  && echo 'R_MAX_NUM_DLLS=300' > /home/rhapsody/.Renviron \
  && echo 'alias ll="ls -alF" \
    \nalias la="ls -A" \
    \nalias l="ls -CF \
    \n" \' \
    >> /home/rhapsody/.bashrc \
  && echo '# .bash_profile \
    \n \
    \n# Get the aliases and functions \
    \nif [ -f ~/.bashrc ]; then \
    \n  . ~/.bashrc \
    \nfi \
    \n' > /home/rhapsody/.bash_profile \
  && chown -R rhapsody:rhapsody /home/rhapsody

RUN cd /tmp \
  && curl -O https://cran.r-project.org/src/base/R-3/R-${R_VERSION}.tar.gz \
  && tar -xf R-${R_VERSION}.tar.gz \
  && cd R-${R_VERSION} \
  && R_PAPERSIZE=letter \
    R_BATCHSAVE="--no-save --no-restore" \
    R_BROWSER=xdg-open \
    PAGER=/usr/bin/pager \
    PERL=/usr/bin/perl \
    R_UNZIPCMD=/usr/bin/unzip \
    R_ZIPCMD=/usr/bin/zip \
    R_PRINTCMD=/usr/bin/lpr \
    LIBnn=lib \
    AWK=/usr/bin/awk \
    CFLAGS="-g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g" \
    CXXFLAGS="-g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g" \
  ## Configure options
  ./configure \
    --enable-R-shlib \
    --enable-memory-profiling \
    --with-readline \
    --with-blas \
    --with-tcltk \
    --disable-nls \
    --without-recommended-packages \
  ## Build and install
  && make \
  && make install \
  && mkdir -p /usr/local/lib/R/etc \
  && echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl')" >> /usr/local/lib/R/etc/Rprofile.site \
  && mkdir -p /usr/local/lib/R/site-library \
  && chown root:rhapsody /usr/local/lib/R/site-library \
  && chmod g+wx /usr/local/lib/R/site-library \
  && echo "R_LIBS_USER='/usr/local/lib/R/site-library'" >> /usr/local/lib/R/etc/Renviron \
  && echo "R_LIBS=\${R_LIBS-'/usr/local/lib/R/site-library:/usr/local/lib/R/library:/usr/lib/R/library'}" >> /usr/local/lib/R/etc/Renviron \
  && cd / \
  && rm -rf /tmp/* \
  && apt-get autoremove -y \
  && apt-get autoclean -y \
  && rm -rf /var/lib/apt/lists/*
 
RUN wget -O libssl1.0.0.deb http://ftp.debian.org/debian/pool/main/o/openssl/libssl1.0.0_1.0.1t-1+deb8u8_amd64.deb \
  && dpkg -i libssl1.0.0.deb \
  && rm libssl1.0.0.deb \
  && RSTUDIO_LATEST=$(wget --no-check-certificate -qO- https://s3.amazonaws.com/rstudio-server/current.ver) \
  && [ -z "$RSTUDIO_VERSION" ] && RSTUDIO_VERSION=$RSTUDIO_LATEST || true \
  && wget -q http://download2.rstudio.org/rstudio-server-${RSTUDIO_VERSION}-amd64.deb \
  && dpkg -i rstudio-server-${RSTUDIO_VERSION}-amd64.deb \
  && rm rstudio-server-*-amd64.deb \
  ## Symlink pandoc & standard pandoc templates for use system-wide
  && ln -s /usr/lib/rstudio-server/bin/pandoc/pandoc /usr/local/bin \
  && ln -s /usr/lib/rstudio-server/bin/pandoc/pandoc-citeproc /usr/local/bin \
  && git clone https://github.com/jgm/pandoc-templates \
  && mkdir -p /opt/pandoc/templates \
  && cp -r pandoc-templates*/* /opt/pandoc/templates && rm -rf pandoc-templates* \
  && mkdir /root/.pandoc && ln -s /opt/pandoc/templates /root/.pandoc/templates \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/ \
  ## RStudio wants an /etc/R, will populate from $R_HOME/etc
  && mkdir -p /etc/R \
  ## Write config files in $R_HOME/etc
  && echo '\n\
    \n# Configure httr to perform out-of-band authentication if HTTR_LOCALHOST \
    \n# is not set since a redirect to localhost may not work depending upon \
    \n# where this Docker container is running. \
    \nif(is.na(Sys.getenv("HTTR_LOCALHOST", unset=NA))) { \
    \n  options(httr_oob_default = TRUE) \
    \n}' >> /usr/local/lib/R/etc/Rprofile.site \
  && echo "PATH=${PATH}" >> /usr/local/lib/R/etc/Renviron \
  ## Prevent rstudio from deciding to use /usr/bin/R if a user apt-get installs a package
  && echo 'rsession-which-r=/usr/local/bin/R' >> /etc/rstudio/rserver.conf \
  ## use more robust file locking to avoid errors when using shared volumes:
  && echo 'lock-type=advisory' >> /etc/rstudio/file-locks \
  ## Set up S6 init system
  && wget -P /tmp/ https://github.com/just-containers/s6-overlay/releases/download/v1.11.0.1/s6-overlay-amd64.tar.gz \
  && tar xzf /tmp/s6-overlay-amd64.tar.gz -C / \
  && mkdir -p /etc/services.d/rstudio \
  && echo '#!/usr/bin/with-contenv bash \
          \n exec /usr/lib/rstudio-server/bin/rserver --server-daemonize 0' \
          > /etc/services.d/rstudio/run \
  && echo '#!/bin/bash \
          \n rstudio-server stop' \
          > /etc/services.d/rstudio/finish \
  && apt-get autoremove -y \
  && apt-get autoclean -y \
  && rm -rf /var/lib/apt/lists/*
  
RUN addgroup rstudio-server rhapsody && usermod -g rhapsody rstudio-server

RUN echo '#!/bin/bash \
  \nmkdir -p /var/run/sshd \
  \n/usr/sbin/sshd \
  \nrstudio-server start \
  \n \
  \ngit -C /home/rhapsody/WP3/scripts pull origin master\
  \n \
  \ntail -f /dev/null \
  \n' >> /tmp/boot.sh \
  && chmod 755 /tmp/boot.sh

RUN cd /home/rhapsody && ln -s /media media

COPY login.html /etc/rstudio/login.html

COPY logo.png  /usr/lib/rstudio-server/www/images/logo.png

RUN echo 'auth-login-page-html=/etc/rstudio/login.html' >> /etc/rstudio/rserver.conf


COPY r_packages.R /tmp/r_packages.R
RUN chmod 777 /tmp/r_packages.R \
  && Rscript /tmp/r_packages.R \
  && rm -rf /tmp/downloaded_packages/ /tmp/*.rds \
  && rm -rf /var/lib/apt/lists/ \
  && apt-get clean \
  && apt-get autoremove -y
  

RUN mkdir -p /home/rhapsody/WP3 \
  && chmod 777 -R /home/rhapsody/WP3 \
  && cd /home/rhapsody/WP3 \
  && git clone https://github.com/vcftools/vcftools.git \
  && cd vcftools \
  && ./autogen.sh \
  && ./configure \
  && make \
  && make install \
  && cd /home/rhapsody/WP3 \
  && rm -rf vcftools \
  && git clone https://gist.github.com/15362a96c1561bb51af98760b41c478e.git scripts \
  && cd scripts \
  && git clone https://gist.github.com/f3e2fdc59757fd8577abfe233854580a.git utils \
  && chown -R rhapsody:rhapsody /home/rhapsody \
  && chown -R rhapsody:rhapsody /home/rhapsody/scripts/.git \
  && chown -R rhapsody:rhapsody /home/rhapsody/scripts/utils/.git

  
EXPOSE 8787


CMD ["/bin/sh", "/tmp/boot.sh"]
