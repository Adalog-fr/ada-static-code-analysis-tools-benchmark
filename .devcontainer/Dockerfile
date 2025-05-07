FROM gnatpro-asis:24.0

# ENV OS=unix

# Dependencies for compiling Ada files
RUN apt update
RUN apt-get install -y build-essential libssl-dev \
    unzip liblzma-dev libcurl4-openssl-dev \
    default-libmysqlclient-dev \
    mariadb-client libmariadbclient-dev-compat \
    sqlite3 libsqlite3-dev \
    postgresql-client libpq-dev \
    libgtk-3-dev \
    libfuse-dev \
    lua5.4 liblua5.4-dev liblua5.3-dev \
    liblapack3 libblas3 libblas-dev \
    libbullet-dev libfreetype6-dev libexpat1-dev \
    libcanberra-pulse \
    libfbclient2 \
    libcsfml-dev \
    libglfw3-dev \
    libgmp-dev \
    libgsl-dev \
    libhidapi-dev \
    libpython3-dev \
    librtmidi-dev \
    libsdl2-dev libsdl2-image-dev libsdl2-ttf-dev \
    libzmq5 libzmq3-dev \
    libx11-dev \
    re2c \
    unixodbc-dev \
    zlib1g-dev \
    libgnutls28-dev liblapack-dev \
    libmpfr-dev libcanberra-dev libusb-1.0-0-dev \
    # Install zsh and Oh-My-Posh for a better shell experience
    zsh sudo \
    # Install jc to transform output of time in JSON format
    jc

# Install Neo4J: cypher-shell

RUN wget --no-check-certificate https://dist.neo4j.org/cypher-shell/cypher-shell_5.23.0_all.deb && \
    sudo apt install -y ./cypher-shell_5.23.0_all.deb && \
    rm cypher-shell_5.23.0_all.deb

# Create linuxbrew user and group
RUN groupadd -f --gid 1001 linuxbrew && \
    useradd -m --uid 1001 --gid 1001 -s /bin/bash linuxbrew && \
    usermod -aG sudo linuxbrew &&  \
    mkdir -p /home/linuxbrew/.linuxbrew && \
    chown -R linuxbrew: /home/linuxbrew/.linuxbrew && \
    echo 'linuxbrew ALL=(ALL) NOPASSWD:ALL' >>/etc/sudoers

# RUN localedef -i en_US -f UTF-8 en_US.UTF-8

USER linuxbrew
RUN sh -c "$(curl -fsSL https://raw.githubusercontent.com/Linuxbrew/install/master/install.sh)"

USER root

# Create the user
ARG USERNAME=devy
ARG USER_UID=1000
ARG USER_GID=$USER_UID

# Create the group and user
RUN groupadd -f --gid $USER_GID $USERNAME \
    && useradd --uid $USER_UID --gid $USER_GID -m --no-log-init -G sudo -s /usr/bin/zsh -r $USERNAME \
    && echo $USERNAME ALL=\(root\) NOPASSWD:ALL >> /etc/sudoers
USER $USERNAME

# Set up Homebrew environment variables
ENV PATH="/home/linuxbrew/.linuxbrew/bin:${PATH}"
ENV HOMEBREW_PREFIX="/home/linuxbrew/.linuxbrew"
ENV HOMEBREW_CELLAR="/home/linuxbrew/.linuxbrew/Cellar"
ENV HOMEBREW_REPOSITORY="/home/linuxbrew/.linuxbrew/Homebrew"
ENV MANPATH="/home/linuxbrew/.linuxbrew/share/man:${MANPATH}"
ENV INFOPATH="/home/linuxbrew/.linuxbrew/share/info:${INFOPATH}"

# Add devy user to linuxbrew group
USER root
RUN usermod -aG linuxbrew $USERNAME && \
    chown -R devy /home/linuxbrew/.linuxbrew

USER $USERNAME

# Install Oh My Posh for a better prompt
RUN curl -s https://ohmyposh.dev/install.sh | bash -s

# Install SCC to count lines of codes
RUN brew install scc

# Install sed version 4.9 to prevent `sed -i` issues
RUN cd /tmp \
    && wget ftp://ftp.gnu.org/gnu/sed/sed-4.9.tar.xz \
    && tar xf sed-4.9.tar.xz \
    && rm sed-4.9.tar.xz \
    && cd sed-4.9 \
    && ./configure \
    && make \
    && sudo make install \
    && cd ..\
    && rm -rf sed-4.9

ENTRYPOINT ["/bin/zsh"]
