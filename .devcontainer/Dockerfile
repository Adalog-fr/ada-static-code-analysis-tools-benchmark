FROM gnatpro-asis:24.0

# ENV OS=unix

# Dependencies for compiling Ada files
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
    zsh sudo

# Install Oh My Posh for a better prompt
RUN curl -s https://ohmyposh.dev/install.sh | bash -s

ARG USERNAME=devy
ARG USER_UID=1000
ARG USER_GID=$USER_UID

# Create the user
RUN groupadd --gid $USER_GID $USERNAME \
    && useradd --uid $USER_UID --gid $USER_GID -m --no-log-init -G sudo -s /usr/bin/zsh -r $USERNAME \
    && echo $USERNAME ALL=\(root\) NOPASSWD:ALL > /etc/sudoers.d/$USERNAME \
    && chmod 0440 /etc/sudoers.d/$USERNAME

# Set the default user
USER $USERNAME

# Install tokei to count lines of codes
RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
RUN $HOME/.cargo/bin/cargo install --git https://github.com/XAMPPRocky/tokei.git tokei

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
