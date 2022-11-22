FROM gitpod/workspace-base

# Install dependencies
RUN sudo apt-get install -y build-essential curl libffi-dev libffi7 libgmp-dev libgmp10 libncurses-dev libncurses5 libtinfo5

RUN mkdir -p "$HOME/.ghcup/bin" \
    && curl -LJ "https://downloads.haskell.org/~ghcup/x86_64-linux-ghcup" -o "$HOME/.ghcup/bin/ghcup" \
    && chmod +x "$HOME/.ghcup/bin/ghcup"
ENV PATH="/home/$USERNAME/.cabal/bin:/home/$USERNAME/.ghcup/bin:$PATH"

# Set up the environment. This will install the default versions of every tool.
RUN ghcup install ghc 8.10.7 --set 
RUN ghcup install stack recommended --set
RUN ghcup install cabal recommended --set
RUN ghcup install hls recommended --set
RUN cabal update

# change stack's configuration to use system installed ghc.
# By default, stack tool will download its own version of the compiler,
# Setting up this configuration will avoid downloading haskell compiler twice.
# WARNING! Maybe this is not adecuate for your project! use your project wise stack.yaml to change this
RUN stack config set install-ghc --global false
RUN stack config set system-ghc --global true 
