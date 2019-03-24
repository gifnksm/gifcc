FROM archlinux/base AS build-env
RUN pacman -Syu --noconfirm
RUN pacman -S base-devel --noconfirm
RUN echo "Defaults        lecture = never" > /etc/sudoers.d/privacy
RUN echo "%wheel ALL=(ALL) NOPASSWD: ALL" > /etc/sudoers.d/wheel
RUN useradd -m -G wheel -s /bin/bash builder
USER builder
WORKDIR  /home/builder
COPY \
        --chown=builder \
        nodejs-tap-xunit/PKGBUILD /home/builder/nodejs-tap-xunit/
RUN cd /home/builder/nodejs-tap-xunit && sudo -u builder makepkg -s --noconfirm
ADD \
        --chown=builder \
        https://aur.archlinux.org/cgit/aur.git/snapshot/tmsu.tar.gz \
        /home/builder/
RUN tar xvf tmsu.tar.gz
RUN cd /home/builder/tmsu && sudo -u builder makepkg -s --noconfirm
ADD \
        --chown=builder \
        https://aur.archlinux.org/cgit/aur.git/snapshot/lcov.tar.gz \
        /home/builder/
RUN tar xvf lcov.tar.gz
RUN cd /home/builder/lcov && sudo -u builder makepkg -s --noconfirm

FROM archlinux/base
COPY \
        --from=build-env \
        /home/builder/*/*.tar.xz \
        ./
RUN \
        set -x && \
        pacman -Syu --noconfirm && \
        pacman -S base-devel npm python3 git clang --noconfirm && \
        pacman -U *.tar.xz --noconfirm && \
        rm *.tar.xz && \
        pacman -Scc --noconfirm
