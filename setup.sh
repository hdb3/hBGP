# basic start from clean host script to build hBGP
#
# install build tools
sudo apt -y -y install build-essential curl libffi-dev libffi8 libgmp-dev libgmp10 libncurses-dev pkg-config
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | BOOTSTRAP_HASKELL_NONINTERACTIVE=y sh
#
# hBGP specifics
sudo apt -y -y install zlib1g-dev
./make
