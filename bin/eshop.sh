ESHOP_PATH="$HOME/cl-restas-eshop/"
CONFIG_PATH=$ESHOP_PATH"config/default.conf"
LIBS_PATH="$HOME/cl-libs/"
SWANK_PORT=4005

export ESHOP_PATH
export CONFIG_PATH
export LIBS_PATH
export SWANK_PORT

screen -dmS eshop sbcl --lose-on-corruption --disable-ldb --dynamic-space-size 2048 --load $ESHOP_PATH"start-eshop.lisp"