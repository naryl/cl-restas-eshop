ESHOP_PATH="$HOME/cl-restas-eshop/"
CONFIG_PATH=$ESHOP_PATH"example-start/config-dev.conf"
LIBS_PATH="$HOME/eshop/libs/"
SWANK_PORT=4005

export ESHOP_PATH
export CONFIG_PATH
export LIBS_PATH
export SWANK_PORT

sbcl --lose-on-corruption --disable-ldb --dynamic-space-size 1024 --load $ESHOP_PATH"start-eshop.lisp"
