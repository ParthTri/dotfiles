export NNN_FIFO=/tmp/nnn.fifo
export NNN_PLUG="f:fzcd;o:fzopen;z:fzz;d:diffs;t:treeview;v:preview-tui"

export BLK="04" 
export CHR="04" 
export DIR="04" 
export EXE="00" 
export REG="00" 
export HARDLINK="00" 
export SYMLINK="06" 
export MISSING="00" 
export ORPHAN="01" 
export FIFO="0F" 
export LOCK="0F" 
export OTHER="02"
export NNN_FCOLORS="$BLK$CHR$DIR$EXE$REG$HARDLINK$SYMLINK$MISSING$ORPHAN$FIFO$SOCK$OTHER"