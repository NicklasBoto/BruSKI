FTFILE="$HOME/.vim/ftdetect/bru.vim"
SXFILE="$HOME/.vim/syntax/BruSKI.vim"

if [ -f "$FTFILE" ]
then
        if diff bru.vim $FTFILE > /dev/null
        then
                echo "Latest ftdetect already installed, skipping..."
        else
                echo "Updating ftdetect file..."
                install -Dm644 "bru.vim" $FTFILE
        fi
else
        echo "Creating ftdetect file..."
        install -Dm644 "bru.vim" $FTFILE
fi

if [ -f "$SXFILE" ]
then
        if diff BruSKI.vim $SXFILE > /dev/null
        then
                echo "Latest syntax file already installed, skipping..."
        else
                echo "Updating syntax file..."
                install -Dm644 "BruSKI.vim" $SXFILE
        fi
else
        echo "Creating syntax file..."
        install -Dm644 "BruSKI.vim" $SXFILE
fi

if grep -Eqs "inoremap.+λ" "$HOME/.vimrc"
then
        echo "Lambda macro already exists, skipping..."
else
        
        read -p "Enter a keyword for your lambda macro (default is 'l'): " keyword
        
        echo '" BruSKI insert maps' >> ~/.vimrc
        
        if [ -z "$keyword" ]
        then
                echo "inoremap \l λ" >> ~/.vimrc
        else
                echo "inoremap \\$keyword λ" >> ~/.vimrc
        fi
fi

echo "Done!"
