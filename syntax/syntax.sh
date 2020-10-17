echo "Installing BruSKI for vim..."

if diff bru.vim ~/.vim/ftdetect/bru.vim > /dev/null 
then
        echo "Latest ftdetect already installed, skipping..."
else
        install -Dm644 "bru.vim" "$HOME/.vim/ftdetect/bru.vim"
fi

if diff BruSKI.vim ~/.vim/syntax/BruSKI.vim > /dev/null
then
        echo "Latest syntax file already installed, skipping..."
else
        install -Dm644 "BruSKI.vim" "$HOME/.vim/syntax/BruSKI.vim"
fi

if grep -Eq "inoremap.+λ" "$HOME/.vimrc"
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
