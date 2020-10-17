# Installing BruSKI for vim

Run the install script `syntax.sh` and follow the prompts. 

```
$ ./syntax.sh
```

## What is a λ-macro?
BruSKI uses a unicode lambda symbol (\955) as part of its syntax. It is therefore advantageous to have a nice way of writing that character. During the install, the script will ask you for a macro character, this character will be used in a vim mapping replacing it with a λ-symbol. If you choose the standard `l`, the macro will be:

```
\l --> λ
```

If you type `\l` vim will now replace it with a λ.
