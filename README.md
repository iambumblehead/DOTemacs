 ```elisp
 (load-file "~/path/to/DOTemacs/DOTemacs.el")
```

![emacs](https://github.com/iambumblehead/DOTemacs/raw/master/emacs-nw.gif)

```bash
git clone --depth=1 git://git.savannah.gnu.org/emacs.git
cd emacs
sudo apt build-dep emacs24
./autogen.sh
./autogen.sh git
./configure
make && sudo make install
make clean
```
