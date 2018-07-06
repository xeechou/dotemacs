# My personal emacs configurations

My emacs working environment, majorly for my daily working routine
- C/C++
- Python
- elisp
- org mode
- and others.

To start, you need to run `package-refresh-content` once. I comment out the
command because it is too long for a sytemd job.

I installed many packages, now I use use-package to manage all my packages. Some
of the projects require external program to work.

- Irony requires cmake and libclang, you should run `irony-install-server` at any
  c/cpp file.
- Python need to install `jedi server`, run `jedi:install-server` before using
- ggtags requires gtags to work, you could install based on your distribution.



### Issues happend
- when I put some command hook in `:init` section with `use-package`, somehow I
  broke the `match-string` function.
- loading `irony-mode` only at `c` and `c++`, because **php** in emacs is also a
  cc-mode.
- `rdm-mode` is really buggy.


### switch to lsp-mode + ccls-emacs
required `ccls` package, for archlinux, go for ccls-git, for `ubuntu` and other
distros, compile and and move to `$PATH`.
