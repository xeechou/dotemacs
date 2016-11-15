#My personal emacs configurations

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
- ggtags requires gtags to work, you could install based on your distribution.
