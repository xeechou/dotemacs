# My Vanilla yet powerful Emacs environment

For years I have been perfecting my Emacs configuration, it is now the
essential part of my digital life. This is my personal configuration but you
may also find it useful to your needs. Feel free to grab some of my config
snippets to work for you.

Here is a list of incomplete features you get from this config set.
- Clean configuration with [use-package](https://github.com/jwiegley/use-package)
- Some programming language support.
  + C/C++
  + Python
  + typescript.
  + golang.
  + glsl.
  + many others.
- Some of my collected code snippets for different languages.
- Code auto-complete support via [lsp-mode](https://emacs-lsp.github.io/lsp-mode/)
  or [Company-mode](https://company-mode.github.io/).
- Completion framework using [Ivy](https://github.com/abo-abo/swiper).
- keybinding look-up with
  [which-key](https://github.com/justbur/emacs-which-key).
- Flyspell support via Hunspell.
- Org mode configurations.
  - org-roam v2.
  - org file synchronization
    [script](https://github.com/xeechou/dotemacs/blob/master/lisp/org-msync.el)
  - org-babel code evaluation support for python, C/C++.
- Theme and font configuration
  - **ligature** support if follow fonts available in your system.
	- fira-code
	- cascadia-code
	- losevka
	- jetbrain
	- monolisa.
  - auto configuring main fonts, CKJ fonts and Symbol fonts
  - ample theme.

I am a C++ programmer 90 percent of the time, so this environment is heavily
tailored for C++ than other languages. I use **clangd** as the lsp server for C
and C++, you can also use other back-ends like
[ccls](https://github.com/MaskRay/ccls).

## How to use this configuration system.

To start, you need to run `package-refresh-content` once. The official ELPA
repositories are not direct accessible in China so you can uncomment the
mirrors to use those instead. 

Then run the `load-file` command to run the `init.el` again, Emacs will load
and compile all the packages, you can lean back for it to finish.

## Run-time requirement for some packages.

Some of the packages requires not only Emacs-Lisp code but also other binaries to
work. Here is the list of binaries required for all the features.

- Clang installation for LSP back-end.
- Compiler or Interpreter for the targeting programming languages.
- SQLite3 for org-roam.
- Latex installation for `org-latex-preview`
- [ripgrep](https://github.com/BurntSushi/ripgrep) for refactoring support.
- Hunspell installation for flyspell.

Note that none of those are hard requirements, you will simply lose some
features if you don't have them.

### Binaries for Windows
On Linux, those run-time requires can be easily satisfied with package managers
like `apt-get` or `dnf install`. On windows, it's another story, for this
purpose, I maintain a
[dotemacs-msbin](https://github.com/xeechou/dotemacs-msbin) for those
dependencies on Windows.

For Mac users, I am not sure, maybe you can do it with `brew install`?
