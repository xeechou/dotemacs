(when (executable-find "ollama")
  (use-package ellama :ensure t
    :init
    ;; setup key bindings
    (setopt ellama-keymap-prefix "C-c e")
    ;; we only use the default model "zephyr:latest"
    ;; TODO : adding new models
    ;; language you want ellama to translate to
    (setopt ellama-language "French")))
