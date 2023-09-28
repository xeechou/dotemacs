;; collections of c styles for matching editing experience with clang-format
;; Open an cpp file in an existing project. Run ~M-x c-guess-no-install~, then
;; use ~M-x c-guess-view~ and paste the result here

;; later in the project you can have a dir-locals.el with

;; ( (c++-mode  . ((tab-width . 4)
;;	      (c-basic-offset . 4)
;;	      (indent-tabs-mode . nil)
;;	      (fill-column . 140)
;;	      (c-file-style . "O3DE")))
;;   )

(require 'cc-styles)

(c-add-style "O3DE"
	     '("linux"
	       (c-basic-offset . 4)	; Guessed value
	       (c-offsets-alist
		(access-label . 0)	; Guessed value
		(arglist-cont . 0)	; Guessed value
		(arglist-intro . +)	; Guessed value
		(class-close . 0)	; Guessed value
		(class-open . 0)	; Guessed value
		(inclass . +)		; Guessed value
		(inher-intro . +)	; Guessed value
		(innamespace . +)	; Guessed value
		(namespace-close . 0)	; Guessed value
		(namespace-open . 0)	; Guessed value
		(topmost-intro . 0)	; Guessed value
		(annotation-top-cont . 0)
		(annotation-var-cont . +)
		(arglist-close . c-lineup-close-paren)
		(arglist-cont-nonempty . c-lineup-arglist)
		(block-close . 0)
		(block-open . 0)
		(brace-entry-open . 0)
		(brace-list-close . 0)
		(brace-list-entry . 0)
		(brace-list-intro . +)
		(brace-list-open . 0)
		(c . c-lineup-C-comments)
		(case-label . 0)
		(catch-clause . 0)
		(comment-intro . c-lineup-comment)
		(composition-close . 0)
		(composition-open . 0)
		(cpp-define-intro c-lineup-cpp-define +)
		(cpp-macro . -1000)
		(cpp-macro-cont . +)
		(defun-block-intro . +)
		(defun-close . 0)
		(defun-open . 0)
		(do-while-closure . 0)
		(else-clause . 0)
		(extern-lang-close . 0)
		(extern-lang-open . 0)
		(friend . 0)
		(func-decl-cont . +)
		(incomposition . +)
		(inexpr-class . +)
		(inexpr-statement . +)
		(inextern-lang . 0)
		(inher-cont . c-lineup-multi-inher)
		(inlambda . 0)
		(inline-close . 0)
		(inline-open . 0)
		(inmodule . +)
		(knr-argdecl . 0)
		(knr-argdecl-intro . 0)
		(label . 0)
		(lambda-intro-cont . +)
		(member-init-cont . c-lineup-multi-inher)
		(member-init-intro . +)
		(module-close . 0)
		(module-open . 0)
		(objc-method-args-cont . c-lineup-ObjC-method-args)
		(objc-method-call-cont c-lineup-ObjC-method-call-colons c-lineup-ObjC-method-call +)
		(objc-method-intro .
				   [0])
		(statement . 0)
		(statement-block-intro . +)
		(statement-case-intro . +)
		(statement-case-open . 0)
		(statement-cont . +)
		(stream-op . c-lineup-streamop)
		(string . -1000)
		(substatement . +)
		(substatement-label . 0)
		(substatement-open . 0)
		(template-args-cont c-lineup-template-args +)
		(topmost-intro-cont . c-lineup-topmost-intro-cont))))


(c-add-style "sparroh"
	     '("linux"
	       (c-basic-offset . 4)	; Guessed value
	       (c-offsets-alist
		(arglist-intro . +)	; Guessed value
		(block-close . 0)	; Guessed value
		(brace-list-close . 0)	; Guessed value
		(brace-list-entry . 0)	; Guessed value
		(brace-list-intro . +)	; Guessed value
		(defun-block-intro . +)	; Guessed value
		(defun-close . 0)	; Guessed value
		(defun-open . 0)	; Guessed value
		(else-clause . 0)	; Guessed value
		(innamespace . 0)	; Guessed value
		(namespace-close . 0)	; Guessed value
		(namespace-open . 0)	; Guessed value
		(statement . 0)		; Guessed value
		(statement-block-intro . +) ; Guessed value
		(statement-cont . 0)	; Guessed value
		(substatement-open . 0)	; Guessed value
		(topmost-intro . 0)	; Guessed value
		(topmost-intro-cont . 0) ; Guessed value
		(access-label . -)
		(annotation-top-cont . 0)
		(annotation-var-cont . +)
		(arglist-close . c-lineup-close-paren)
		(arglist-cont c-lineup-gcc-asm-reg 0)
		(arglist-cont-nonempty . c-lineup-arglist)
		(block-open . 0)
		(brace-entry-open . 0)
		(brace-list-open . 0)
		(c . c-lineup-C-comments)
		(case-label . 0)
		(catch-clause . 0)
		(class-close . 0)
		(class-open . 0)
		(comment-intro . c-lineup-comment)
		(composition-close . 0)
		(composition-open . 0)
		(cpp-define-intro c-lineup-cpp-define +)
		(cpp-macro . -1000)
		(cpp-macro-cont . +)
		(do-while-closure . 0)
		(extern-lang-close . 0)
		(extern-lang-open . 0)
		(friend . 0)
		(func-decl-cont . +)
		(inclass . +)
		(incomposition . +)
		(inexpr-class . +)
		(inexpr-statement . +)
		(inextern-lang . 0)
		(inher-cont . c-lineup-multi-inher)
		(inher-intro . +)
		(inlambda . 0)
		(inline-close . 0)
		(inline-open . 0)
		(inmodule . +)
		(knr-argdecl . 0)
		(knr-argdecl-intro . 0)
		(label . 0)
		(lambda-intro-cont . +)
		(member-init-cont . c-lineup-multi-inher)
		(member-init-intro . +)
		(module-close . 0)
		(module-open . 0)
		(objc-method-args-cont . c-lineup-ObjC-method-args)
		(objc-method-call-cont c-lineup-ObjC-method-call-colons c-lineup-ObjC-method-call +)
		(objc-method-intro .
				   [0])
		(statement-case-intro . +)
		(statement-case-open . 0)
		(stream-op . c-lineup-streamop)
		(string . -1000)
		(substatement . +)
		(substatement-label . 0)
		(template-args-cont c-lineup-template-args +))))


;; Ref: https://stackoverflow.com/questions/39894233/extract-emacs-c-style-options-from-clang-format-style
